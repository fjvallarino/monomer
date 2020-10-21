{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Handlers (
  HandlerStep,
  handleWidgetResult,
  handleSystemEvents,
  handleResourcesInit,
  handleWidgetInit
) where

import Control.Concurrent.Async (async)
import Control.Lens ((.=), at, use)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan)
import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.IO.Class
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq(..), (><), (|>))

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified SDL
import qualified SDL.Raw.Enum as SDLE
import qualified SDL.Raw.Event as SDLE
import qualified SDL.Raw.Types as SDL

import Monomer.Core
import Monomer.Event
import Monomer.Main.Types
import Monomer.Main.Util

import qualified Monomer.Main.Lens as L

type HandlerStep s e = (WidgetEnv s e, Seq e, WidgetInstance s e)

getTargetPath
  :: WidgetEnv s e
  -> Maybe Path
  -> Maybe Path
  -> Path
  -> SystemEvent
  -> WidgetInstance s e
  -> Maybe Path
getTargetPath wenv pressed overlay target event widgetRoot = case event of
    -- Keyboard
    KeyAction{}            -> pathEvent target
    TextInput _            -> pathEvent target
    -- Clipboard
    Clipboard _            -> pathEvent target
    -- Mouse/touch
    ButtonAction point _ _ -> pointEvent point
    Click point _          -> pointEvent point
    WheelScroll point _ _  -> pointEvent point
    Focus                  -> pathEvent target
    Blur                   -> pathEvent target
    Enter newPath _        -> pathEvent newPath
    Move point             -> pointEvent point
    Leave oldPath _        -> pathEvent oldPath
  where
    widget = _wiWidget widgetRoot
    startPath = fromMaybe rootPath overlay
    pathEvent = Just
    pathFromPoint p = widgetFindByPoint widget wenv startPath p widgetRoot
    pointEvent point = pressed <|> pathFromPoint point <|> overlay

handleSystemEvents
  :: (MonomerM s m)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetInstance s e
  -> m (HandlerStep s e)
handleSystemEvents wenv systemEvents widgetRoot = nextStep where
  reducer (cWctx, cEvents, cRoot) evt = do
    focused <- use L.pathFocus

    (wenv2, evts2, wroot2) <- handleSystemEvent cWctx evt focused cRoot
    return (wenv2, cEvents >< evts2, wroot2)
  nextStep = foldM reducer (wenv, Seq.empty, widgetRoot) systemEvents

handleSystemEvent
  :: (MonomerM s m)
  => WidgetEnv s e
  -> SystemEvent
  -> Path
  -> WidgetInstance s e
  -> m (HandlerStep s e)
handleSystemEvent wenv event currentTarget widgetRoot = do
  pressed <- use L.pathPressed
  overlay <- use L.pathOverlay

  case getTargetPath wenv pressed overlay currentTarget event widgetRoot of
    Nothing -> return (wenv, Seq.empty, widgetRoot)
    Just target -> do
      let widget = _wiWidget widgetRoot
      let emptyResult = WidgetResult Seq.empty Seq.empty widgetRoot
      let evtResult = widgetHandleEvent widget wenv target event widgetRoot
      let widgetResult = fromMaybe emptyResult evtResult
      let reqs = _wrRequests widgetResult
      let stopProcessing = isJust $ Seq.findIndexL isIgnoreParentEvents reqs

      handleWidgetResult wenv widgetResult
        >>= handleFocusChange event stopProcessing

handleResourcesInit :: MonomerM s m => m ()
handleResourcesInit = do
  cursors <- foldM insert Map.empty [toEnum 0 ..]
  L.cursorIcons .= cursors
  where
    insert map icon = do
      cursor <- SDLE.createSystemCursor (cursorToSDL icon)
      return $ Map.insert icon cursor map

handleWidgetInit
  :: (MonomerM s m)
  => WidgetEnv s e
  -> WidgetInstance s e
  -> m (HandlerStep s e)
handleWidgetInit wenv widgetRoot = do
  let widget = _wiWidget widgetRoot
  let widgetResult = widgetInit widget wenv widgetRoot

  handleWidgetResult wenv widgetResult

handleWidgetResult
  :: (MonomerM s m)
  => WidgetEnv s e
  -> WidgetResult s e
  -> m (HandlerStep s e)
handleWidgetResult wenv (WidgetResult reqs events evtRoot) = do
  let evtUpdates = getUpdateModelReqs reqs
  let evtModel = foldr (.) id evtUpdates (_weModel wenv)
  let evtWctx = wenv { _weModel = evtModel }

  handleNewWidgetTasks reqs

  handleFocusSet reqs (evtWctx, events, evtRoot)
    >>= handleClipboardGet reqs
    >>= handleClipboardSet reqs
    >>= handleStartTextInput reqs
    >>= handleStopTextInput reqs
    >>= handleSendMessages reqs
    >>= handleOverlaySet reqs
    >>= handleOverlayReset reqs
    >>= handleCursorIconSet reqs
    >>= handleResize reqs

handleFocusChange
  :: (MonomerM s m)
  => SystemEvent
  -> Bool
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleFocusChange systemEvent stopProcessing (wenv, events, widgetRoot)
  | focusChangeRequested = do
      oldFocus <- use L.pathFocus
      (wenv1, events1, root1) <- handleSystemEvent wenv Blur oldFocus widgetRoot

      let newFocus = findNextFocus wenv1 focusDirection oldFocus root1
      let tempWenv = wenv1 {
        _weFocusedPath = newFocus
      }

      L.pathFocus .= newFocus
      (wenv2, events2, root2) <- handleSystemEvent tempWenv Focus newFocus root1

      return (wenv2, events >< events1 >< events2, root2)
  | otherwise = return (wenv, events, widgetRoot)
  where
    focusChangeRequested = not stopProcessing && isKeyPressed systemEvent keyTab
    focusDirection
      | isShiftPressed systemEvent = FocusBwd
      | otherwise = FocusFwd

handleFocusSet
  :: (MonomerM s m)
  => Seq (WidgetRequest s)
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleFocusSet reqs previousStep@(wenv, events, root) =
  case Seq.filter isSetFocus reqs of
    SetFocus newFocus :<| _ -> do
      L.pathFocus .= newFocus
      (wenv2, events2, root2) <- handleSystemEvent wenv Focus newFocus root

      return (wenv2, events >< events2, root2)
    _ -> return previousStep

handleResize
  :: (MonomerM s m)
  => Seq (WidgetRequest s)
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleResize reqs previousStep =
  case Seq.filter isResize reqs of
    Resize :<| _ -> do
      windowSize <- use L.windowSize

      let (wenv, events, widgetRoot) = previousStep
      let newWidgetRoot = resizeWidget wenv windowSize widgetRoot

      return (wenv, events, newWidgetRoot)
    _ -> return previousStep

handleClipboardGet
  :: (MonomerM s m)
  => Seq (WidgetRequest s)
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleClipboardGet reqs previousStep = do
    hasText <- SDL.hasClipboardText
    contents <- if hasText
                  then fmap ClipboardText SDL.getClipboardText
                  else return ClipboardEmpty

    foldM (reducer contents) previousStep reqs
  where
    reducer contents (wenv, events, widgetRoot) (GetClipboard path) = do
      (newWenv2, newEvents2, newRoot2)
        <- handleSystemEvent wenv (Clipboard contents) path widgetRoot

      return (newWenv2, events >< newEvents2, newRoot2)
    reducer contents prevStep _ = return prevStep

handleClipboardSet
  :: (MonomerM s m)
  => Seq (WidgetRequest s)
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleClipboardSet reqs previousStep =
  case Seq.filter isSetClipboard reqs of
    SetClipboard (ClipboardText text) :<| _ -> do
      SDL.setClipboardText text

      return previousStep
    _ -> return previousStep

handleStartTextInput
  :: (MonomerM s m)
  => Seq (WidgetRequest s)
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleStartTextInput reqs previousStep =
  case Seq.filter isSetOverlay reqs of
    StartTextInput (Rect x y w h) :<| _ -> do
      SDL.startTextInput (SDL.Rect (c x) (c y) (c w) (c h))

      return previousStep
    _ -> return previousStep
  where
    c x = fromIntegral $ round x

handleStopTextInput
  :: (MonomerM s m)
  => Seq (WidgetRequest s)
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleStopTextInput reqs previousStep =
  case Seq.filter isSetOverlay reqs of
    StopTextInput :<| _ -> do
      SDL.stopTextInput

      return previousStep
    _ -> return previousStep

handleOverlaySet
  :: (MonomerM s m)
  => Seq (WidgetRequest s)
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleOverlaySet reqs previousStep =
  case Seq.filter isSetOverlay reqs of
    SetOverlay path :<| _ -> do
      L.pathOverlay .= Just path

      return previousStep
    _ -> return previousStep

handleOverlayReset
  :: (MonomerM s m)
  => Seq (WidgetRequest s)
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleOverlayReset reqs previousStep =
  case Seq.filter isResetOverlay reqs of
    ResetOverlay :<| _ -> do
      L.pathOverlay .= Nothing

      return previousStep
    _ -> return previousStep

handleCursorIconSet
  :: (MonomerM s m)
  => Seq (WidgetRequest s)
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleCursorIconSet reqs previousStep =
  case Seq.filter isSetCursorIcon reqs of
    SetCursorIcon icon :<| _ -> do
      cursor <- (Map.! icon) <$> use L.cursorIcons
      SDLE.setCursor cursor

      return previousStep
    _ -> return previousStep

cursorToSDL :: CursorIcon -> SDLE.SystemCursor
cursorToSDL CursorArrow = SDLE.SDL_SYSTEM_CURSOR_ARROW
cursorToSDL CursorHand = SDLE.SDL_SYSTEM_CURSOR_HAND
cursorToSDL CursorIBeam = SDLE.SDL_SYSTEM_CURSOR_IBEAM
cursorToSDL CursorInvalid = SDLE.SDL_SYSTEM_CURSOR_NO
cursorToSDL CursorSizeH = SDLE.SDL_SYSTEM_CURSOR_SIZEWE
cursorToSDL CursorSizeV = SDLE.SDL_SYSTEM_CURSOR_SIZENS
cursorToSDL CursorDiagTL = SDLE.SDL_SYSTEM_CURSOR_SIZENWSE
cursorToSDL CursorDiagTR = SDLE.SDL_SYSTEM_CURSOR_SIZENESW

handleSendMessages
  :: (MonomerM s m)
  => Seq (WidgetRequest s)
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleSendMessages reqs previousStep = nextStep where
  nextStep = foldM reducer previousStep reqs
  reducer prevStep (SendMessage path message) = do
    currentFocus <- use L.pathFocus

    let (wenv, events, widgetRoot) = prevStep
    let emptyResult = WidgetResult Seq.empty Seq.empty widgetRoot
    let widget = _wiWidget widgetRoot
    let msgResult = widgetHandleMessage widget wenv path message widgetRoot
    let widgetResult = fromMaybe emptyResult msgResult

    (newWenv, newEvents, newWidgetRoot)
      <- handleWidgetResult wenv widgetResult

    return (newWenv, events >< newEvents, newWidgetRoot)
  reducer prevStep _ = return prevStep

handleNewWidgetTasks :: (MonomerM s m) => Seq (WidgetRequest s) -> m ()
handleNewWidgetTasks reqs = do
  let taskHandlers = Seq.filter isTaskHandler reqs
  let producerHandlers = Seq.filter isProducerHandler reqs

  singleTasks <- forM taskHandlers $ \(RunTask path handler) -> do
    asyncTask <- liftIO $ async (liftIO handler)
    return $ WidgetTask path asyncTask

  producerTasks <- forM producerHandlers $ \(RunProducer path handler) -> do
    newChannel <- liftIO newTChanIO
    asyncTask <- liftIO $ async (liftIO $ handler (sendMessage newChannel))
    return $ WidgetProducer path newChannel asyncTask

  previousTasks <- use L.widgetTasks
  L.widgetTasks .= previousTasks >< singleTasks >< producerTasks

sendMessage :: TChan e -> e -> IO ()
sendMessage channel message = atomically $ writeTChan channel message

isIgnoreParentEvents :: WidgetRequest s -> Bool
isIgnoreParentEvents IgnoreParentEvents = True
isIgnoreParentEvents _ = False

isResize :: WidgetRequest s -> Bool
isResize Resize = True
isResize _ = False

isSetClipboard :: WidgetRequest s -> Bool
isSetClipboard SetClipboard{} = True
isSetClipboard _ = False

isSetOverlay :: WidgetRequest s -> Bool
isSetOverlay SetOverlay{} = True
isSetOverlay _ = False

isResetOverlay :: WidgetRequest s -> Bool
isResetOverlay ResetOverlay{} = True
isResetOverlay _ = False

isSetFocus :: WidgetRequest s -> Bool
isSetFocus SetFocus{} = True
isSetFocus _ = False

isSetCursorIcon :: WidgetRequest s -> Bool
isSetCursorIcon SetCursorIcon{} = True
isSetCursorIcon _ = False

isProducerHandler :: WidgetRequest s -> Bool
isProducerHandler RunProducer{} = True
isProducerHandler _ = False

isTaskHandler :: WidgetRequest s -> Bool
isTaskHandler RunTask{} = True
isTaskHandler _ = False

getUpdateModelReqs :: (Traversable t) => t (WidgetRequest s) -> Seq (s -> s)
getUpdateModelReqs reqs = foldl' foldHelper Seq.empty reqs where
  foldHelper acc (UpdateModel fn) = acc |> fn
  foldHelper acc _ = acc
