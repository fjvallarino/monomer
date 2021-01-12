{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Monomer.Main.Handlers (
  HandlerStep,
  handleWidgetResult,
  handleSystemEvents,
  handleResourcesInit,
  handleWidgetInit,
  handleWidgetRestore,
  handleWidgetDispose,
  handleRequests
) where

import Control.Concurrent.Async (async)
import Control.Lens (
  (&), (^.), (.~), (%~), (.=), (%=), (?=), ix, at, non, use, _1)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan)
import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Extra(concatMapM)
import Control.Monad.IO.Class
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq(..), (><), (|>))
import Data.Typeable (Typeable)
import Foreign (alloca, poke)
import SDL (($=))

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified SDL
import qualified SDL.Raw.Enum as SDLEnum
import qualified SDL.Raw.Event as SDLE
import qualified SDL.Raw.Types as SDLT

import Monomer.Core
import Monomer.Event
import Monomer.Main.Types
import Monomer.Main.Util

import qualified Monomer.Lens as L

type HandlerStep s e = (WidgetEnv s e, Seq e, WidgetNode s e)

getTargetPath
  :: WidgetEnv s e
  -> Maybe Path
  -> Maybe Path
  -> Path
  -> SystemEvent
  -> WidgetNode s e
  -> Maybe Path
getTargetPath wenv pressed overlay target event widgetRoot = case event of
    -- Keyboard
    KeyAction{}                       -> pathEvent target
    TextInput _                       -> pathEvent target
    -- Clipboard
    Clipboard _                       -> pathEvent target
    -- Mouse/touch
    ButtonAction point _ PressedBtn _ -> pointEvent point
    ButtonAction _ _ ReleasedBtn _    -> pathEvent target
    Click{}                           -> pathEvent target
    DblClick{}                        -> pathEvent target
    WheelScroll point _ _             -> pointEvent point
    Focus                             -> pathEvent target
    Blur                              -> pathEvent target
    Enter{}                           -> pathEvent target
    Move point                        -> pointEvent point
    Leave{}                           -> pathEvent target
  where
    widget = widgetRoot ^. L.widget
    startPath = fromMaybe emptyPath overlay
    pathEvent = Just
    pathFromPoint p = widgetFindByPoint widget wenv startPath p widgetRoot
    -- pressed is only really used for Move
    pointEvent point = pressed <|> pathFromPoint point <|> overlay

handleSystemEvents
  :: (MonomerM s m)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> m (HandlerStep s e)
handleSystemEvents wenv baseEvents widgetRoot = nextStep where
  mainBtn = wenv ^. L.mainButton
  reduceBaseEvt currStep evt = do
    let (currWenv, currEvents, currRoot) = currStep
    systemEvents <- addRelatedEvents currWenv mainBtn currRoot evt
    mainBtnPress <- use L.mainBtnPress
    inputStatus <- use L.inputStatus

    let newWenv = currWenv
          & L.mainBtnPress .~ mainBtnPress
          & L.inputStatus .~ inputStatus

    foldM reduceSysEvt (newWenv, currEvents, currRoot) systemEvents
  reduceSysEvt (currWenv, currEvents, currRoot) (evt, evtTarget) = do
    focused <- use L.focusedPath
    let target = fromMaybe focused evtTarget

    (wenv2, evts2, wroot2) <- handleSystemEvent currWenv evt target currRoot

    return (wenv2, currEvents >< evts2, wroot2)
  processedEvents = preProcessEvents baseEvents
  nextStep = foldM reduceBaseEvt (wenv, Seq.empty, widgetRoot) processedEvents

handleSystemEvent
  :: (MonomerM s m)
  => WidgetEnv s e
  -> SystemEvent
  -> Path
  -> WidgetNode s e
  -> m (HandlerStep s e)
handleSystemEvent wenv event currentTarget widgetRoot = do
  mainStart <- use L.mainBtnPress
  overlay <- use L.overlayPath
  leaveEnterPair <- use L.leaveEnterPair
  let pressed = fmap fst mainStart

  case getTargetPath wenv pressed overlay currentTarget event widgetRoot of
    Nothing -> return (wenv, Seq.empty, widgetRoot)
    Just target -> do
      let widget = widgetRoot ^. L.widget
      let emptyResult = WidgetResult widgetRoot Seq.empty Seq.empty
      let evtResult = widgetHandleEvent widget wenv target event widgetRoot
      let widgetResult = fromMaybe emptyResult evtResult
      -- Do not resize if event is Leave and follow-up is Enter
      let resizeWidgets = not (leaveEnterPair && isOnLeave event)

      handleWidgetResult wenv resizeWidgets widgetResult {
        _wrRequests = addFocusReq event (_wrRequests widgetResult)
      }

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
  -> WidgetNode s e
  -> m (HandlerStep s e)
handleWidgetInit wenv widgetRoot = do
  let widget = widgetRoot ^. L.widget
  let widgetResult = widgetInit widget wenv widgetRoot

  handleWidgetResult wenv True widgetResult
    >>= handleMoveFocus Nothing FocusFwd

handleWidgetRestore
  :: (MonomerM s m)
  => WidgetEnv s e
  -> WidgetInstanceNode
  -> WidgetNode s e
  -> m (HandlerStep s e)
handleWidgetRestore wenv widgetInst widgetRoot = do
  let widget = widgetRoot ^. L.widget
  let widgetResult = widgetRestore widget wenv widgetInst widgetRoot

  handleWidgetResult wenv True widgetResult

handleWidgetDispose
  :: (MonomerM s m)
  => WidgetEnv s e
  -> WidgetNode s e
  -> m (HandlerStep s e)
handleWidgetDispose wenv widgetRoot = do
  let widget = widgetRoot ^. L.widget
  let widgetResult = widgetDispose widget wenv widgetRoot

  handleWidgetResult wenv False widgetResult

handleWidgetResult
  :: (MonomerM s m)
  => WidgetEnv s e
  -> Bool
  -> WidgetResult s e
  -> m (HandlerStep s e)
handleWidgetResult wenv resizeWidgets result = do
  let WidgetResult evtRoot reqs events = result
  let resizeReq = isResizeResult (Just result)

  resizePending <- use L.resizePending
  step <- handleRequests reqs (wenv, events, evtRoot)

  if resizeWidgets && (resizeReq || resizePending)
    then handleResizeWidgets step
    else do
      L.resizePending .= resizeReq
      return step

handleRequests
  :: (MonomerM s m)
  => Seq (WidgetRequest s)
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleRequests reqs step = foldM handleRequest step reqs where
  handleRequest step req = case req of
    IgnoreParentEvents -> return step
    IgnoreChildrenEvents -> return step
    ResizeWidgets -> return step
    MoveFocus start dir -> handleMoveFocus start dir step
    SetFocus path -> handleSetFocus path step
    GetClipboard path -> handleGetClipboard path step
    SetClipboard cdata -> handleSetClipboard cdata step
    StartTextInput rect -> handleStartTextInput rect step
    StopTextInput -> handleStopTextInput step
    SetOverlay path -> handleSetOverlay path step
    ResetOverlay -> handleResetOverlay step
    SetCursorIcon icon -> handleSetCursorIcon icon step
    RenderOnce -> handleRenderOnce step
    RenderEvery path ms -> handleRenderEvery path ms step
    RenderStop path -> handleRenderStop path step
    ExitApplication exit -> handleExitApplication exit step
    UpdateWindow req -> handleUpdateWindow req step
    UpdateModel fn -> handleUpdateModel fn step
    UpdateWidgetPath wid path -> handleUpdateWidgetPath wid path step
    SendMessage path msg -> handleSendMessage path msg step
    RunTask wid path handler -> handleRunTask wid path handler step
    RunProducer wid path handler -> handleRunProducer wid path handler step

handleResizeWidgets
  :: (MonomerM s m)
  => HandlerStep s e
  -> m (HandlerStep s e)
handleResizeWidgets previousStep = do
  windowSize <- use L.windowSize

  liftIO . putStrLn $ "Resizing widgets"

  let (wenv, events, widgetRoot) = previousStep
  let newWidgetRoot = resizeRoot wenv windowSize widgetRoot

  L.renderRequested .= True
  L.resizePending .= False

  return (wenv, events, newWidgetRoot)

handleMoveFocus
  :: (MonomerM s m)
  => Maybe Path
  -> FocusDirection
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleMoveFocus startFrom direction (wenv, events, root) = do
  oldFocus <- use L.focusedPath
  let wenv0 = wenv { _weFocusedPath = emptyPath }
  (wenv1, events1, root1) <- handleSystemEvent wenv0 Blur oldFocus root
  currFocus <- use L.focusedPath
  currOverlay <- use L.overlayPath

  if oldFocus == currFocus
    then do
      let searchFrom = fromMaybe currFocus startFrom
      let newFocus = findNextFocus wenv1 direction searchFrom currOverlay root1
      let tempWenv = wenv1 { _weFocusedPath = newFocus }

      L.focusedPath .= newFocus
      (wenv2, events2, root2) <- handleSystemEvent tempWenv Focus newFocus root1

      return (wenv2, events >< events1 >< events2, root2)
    else
      return (wenv1, events1, root1)

handleSetFocus
  :: (MonomerM s m) => Path -> HandlerStep s e -> m (HandlerStep s e)
handleSetFocus newFocus (wenv, events, root) =  do
  let wenv0 = wenv { _weFocusedPath = newFocus }

  oldFocus <- use L.focusedPath

  if oldFocus /= newFocus
    then do
      L.focusedPath .= newFocus

      (wenv1, events1, root1) <- handleSystemEvent wenv0 Blur oldFocus root
      (wenv2, events2, root2) <- handleSystemEvent wenv1 Focus newFocus root1

      return (wenv2, events >< events1 >< events2, root2)
    else
      return (wenv, events, root)

handleGetClipboard
  :: (MonomerM s m) => Path -> HandlerStep s e -> m (HandlerStep s e)
handleGetClipboard path (wenv, evts, root) = do
  hasText <- SDL.hasClipboardText
  contents <- if hasText
                then fmap ClipboardText SDL.getClipboardText
                else return ClipboardEmpty

  (wenv2, evts2, root2) <- handleSystemEvent wenv (Clipboard contents) path root
  return (wenv2, evts >< evts2, root2)

handleSetClipboard
  :: (MonomerM s m) => ClipboardData -> HandlerStep s e -> m (HandlerStep s e)
handleSetClipboard (ClipboardText text) previousStep = do
  SDL.setClipboardText text
  return previousStep
handleSetClipboard _ previousStep = return previousStep

handleStartTextInput
  :: (MonomerM s m) => Rect -> HandlerStep s e -> m (HandlerStep s e)
handleStartTextInput (Rect x y w h) previousStep = do
  SDL.startTextInput (SDLT.Rect (c x) (c y) (c w) (c h))
  return previousStep
  where
    c x = fromIntegral $ round x

handleStopTextInput :: (MonomerM s m) => HandlerStep s e -> m (HandlerStep s e)
handleStopTextInput previousStep = do
  SDL.stopTextInput
  return previousStep

handleSetOverlay
  :: (MonomerM s m) => Path -> HandlerStep s e -> m (HandlerStep s e)
handleSetOverlay path previousStep = do
  L.overlayPath .= Just path
  return previousStep

handleResetOverlay :: (MonomerM s m) => HandlerStep s e -> m (HandlerStep s e)
handleResetOverlay previousStep = do
  L.overlayPath .= Nothing
  return previousStep

handleSetCursorIcon
  :: (MonomerM s m) => CursorIcon -> HandlerStep s e -> m (HandlerStep s e)
handleSetCursorIcon icon previousStep = do
  L.currentCursor .= icon
  cursor <- (Map.! icon) <$> use L.cursorIcons
  SDLE.setCursor cursor

  return previousStep

handleRenderOnce :: (MonomerM s m) => HandlerStep s e -> m (HandlerStep s e)
handleRenderOnce previousStep = do
  L.renderRequested .= True
  return previousStep

handleRenderEvery
  :: (MonomerM s m) => Path -> Int -> HandlerStep s e -> m (HandlerStep s e)
handleRenderEvery path ms previousStep = do
  schedule <- use L.renderSchedule
  L.renderSchedule .= addSchedule schedule
  return previousStep
  where
    (wenv, _, _) = previousStep
    newValue = RenderSchedule {
      _rsPath = path,
      _rsStart = _weTimestamp wenv,
      _rsMs = ms
    }
    addSchedule schedule
      | ms > 0 = Map.insert path newValue schedule
      | otherwise = schedule

handleRenderStop :: (MonomerM s m) => Path -> HandlerStep s e -> m (HandlerStep s e)
handleRenderStop path previousStep = do
  schedule <- use L.renderSchedule
  L.renderSchedule .= Map.delete path schedule
  return previousStep

handleExitApplication
  :: (MonomerM s m) => Bool -> HandlerStep s e -> m (HandlerStep s e)
handleExitApplication exit previousStep = do
  L.exitApplication .= exit
  return previousStep

handleUpdateWindow
  :: (MonomerM s m) => WindowRequest -> HandlerStep s e -> m (HandlerStep s e)
handleUpdateWindow windowRequest previousStep = do
  window <- use L.window
  case windowRequest of
    WindowSetTitle title -> SDL.windowTitle window $= title
    WindowSetFullScreen -> SDL.setWindowMode window SDL.FullscreenDesktop
    WindowMaximize -> SDL.setWindowMode window SDL.Maximized
    WindowMinimize -> SDL.setWindowMode window SDL.Minimized
    WindowRestore -> SDL.setWindowMode window SDL.Windowed
    WindowBringToFront -> SDL.raiseWindow window
  return previousStep

handleUpdateModel
  :: (MonomerM s m) => (s -> s) -> HandlerStep s e -> m (HandlerStep s e)
handleUpdateModel fn (wenv, evts, root) = do
  L.mainModel .= _weModel wenv2
  return (wenv2, evts, root)
  where
    wenv2 = wenv & L.model %~ fn

handleUpdateWidgetPath
  :: (MonomerM s m) => WidgetId -> Path -> HandlerStep s e -> m (HandlerStep s e)
handleUpdateWidgetPath wid path step = do
  setWidgetIdPath wid path
  return step

handleSendMessage
  :: forall s e m msg . (MonomerM s m, Typeable msg)
  => Path
  -> msg
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleSendMessage path message (wenv, events, widgetRoot) = do
  let emptyResult = WidgetResult widgetRoot Seq.empty Seq.empty
  let widget = widgetRoot ^. L.widget
  let msgResult = widgetHandleMessage widget wenv path message widgetRoot
  let result = fromMaybe emptyResult msgResult

  (newWenv, newEvents, newWidgetRoot) <- handleWidgetResult wenv True result

  return (newWenv, events >< newEvents, newWidgetRoot)

handleRunTask
  :: forall s e m i . (MonomerM s m, Typeable i)
  => WidgetId
  -> Path
  -> IO i
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleRunTask widgetId path handler previousStep = do
  asyncTask <- liftIO $ async (liftIO handler)

  previousTasks <- use L.widgetTasks
  L.widgetTasks .= previousTasks |> WidgetTask widgetId asyncTask
  addWidgetIdPath widgetId path

  return previousStep

handleRunProducer
  :: forall s e m i . (MonomerM s m, Typeable i)
  => WidgetId
  -> Path
  -> ((i -> IO ()) -> IO ())
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleRunProducer widgetId path handler previousStep = do
  newChannel <- liftIO newTChanIO
  asyncTask <- liftIO $ async (liftIO $ handler (sendMessage newChannel))

  previousTasks <- use L.widgetTasks
  L.widgetTasks .= previousTasks |> WidgetProducer widgetId newChannel asyncTask
  addWidgetIdPath widgetId path

  return previousStep

addFocusReq
  :: SystemEvent
  -> Seq (WidgetRequest s)
  -> Seq (WidgetRequest s)
addFocusReq (KeyAction mod code KeyPressed) reqs = newReqs where
  isTabPressed = isKeyTab code
  stopProcessing = isJust $ Seq.findIndexL isIgnoreParentEvents reqs
  focusReqExists = isJust $ Seq.findIndexL isFocusRequest reqs
  focusReqNeeded = isTabPressed && not stopProcessing && not focusReqExists
  direction
    | mod ^. L.leftShift = FocusBwd
    | otherwise = FocusFwd
  newReqs
    | focusReqNeeded = reqs |> MoveFocus Nothing direction
    | otherwise = reqs
addFocusReq _ reqs = reqs

preProcessEvents :: [SystemEvent] -> [SystemEvent]
preProcessEvents [] = []
preProcessEvents (e:es) = case e of
  WheelScroll p _ _ -> e : Move p : preProcessEvents es
  _ -> e : preProcessEvents es

addRelatedEvents
  :: (MonomerM s m)
  => WidgetEnv s e
  -> Button
  -> WidgetNode s e
  -> SystemEvent
  -> m [(SystemEvent, Maybe Path)]
addRelatedEvents wenv mainBtn widgetRoot evt = case evt of
  Move point -> do
    (_, hoverEvts) <- addHoverEvents wenv widgetRoot point
    -- Update input status
    status <- use L.inputStatus
    L.inputStatus . L.mousePosPrev .= status ^. L.mousePos
    L.inputStatus . L.mousePos .= point

    return $ hoverEvts ++ [(evt, Nothing)]
  ButtonAction point btn PressedBtn _ -> do
    overlay <- use L.overlayPath
    let startPath = fromMaybe emptyPath overlay
    let widget = widgetRoot ^. L.widget
    let curr = widgetFindByPoint widget wenv startPath point widgetRoot

    when (btn == mainBtn) $
      L.mainBtnPress .= fmap (, point) curr

    L.inputStatus . L.buttons . at btn ?= PressedBtn

    SDLE.captureMouse True

    return [(evt, Nothing)]
  ButtonAction point btn ReleasedBtn clicks -> do
    -- Hover changes need to be handled here too
    mainPress <- use L.mainBtnPress

    when (btn == mainBtn) $
      L.mainBtnPress .= Nothing

    (target, hoverEvts) <- addHoverEvents wenv widgetRoot point

    let pressed = fmap fst mainPress
    let isPressed = btn == mainBtn && target == pressed
    let clickEvt = [(Click point btn, pressed) | isPressed && clicks == 1]
    let dblClickEvt = [(DblClick point btn, pressed) | isPressed && clicks == 2]
    let releasedEvt = [(evt, pressed <|> target)]

    L.inputStatus . L.buttons . at btn ?= ReleasedBtn

    SDLE.captureMouse False

    return $ clickEvt ++ dblClickEvt ++ releasedEvt ++ hoverEvts
  KeyAction mod code status -> do
    L.inputStatus . L.keyMod .= mod
    L.inputStatus . L.keys . at code ?= status

    return [(evt, Nothing)]
  -- These handlers are only here to help with testing functions
  -- This will only be reached from `handleSystemEvents`
  Click point btn -> findEvtTargetByPoint wenv widgetRoot evt point
  DblClick point btn -> findEvtTargetByPoint wenv widgetRoot evt point
  _ -> return [(evt, Nothing)]

addHoverEvents
  :: (MonomerM s m)
  => WidgetEnv s e
  -> WidgetNode s e
  -> Point
  -> m (Maybe Path, [(SystemEvent, Maybe Path)])
addHoverEvents wenv widgetRoot point = do
  overlay <- use L.overlayPath
  hover <- use L.hoveredPath
  mainBtnPress <- use L.mainBtnPress
  let startPath = fromMaybe emptyPath overlay
  let widget = widgetRoot ^. L.widget
  let target = widgetFindByPoint widget wenv startPath point widgetRoot
  let hoverChanged = target /= hover && isNothing mainBtnPress
  let enter = [(Enter point, target) | isJust target && hoverChanged]
  let leave = [(Leave point, hover) | isJust hover && hoverChanged]

  when hoverChanged $
    L.hoveredPath .= target

  L.leaveEnterPair .= not (null leave || null enter)

  return (target, leave ++ enter)

findEvtTargetByPoint
  :: (MonomerM s m)
  => WidgetEnv s e
  -> WidgetNode s e
  -> SystemEvent
  -> Point
  -> m [(SystemEvent, Maybe Path)]
findEvtTargetByPoint wenv widgetRoot evt point = do
  overlay <- use L.overlayPath
  let startPath = fromMaybe emptyPath overlay
  let widget = widgetRoot ^. L.widget
  let curr = widgetFindByPoint widget wenv startPath point widgetRoot
  return [(evt, curr)]

sendMessage :: TChan e -> e -> IO ()
sendMessage channel message = atomically $ writeTChan channel message

cursorToSDL :: CursorIcon -> SDLEnum.SystemCursor
cursorToSDL CursorArrow = SDLEnum.SDL_SYSTEM_CURSOR_ARROW
cursorToSDL CursorHand = SDLEnum.SDL_SYSTEM_CURSOR_HAND
cursorToSDL CursorIBeam = SDLEnum.SDL_SYSTEM_CURSOR_IBEAM
cursorToSDL CursorInvalid = SDLEnum.SDL_SYSTEM_CURSOR_NO
cursorToSDL CursorSizeH = SDLEnum.SDL_SYSTEM_CURSOR_SIZEWE
cursorToSDL CursorSizeV = SDLEnum.SDL_SYSTEM_CURSOR_SIZENS
cursorToSDL CursorDiagTL = SDLEnum.SDL_SYSTEM_CURSOR_SIZENWSE
cursorToSDL CursorDiagTR = SDLEnum.SDL_SYSTEM_CURSOR_SIZENESW

setWidgetIdPath :: (MonomerM s m) => WidgetId -> Path -> m ()
setWidgetIdPath widgetId path =
  L.widgetPaths . ix widgetId . _1 .= path

addWidgetIdPath :: (MonomerM s m) => WidgetId -> Path -> m ()
addWidgetIdPath widgetId path =
  L.widgetPaths . at widgetId . non (path, 0) %= \(_, c) -> (path, c + 1)
