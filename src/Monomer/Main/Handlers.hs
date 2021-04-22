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
  handleRequests,
  handleResizeWidgets
) where

import Control.Concurrent.Async (async)
import Control.Lens
  ((&), (^.), (^?), (.~), (%~), (.=), (?=), _Just, _1, _2, ix, at, use)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan)
import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Extra (concatMapM, maybeM)
import Control.Monad.IO.Class
import Data.Default
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq(..), (|>))
import Data.Typeable (Typeable, typeOf)
import Foreign (alloca, poke)
import Safe (headMay)
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
import Control.Monad.Trans.Maybe

type HandlerStep s e = (WidgetEnv s e, WidgetNode s e, Seq (WidgetRequest s e))

getTargetPath
  :: WidgetEnv s e
  -> Maybe Path
  -> Maybe Path
  -> Path
  -> SystemEvent
  -> WidgetNode s e
  -> Maybe Path
getTargetPath wenv pressed overlay target event root = case event of
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
    -- Drag/drop
    Drag point _ _                    -> pointEvent point
    Drop point _ _                    -> pointEvent point
  where
    widget = root ^. L.widget
    startPath = fromMaybe emptyPath overlay
    pathEvent = Just
    pathFromPoint p = fmap (^. L.path) wni where
      wni = widgetFindByPoint widget wenv startPath p root
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
  reduceEvt curStep evt = do
    let (curWenv, curRoot, curReqs) = curStep
    systemEvents <- addRelatedEvents curWenv mainBtn curRoot evt

    foldM reduceSysEvt (curWenv, curRoot, curReqs) systemEvents
  reduceSysEvt curStep (evt, evtTarget) = do
    focused <- getFocusedPath
    let (curWenv, curRoot, curReqs) = curStep
    let target = fromMaybe focused evtTarget
    let curWidget = curRoot ^. L.widget
    let targetWni = evtTarget
          >>= flip (widgetFindByPath curWidget curWenv) curRoot
    let targetWid = (^. L.widgetId) <$> targetWni

    when (isOnEnter evt) $
      L.hoveredWidgetId .= targetWid

    when (isOnMove evt)
      restoreCursorOnWindowEnter

    curCursor <- getCurrentCursor
    hoveredPath <- getHoveredPath
    mainBtnPress <- use L.mainBtnPress
    inputStatus <- use L.inputStatus

    let tmpWenv = curWenv
          & L.cursor .~ curCursor
          & L.hoveredPath .~ hoveredPath
          & L.mainBtnPress .~ mainBtnPress
          & L.inputStatus .~ inputStatus
    let findByPath path = widgetFindByPath curWidget tmpWenv path curRoot
    let newWenv = tmpWenv
          & L.findByPath .~ findByPath
    (wenv2, root2, reqs2) <- handleSystemEvent newWenv evt target curRoot

    when (isOnLeave evt) $ do
      resetCursorOnNodeLeave evt curStep
      L.hoveredWidgetId .= Nothing

    return (wenv2, root2, curReqs <> reqs2)
  newEvents = preProcessEvents baseEvents
  nextStep = foldM reduceEvt (wenv, widgetRoot, Seq.empty) newEvents

handleSystemEvent
  :: (MonomerM s m)
  => WidgetEnv s e
  -> SystemEvent
  -> Path
  -> WidgetNode s e
  -> m (HandlerStep s e)
handleSystemEvent wenv event currentTarget widgetRoot = do
  mainStart <- use L.mainBtnPress
  overlay <- getOverlayPath
  leaveEnterPair <- use L.leaveEnterPair
  let pressed = fmap fst mainStart

  case getTargetPath wenv pressed overlay currentTarget event widgetRoot of
    Nothing -> return (wenv, widgetRoot, Seq.empty)
    Just target -> do
      let widget = widgetRoot ^. L.widget
      let emptyResult = WidgetResult widgetRoot Seq.empty
      let evtResult = widgetHandleEvent widget wenv target event widgetRoot
      let resizeWidgets = not (leaveEnterPair && isOnLeave event)
      let widgetResult = fromMaybe emptyResult evtResult
            & L.requests %~ addFocusReq event

      step <- handleWidgetResult wenv resizeWidgets widgetResult

      if isDropEvent event
        then handleFinalizeDrop step
        else return step

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
  let reqs = widgetResult ^. L.requests
  let focusReqExists = isJust $ Seq.findIndexL isFocusRequest reqs

  L.resizePending .= True
  step <- handleWidgetResult wenv True widgetResult

  if not focusReqExists
    then handleMoveFocus Nothing FocusFwd step
    else return step

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
  let WidgetResult evtRoot reqs = result
  let resizeReq = isResizeResult (Just result)

  resizePending <- use L.resizePending
  step <- handleRequests reqs (wenv, evtRoot, reqs)

  if resizeWidgets && (resizeReq || resizePending)
    then handleResizeWidgets step
    else do
      L.resizePending .= resizeReq
      return step

handleRequests
  :: (MonomerM s m)
  => Seq (WidgetRequest s e)
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleRequests reqs step = foldM handleRequest step reqs where
  handleRequest step req = case req of
    IgnoreParentEvents -> return step
    IgnoreChildrenEvents -> return step
    ResizeWidgets -> return step
    MoveFocus start dir -> handleMoveFocus start dir step
    SetFocus path -> handleSetFocus path step
    GetClipboard wid -> handleGetClipboard wid step
    SetClipboard cdata -> handleSetClipboard cdata step
    StartTextInput rect -> handleStartTextInput rect step
    StopTextInput -> handleStopTextInput step
    SetOverlay wid path -> handleSetOverlay wid path step
    ResetOverlay wid -> handleResetOverlay wid step
    SetCursorIcon wid icon -> handleSetCursorIcon wid icon step
    ResetCursorIcon wid -> handleResetCursorIcon wid step
    StartDrag wid path info -> handleStartDrag wid path info step
    StopDrag wid -> handleStopDrag wid step
    RenderOnce -> handleRenderOnce step
    RenderEvery wid ms repeat -> handleRenderEvery wid ms repeat step
    RenderStop wid -> handleRenderStop wid step
    ExitApplication exit -> handleExitApplication exit step
    UpdateWindow req -> handleUpdateWindow req step
    UpdateModel fn -> handleUpdateModel fn step
    SetWidgetPath wid path -> handleSetWidgetPath wid path step
    ResetWidgetPath wid -> handleResetWidgetPath wid step
    RaiseEvent msg -> handleRaiseEvent msg step
    SendMessage wid msg -> handleSendMessage wid msg step
    RunTask wid path handler -> handleRunTask wid path handler step
    RunProducer wid path handler -> handleRunProducer wid path handler step

handleResizeWidgets
  :: (MonomerM s m)
  => HandlerStep s e
  -> m (HandlerStep s e)
handleResizeWidgets previousStep = do
  Size w h <- use L.windowSize

  let winRect = Rect 0 0 w h
  let (wenv, root, reqs) = previousStep
  let newResult = widgetResize (root ^. L.widget) wenv winRect root

  L.renderRequested .= True
  L.resizePending .= False

  (wenv2, root2, reqs2) <- handleWidgetResult wenv True newResult

  return (wenv2, root2, reqs <> reqs2)

handleMoveFocus
  :: (MonomerM s m)
  => Maybe WidgetId
  -> FocusDirection
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleMoveFocus startFromWid dir (wenv, root, reqs) = do
  oldFocus <- getFocusedPath
  let wenv0 = wenv & L.focusedPath .~ emptyPath
  (wenv1, root1, reqs1) <- handleSystemEvent wenv0 Blur oldFocus root
  currFocus <- getFocusedPath
  currOverlay <- getOverlayPath

  if oldFocus == currFocus
    then do
      startFrom <- mapM getWidgetIdPath startFromWid
      let searchFrom = fromMaybe currFocus startFrom
      let newFocusWni = findNextFocus wenv1 dir searchFrom currOverlay root1
      let newFocus = newFocusWni ^. L.path
      let wenvF = wenv1 & L.focusedPath .~ newFocus

      L.focusedWidgetId .= newFocusWni ^. L.widgetId
      L.renderRequested .= True
      (wenv2, root2, reqs2) <- handleSystemEvent wenvF Focus newFocus root1

      return (wenv2, root2, reqs <> reqs1 <> reqs2)
    else
      return (wenv1, root1, reqs <> reqs1)

handleSetFocus
  :: (MonomerM s m) => WidgetId -> HandlerStep s e -> m (HandlerStep s e)
handleSetFocus newFocusWid (wenv, root, reqs) = do
  newFocus <- getWidgetIdPath newFocusWid
  oldFocus <- getFocusedPath

  if oldFocus /= newFocus
    then do
      let wenv0 = wenv & L.focusedPath .~ newFocus
      (wenv1, root1, reqs1) <- handleSystemEvent wenv0 Blur oldFocus root
      let wenvF = wenv1 & L.focusedPath .~ newFocus

      L.focusedWidgetId .= newFocusWid
      L.renderRequested .= True
      (wenv2, root2, reqs2) <- handleSystemEvent wenvF Focus newFocus root1

      return (wenv2, root2, reqs <> reqs1 <> reqs2)
    else
      return (wenv, root, reqs)

handleGetClipboard
  :: (MonomerM s m) => WidgetId -> HandlerStep s e -> m (HandlerStep s e)
handleGetClipboard widgetId (wenv, root, reqs) = do
  path <- getWidgetIdPath widgetId
  hasText <- SDL.hasClipboardText
  contents <- fmap Clipboard $ if hasText
                then fmap ClipboardText SDL.getClipboardText
                else return ClipboardEmpty

  (wenv2, root2, reqs2) <- handleSystemEvent wenv contents path root
  return (wenv2, root2, reqs <> reqs2)

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
  :: (MonomerM s m)
  => WidgetId
  -> Path
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleSetOverlay widgetId path previousStep = do
  overlay <- use L.overlayWidgetId

  L.overlayWidgetId .= Just widgetId
  setWidgetIdPath widgetId path
  return previousStep

handleResetOverlay
  :: (MonomerM s m) => WidgetId -> HandlerStep s e -> m (HandlerStep s e)
handleResetOverlay widgetId step = do
  let (wenv, root, reqs) = step
  let mousePos = wenv ^. L.inputStatus . L.mousePos

  overlay <- use L.overlayWidgetId

  (wenv2, root2, reqs2) <- if overlay == Just widgetId
    then do
      L.overlayWidgetId .= Nothing
      void $ handleResetCursorIcon widgetId step
      handleSystemEvents wenv [Move mousePos] root
    else
      return (wenv, root, Empty)

  return (wenv2, root2, reqs <> reqs2)

handleSetCursorIcon
  :: (MonomerM s m)
  => WidgetId
  -> CursorIcon
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleSetCursorIcon wid icon previousStep = do
  cursors <- use L.cursorStack >>= dropNonParentWidgetId wid
  L.cursorStack .= (wid, icon) : cursors
  cursor <- (Map.! icon) <$> use L.cursorIcons
  SDLE.setCursor cursor

  return previousStep

handleResetCursorIcon
  :: (MonomerM s m)
  => WidgetId
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleResetCursorIcon wid previousStep = do
  cursors <- use L.cursorStack >>= dropNonParentWidgetId wid
  let newCursors = dropWhile ((==wid) . fst) cursors
  let newCursorIcon
        | null newCursors = CursorArrow
        | otherwise = snd . head $ newCursors
  L.cursorStack .= newCursors
  cursor <- (Map.! newCursorIcon) <$> use L.cursorIcons
  SDLE.setCursor cursor

  return previousStep

handleStartDrag
  :: (MonomerM s m)
  => WidgetId
  -> Path
  -> WidgetDragMsg
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleStartDrag widgetId path dragData previousStep = do
  oldDragAction <- use L.dragAction
  let prevWidgetId = fmap (^. L.widgetId) oldDragAction

  L.dragAction .= Just (DragAction widgetId dragData)
  setWidgetIdPath widgetId path
  return previousStep

handleStopDrag
  :: (MonomerM s m)
  => WidgetId
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleStopDrag widgetId previousStep = do
  oldDragAction <- use L.dragAction
  let prevWidgetId = fmap (^. L.widgetId) oldDragAction

  if prevWidgetId == Just widgetId
    then do
      L.renderRequested .= True
      L.dragAction .= Nothing
      return $ previousStep & _1 . L.dragStatus .~ Nothing
  else return previousStep

handleFinalizeDrop
  :: (MonomerM s m)
  => HandlerStep s e
  -> m (HandlerStep s e)
handleFinalizeDrop previousStep = do
  dragAction <- use L.dragAction
  let widgetId = fmap (^. L.widgetId) dragAction

  if isJust widgetId
    then do
      L.renderRequested .= True
      L.dragAction .= Nothing
      return $ previousStep & _1 . L.dragStatus .~ Nothing
    else return previousStep

handleRenderOnce :: (MonomerM s m) => HandlerStep s e -> m (HandlerStep s e)
handleRenderOnce previousStep = do
  L.renderRequested .= True
  return previousStep

handleRenderEvery
  :: (MonomerM s m)
  => WidgetId
  -> Int
  -> Maybe Int
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleRenderEvery widgetId ms repeat previousStep = do
  schedule <- use L.renderSchedule
  L.renderSchedule .= addSchedule schedule
  return previousStep
  where
    (wenv, _, _) = previousStep
    newValue = RenderSchedule {
      _rsWidgetId = widgetId,
      _rsStart = _weTimestamp wenv,
      _rsMs = ms,
      _rsRepeat = repeat
    }
    addSchedule schedule
      | ms > 0 = Map.insert widgetId newValue schedule
      | otherwise = schedule

handleRenderStop
  :: (MonomerM s m) => WidgetId -> HandlerStep s e -> m (HandlerStep s e)
handleRenderStop widgetId previousStep = do
  schedule <- use L.renderSchedule
  L.renderSchedule .= Map.delete widgetId schedule
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
handleUpdateModel fn (wenv, root, reqs) = do
  L.mainModel .= _weModel wenv2
  return (wenv2, root, reqs)
  where
    wenv2 = wenv & L.model %~ fn

handleSetWidgetPath
  :: (MonomerM s m) => WidgetId -> Path -> HandlerStep s e -> m (HandlerStep s e)
handleSetWidgetPath wid path step = do
  setWidgetIdPath wid path
  return step

handleResetWidgetPath
  :: (MonomerM s m) => WidgetId -> HandlerStep s e -> m (HandlerStep s e)
handleResetWidgetPath wid step = do
  delWidgetIdPath wid
  return step

handleRaiseEvent
  :: forall s e m msg . (MonomerM s m, Typeable msg)
  => msg
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleRaiseEvent message step = do
  --liftIO . putStrLn $ message ++ show (typeOf message)
  return step
  where
    message = "Invalid state. RaiseEvent reached main handler. Type: "

handleSendMessage
  :: forall s e m msg . (MonomerM s m, Typeable msg)
  => WidgetId
  -> msg
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleSendMessage widgetId message (wenv, root, reqs) = do
  path <- getWidgetIdPath widgetId

  let emptyResult = WidgetResult root Seq.empty
  let widget = root ^. L.widget
  let msgResult = widgetHandleMessage widget wenv path message root
  let result = fromMaybe emptyResult msgResult

  (newWenv, newRoot, newReqs) <- handleWidgetResult wenv True result

  return (newWenv, newRoot, reqs <> newReqs)

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
  setWidgetIdPath widgetId path

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
  setWidgetIdPath widgetId path

  return previousStep

sendMessage :: TChan e -> e -> IO ()
sendMessage channel message = atomically $ writeTChan channel message

addFocusReq
  :: SystemEvent
  -> Seq (WidgetRequest s e)
  -> Seq (WidgetRequest s e)
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
    (target, hoverEvts) <- addHoverEvents wenv widgetRoot point
    -- Update input status
    status <- use L.inputStatus
    L.inputStatus . L.mousePosPrev .= status ^. L.mousePos
    L.inputStatus . L.mousePos .= point
    -- Drag event
    mainPress <- use L.mainBtnPress
    draggedMsg <- getDraggedMsgInfo
    let pressed = fmap fst mainPress
    let isPressed = target == pressed
    let dragEvts = case draggedMsg of
          Just (path, msg) -> [(Drag point path msg, target) | not isPressed]
          _ -> []

    when (isJust mainPress || isJust draggedMsg) $
      L.renderRequested .= True

    return $ hoverEvts ++ dragEvts ++ [(evt, Nothing)]
  ButtonAction point btn PressedBtn _ -> do
    overlay <- getOverlayPath
    let startPath = fromMaybe emptyPath overlay
    let widget = widgetRoot ^. L.widget
    let wni = widgetFindByPoint widget wenv startPath point widgetRoot
    let curr = fmap (^. L.path) wni

    when (btn == mainBtn) $
      L.mainBtnPress .= fmap (, point) curr

    L.inputStatus . L.buttons . at btn ?= PressedBtn

    SDLE.captureMouse True

    return [(evt, Nothing)]
  ButtonAction point btn ReleasedBtn clicks -> do
    -- Hover changes need to be handled here too
    mainPress <- use L.mainBtnPress
    draggedMsg <- getDraggedMsgInfo

    when (btn == mainBtn) $
      L.mainBtnPress .= Nothing

    (target, hoverEvts) <- addHoverEvents wenv widgetRoot point

    let pressed = fmap fst mainPress
    let isPressed = btn == mainBtn && target == pressed
    let clickEvt = [(Click point btn, pressed) | isPressed && clicks == 1]
    let dblClickEvt = [(DblClick point btn, pressed) | isPressed && clicks == 2]
    let releasedEvt = [(evt, pressed <|> target)]
    let dropEvts = case draggedMsg of
          Just (path, msg) -> [(Drop point path msg, target) | not isPressed]
          _ -> []

    L.inputStatus . L.buttons . at btn ?= ReleasedBtn

    SDLE.captureMouse False

    return $ releasedEvt ++ dropEvts ++ clickEvt ++ dblClickEvt ++ hoverEvts
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
  overlay <- getOverlayPath
  hover <- getHoveredPath
  mainBtnPress <- use L.mainBtnPress
  let startPath = fromMaybe emptyPath overlay
  let widget = widgetRoot ^. L.widget
  let wni = widgetFindByPoint widget wenv startPath point widgetRoot
  let target = fmap (^. L.path) wni
  let hoverChanged = target /= hover && isNothing mainBtnPress
  let enter = [(Enter point, target) | isJust target && hoverChanged]
  let leave = [(Leave point, hover) | isJust hover && hoverChanged]

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
  overlay <- getOverlayPath
  let startPath = fromMaybe emptyPath overlay
  let widget = widgetRoot ^. L.widget
  let wni = widgetFindByPoint widget wenv startPath point widgetRoot
  let curr = fmap (^. L.path) wni
  return [(evt, curr)]

findNextFocus
  :: WidgetEnv s e
  -> FocusDirection
  -> Path
  -> Maybe Path
  -> WidgetNode s e
  -> WidgetNodeInfo
findNextFocus wenv dir start overlay widgetRoot = fromJust nextFocus where
  widget = widgetRoot ^. L.widget
  restartPath = fromMaybe emptyPath overlay
  candidateWni = widgetFindNextFocus widget wenv dir start widgetRoot
  fromRootWni = widgetFindNextFocus widget wenv dir restartPath widgetRoot
  focusWni = fromMaybe def (widgetFindByPath widget wenv start widgetRoot)
  nextFocus = candidateWni <|> fromRootWni <|> Just focusWni

dropNonParentWidgetId
  :: (MonomerM s m)
  => WidgetId
  -> [(WidgetId, a)]
  -> m [(WidgetId, a)]
dropNonParentWidgetId wid [] = return []
dropNonParentWidgetId wid (x:xs) = do
  path <- getWidgetIdPath wid
  cpath <- getWidgetIdPath cwid
  if isParentPath cpath path
    then return (x:xs)
    else dropNonParentWidgetId wid xs
  where
    (cwid, _) = x
    isParentPath parent child = seqStartsWith parent child && parent /= child

resetCursorOnNodeLeave
  :: (MonomerM s m)
  => SystemEvent
  -> HandlerStep s e
  -> m ()
resetCursorOnNodeLeave (Leave point) step = do
  void $ handleResetCursorIcon widgetId step
  where
    (wenv, root, _) = step
    widget = root ^. L.widget
    childNode = widgetFindByPoint widget wenv emptyPath point root
    widgetId = case childNode of
      Just info -> info ^. L.widgetId
      Nothing -> root ^. L.info . L.widgetId
resetCursorOnNodeLeave _ step = return ()

restoreCursorOnWindowEnter :: MonomerM s m => m ()
restoreCursorOnWindowEnter = do
  -- Restore old icon if needed
  Size ww wh <- use L.windowSize
  status <- use L.inputStatus
  cursorIcons <- use L.cursorIcons
  cursorPair <- headMay <$> use L.cursorStack
  let windowRect = Rect 0 0 ww wh
  let prevInside = pointInRect (status ^. L.mousePosPrev) windowRect
  let currInside = pointInRect (status ^. L.mousePos) windowRect
  let sdlCursor = cursorIcons Map.! snd (fromJust cursorPair)

  when (not prevInside && currInside && isJust cursorPair) $ do
    SDLE.setCursor sdlCursor

cursorToSDL :: CursorIcon -> SDLEnum.SystemCursor
cursorToSDL CursorArrow = SDLEnum.SDL_SYSTEM_CURSOR_ARROW
cursorToSDL CursorHand = SDLEnum.SDL_SYSTEM_CURSOR_HAND
cursorToSDL CursorIBeam = SDLEnum.SDL_SYSTEM_CURSOR_IBEAM
cursorToSDL CursorInvalid = SDLEnum.SDL_SYSTEM_CURSOR_NO
cursorToSDL CursorSizeH = SDLEnum.SDL_SYSTEM_CURSOR_SIZEWE
cursorToSDL CursorSizeV = SDLEnum.SDL_SYSTEM_CURSOR_SIZENS
cursorToSDL CursorDiagTL = SDLEnum.SDL_SYSTEM_CURSOR_SIZENWSE
cursorToSDL CursorDiagTR = SDLEnum.SDL_SYSTEM_CURSOR_SIZENESW
