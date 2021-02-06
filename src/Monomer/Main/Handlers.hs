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
import Control.Lens ((&), (^.), (^?), (.~), (%~), (.=), (?=), _1, ix, at, use)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan)
import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Extra(concatMapM)
import Control.Monad.IO.Class
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq(..), (|>))
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
import Control.Monad.Trans.Maybe

type HandlerStep s e
  = (WidgetEnv s e, WidgetNode s e, Seq (WidgetRequest s), Seq e)

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
  reduceEvt currStep evt = do
    let (currWenv, currRoot, currReqs, currEvents) = currStep
    systemEvents <- addRelatedEvents currWenv mainBtn currRoot evt
    mainBtnPress <- use L.mainBtnPress
    inputStatus <- use L.inputStatus

    let newWenv = currWenv
          & L.mainBtnPress .~ mainBtnPress
          & L.inputStatus .~ inputStatus

    foldM reduceSysEvt (newWenv, currRoot, currReqs, currEvents) systemEvents
  reduceSysEvt (currWenv, currRoot, currReqs, currEvents) (evt, evtTarget) = do
    focused <- use L.focusedPath
    let trgt = fromMaybe focused evtTarget

    (wenv2, root2, reqs2, evts2) <- handleSystemEvent currWenv evt trgt currRoot

    return (wenv2, root2, currReqs <> reqs2, currEvents <> evts2)
  newEvents = preProcessEvents baseEvents
  nextStep = foldM reduceEvt (wenv, widgetRoot, Seq.empty, Seq.empty) newEvents

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
    Nothing -> return (wenv, widgetRoot, Seq.empty, Seq.empty)
    Just target -> do
      let widget = widgetRoot ^. L.widget
      let emptyResult = WidgetResult widgetRoot Seq.empty Seq.empty
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
  let WidgetResult evtRoot reqs events = result
  let resizeReq = isResizeResult (Just result)

  resizePending <- use L.resizePending
  step <- handleRequests reqs (wenv, evtRoot, reqs, events)

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
    GetClipboard wid -> handleGetClipboard wid step
    SetClipboard cdata -> handleSetClipboard cdata step
    StartTextInput rect -> handleStartTextInput rect step
    StopTextInput -> handleStopTextInput step
    SetOverlay wid path -> handleSetOverlay wid path step
    ResetOverlay wid -> handleResetOverlay wid step
    SetCursorIcon icon -> handleSetCursorIcon icon step
    StartDrag wid path info -> handleStartDrag wid path info step
    CancelDrag wid -> handleCancelDrag wid step
    RenderOnce -> handleRenderOnce step
    RenderEvery wid ms repeat -> handleRenderEvery wid ms repeat step
    RenderStop wid -> handleRenderStop wid step
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

  let (wenv, widgetRoot, requests, events) = previousStep
  let reqsNoResize = Seq.filter (not . isResizeWidgets) requests
  let tmpResult = resizeRoot wenv windowSize widgetRoot
  let newResult = tmpResult
        & L.requests .~ reqsNoResize <> tmpResult ^. L.requests
        & L.events .~ events <> tmpResult ^. L.events

  L.renderRequested .= True
  L.resizePending .= False

  handleWidgetResult wenv True newResult

handleMoveFocus
  :: (MonomerM s m)
  => Maybe Path
  -> FocusDirection
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleMoveFocus startFrom direction (wenv, root, reqs, evts) = do
  oldFocus <- use L.focusedPath
  let wenv0 = wenv { _weFocusedPath = emptyPath }
  (wenv1, root1, reqs1, evts1) <- handleSystemEvent wenv0 Blur oldFocus root
  currFocus <- use L.focusedPath
  currOverlay <- getOverlayPath

  if oldFocus == currFocus
    then do
      let searchFrom = fromMaybe currFocus startFrom
      let newFocus = findNextFocus wenv1 direction searchFrom currOverlay root1
      let tempWenv = wenv1 { _weFocusedPath = newFocus }

      L.focusedPath .= newFocus
      L.renderRequested .= True
      (wenv2, root2, reqs2, evts2) <- handleSystemEvent tempWenv Focus newFocus root1

      return (wenv2, root2, reqs <> reqs1 <> reqs2, evts <> evts1 <> evts2)
    else
      return (wenv1, root1, reqs1, evts1)

handleSetFocus
  :: (MonomerM s m) => Path -> HandlerStep s e -> m (HandlerStep s e)
handleSetFocus newFocus (wenv, root, reqs, evts) =  do
  let wenv0 = wenv { _weFocusedPath = newFocus }

  oldFocus <- use L.focusedPath

  if oldFocus /= newFocus
    then do
      (wenv1, root1, reqs1, evts1) <- handleSystemEvent wenv0 Blur oldFocus root

      L.focusedPath .= newFocus
      L.renderRequested .= True
      (wenv2, root2, reqs2, evts2) <- handleSystemEvent wenv1 Focus newFocus root1

      return (wenv2, root2, reqs <> reqs1 <> reqs2, evts <> evts1 <> evts2)
    else
      return (wenv, root, reqs, evts)

handleGetClipboard
  :: (MonomerM s m) => WidgetId -> HandlerStep s e -> m (HandlerStep s e)
handleGetClipboard widgetId (wenv, root, reqs, evts) = do
  path <- getWidgetIdPath widgetId
  hasText <- SDL.hasClipboardText
  contents <- fmap Clipboard $ if hasText
                then fmap ClipboardText SDL.getClipboardText
                else return ClipboardEmpty

  (wenv2, root2, reqs2, evts2) <- handleSystemEvent wenv contents path root
  return (wenv2, root2, reqs <> reqs2, evts <> evts2)

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

  when (isJust overlay) $ do
    delWidgetIdPath (fromJust overlay)

  L.overlayWidgetId .= Just widgetId
  addWidgetIdPath widgetId path
  return previousStep

handleResetOverlay
  :: (MonomerM s m) => WidgetId -> HandlerStep s e -> m (HandlerStep s e)
handleResetOverlay widgetId previousStep = do
  overlay <- use L.overlayWidgetId

  when (overlay == Just widgetId) $ do
    L.overlayWidgetId .= Nothing
    delWidgetIdPath widgetId

  return previousStep

handleSetCursorIcon
  :: (MonomerM s m) => CursorIcon -> HandlerStep s e -> m (HandlerStep s e)
handleSetCursorIcon icon previousStep = do
  L.currentCursor .= icon
  cursor <- (Map.! icon) <$> use L.cursorIcons
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

  when (isJust prevWidgetId) $ do
    delWidgetIdPath (fromJust prevWidgetId)

  L.dragAction .= Just (DragAction widgetId dragData)
  addWidgetIdPath widgetId path
  return previousStep

handleCancelDrag
  :: (MonomerM s m)
  => WidgetId
  -> HandlerStep s e
  -> m (HandlerStep s e)
handleCancelDrag widgetId previousStep = do
  oldDragAction <- use L.dragAction
  let prevWidgetId = fmap (^. L.widgetId) oldDragAction

  if prevWidgetId == Just widgetId
    then do
      L.renderRequested .= True
      L.dragAction .= Nothing
      delWidgetIdPath widgetId
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
      delWidgetIdPath (fromJust widgetId)
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
    (wenv, _, _, _) = previousStep
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
handleUpdateModel fn (wenv, root, reqs, evts) = do
  L.mainModel .= _weModel wenv2
  return (wenv2, root, reqs, evts)
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
handleSendMessage path message (wenv, root, reqs, evts) = do
  let emptyResult = WidgetResult root Seq.empty Seq.empty
  let widget = root ^. L.widget
  let msgResult = widgetHandleMessage widget wenv path message root
  let result = fromMaybe emptyResult msgResult

  (newWenv, newRoot, newReqs, newEvts) <- handleWidgetResult wenv True result

  return (newWenv, newRoot, reqs <> newReqs, evts <> newEvts)

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

    return $ dropEvts ++ clickEvt ++ dblClickEvt ++ releasedEvt ++ hoverEvts
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
  hover <- use L.hoveredPath
  mainBtnPress <- use L.mainBtnPress
  let startPath = fromMaybe emptyPath overlay
  let widget = widgetRoot ^. L.widget
  let wni = widgetFindByPoint widget wenv startPath point widgetRoot
  let target = fmap (^. L.path) wni
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
  -> Path
findNextFocus wenv direction focus overlay widgetRoot = fromJust nextFocus where
  widget = widgetRoot ^. L.widget
  restartPath = fromMaybe emptyPath overlay
  candidateWni = widgetFindNextFocus widget wenv direction focus widgetRoot
  candidateFocus = (^. L.path) <$> candidateWni
  fromRootWni = widgetFindNextFocus widget wenv direction restartPath widgetRoot
  fromRootFocus = (^. L.path) <$> fromRootWni
  nextFocus = candidateFocus <|> fromRootFocus <|> Just focus

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
