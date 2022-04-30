{-|
Module      : Monomer.Main.Core
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Core glue for running an application.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Monomer.Main.Core (
  AppEventResponse(..),
  AppEventHandler(..),
  AppUIBuilder(..),
  startApp
) where

import Control.Concurrent
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)
import Control.Exception
import Control.Lens ((&), (^.), (.=), (.~), use)
import Control.Monad (unless, void, when)
import Control.Monad.Extra
import Control.Monad.State
import Control.Monad.STM (atomically)
import Data.Default
import Data.Maybe
import Data.Map (Map)
import Data.List (foldl')
import Data.Text (Text)
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Graphics.GL

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified SDL
import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.Main.Handlers
import Monomer.Main.Platform
import Monomer.Main.Types
import Monomer.Main.Util
import Monomer.Main.WidgetTask
import Monomer.Graphics
import Monomer.Helper (catchAny)
import Monomer.Widgets.Composite

import qualified Monomer.Lens as L

{-|
Type of response an App event handler can return, with __s__ being the model and
__e__ the user's event type.
-}
type AppEventResponse s e = EventResponse s e s ()

-- | Type of an App event handler.
type AppEventHandler s e
  = WidgetEnv s e            -- ^ The widget environment.
  -> WidgetNode s e          -- ^ The root node of the application.
  -> s                       -- ^ The application's model.
  -> e                       -- ^ The event to handle.
  -> [AppEventResponse s e]  -- ^ The list of requested actions.

-- | Type of the function responsible of creating the App UI.
type AppUIBuilder s e = UIBuilder s e

data MainLoopArgs sp e ep = MainLoopArgs {
  _mlOS :: Text,
  _mlTheme :: Theme,
  _mlAppStartTs :: Millisecond,
  _mlMaxFps :: Int,
  _mlLatestRenderTs :: Millisecond,
  _mlFrameStartTs :: Millisecond,
  _mlFrameAccumTs :: Millisecond,
  _mlFrameCount :: Int,
  _mlExitEvents :: [e],
  _mlWidgetRoot :: WidgetNode sp ep,
  _mlWidgetShared :: MVar (Map Text WidgetShared)
}

data RenderState s e = RenderState {
  _rstDpr :: Double,
  _rstWidgetEnv :: WidgetEnv s e,
  _rstRootNode :: WidgetNode s e
}

{-|
Runs an application, creating the UI with the provided function and initial
model, handling future events with the event handler.

Control will not be returned until the UI exits. This needs to be ran in the
main thread if using macOS.
-}
startApp
  :: (Eq s, WidgetModel s, WidgetEvent e)
  => s                    -- ^ The initial model.
  -> AppEventHandler s e  -- ^ The event handler.
  -> AppUIBuilder s e     -- ^ The UI builder.
  -> [AppConfig e]        -- ^ The application config.
  -> IO ()                -- ^ The application action.
startApp model eventHandler uiBuilder configs = do
  (window, dpr, epr, glCtx) <- initSDLWindow config
  vpSize <- getViewportSize window dpr
  channel <- newTChanIO

  let monomerCtx = initMonomerCtx window channel vpSize dpr epr model

  runStateT (runAppLoop window glCtx channel appWidget config) monomerCtx
  detroySDLWindow window
  where
    config = mconcat configs
    compCfgs
      = (onInit <$> _apcInitEvent config)
      ++ (onDispose <$> _apcDisposeEvent config)
      ++ (onResize <$> _apcResizeEvent config)
    appWidget = composite_ "app" id uiBuilder eventHandler compCfgs

runAppLoop
  :: (MonomerM sp ep m, Eq sp, WidgetEvent e, WidgetEvent ep)
  => SDL.Window
  -> SDL.GLContext
  -> TChan (RenderMsg sp ep)
  -> WidgetNode sp ep
  -> AppConfig e
  -> m ()
runAppLoop window glCtx channel widgetRoot config = do
  dpr <- use L.dpr
  winSize <- use L.windowSize

  let useRenderThreadFlag = fromMaybe True (_apcUseRenderThread config)
  let useRenderThread = useRenderThreadFlag && rtsSupportsBoundThreads
  let maxFps = fromMaybe 60 (_apcMaxFps config)
  let fonts = _apcFonts config
  let theme = fromMaybe def (_apcTheme config)
  let exitEvents = _apcExitEvent config
  let mainBtn = fromMaybe BtnLeft (_apcMainButton config)
  let contextBtn = fromMaybe BtnRight (_apcContextButton config)

  appStartTs <- getCurrentTimestamp
  model <- use L.mainModel
  os <- liftIO getPlatform
  widgetSharedMVar <- liftIO $ newMVar Map.empty
  fontManager <- liftIO $ makeFontManager fonts dpr

  let wenv = WidgetEnv {
    _weOs = os,
    _weDpr = dpr,
    _weAppStartTs = appStartTs,
    _weFontManager = fontManager,
    _weFindBranchByPath = const Seq.empty,
    _weMainButton = mainBtn,
    _weContextButton = contextBtn,
    _weTheme = theme,
    _weWindowSize = winSize,
    _weWidgetShared = widgetSharedMVar,
    _weWidgetKeyMap = Map.empty,
    _weCursor = Nothing,
    _weHoveredPath = Nothing,
    _weFocusedPath = emptyPath,
    _weOverlayPath = Nothing,
    _weDragStatus = Nothing,
    _weMainBtnPress = Nothing,
    _weModel = model,
    _weInputStatus = def,
    _weTimestamp = 0,
    _weThemeChanged = False,
    _weInTopLayer = const True,
    _weLayoutDirection = LayoutNone,
    _weViewport = Rect 0 0 (winSize ^. L.w) (winSize ^. L.h),
    _weOffset = def
  }
  let pathReadyRoot = widgetRoot
        & L.info . L.path .~ rootPath
        & L.info . L.widgetId .~ WidgetId (wenv ^. L.timestamp) rootPath
  let makeMainThreadRenderer = do
        renderer <- liftIO $ makeRenderer fonts dpr
        L.renderMethod .= Left renderer
        return RenderSetupSingle

  setupRes <- if useRenderThread
    then do
      stpChan <- liftIO newTChanIO

      liftIO . void . forkOS $
        {-
        The wenv and widgetRoot values are not used, since they are replaced
        during MsgInit. Kept to avoid issues with the Strict pragma.
        -}
        startRenderThread stpChan channel window glCtx fonts dpr wenv widgetRoot

      setupRes <- liftIO . atomically $ readTChan stpChan

      case setupRes of
        RenderSetupMakeCurrentFailed msg -> do
          liftIO . putStrLn $ "Setup of the rendering thread failed: " ++ msg
          liftIO . putStrLn $ "Falling back to rendering in the main thread. "
            ++ "The content may not be updated while resizing the window."

          makeMainThreadRenderer
        _ -> do
          return RenderSetupMulti
    else do
      makeMainThreadRenderer

  handleResourcesInit
  (newWenv, newRoot, _) <- handleWidgetInit wenv pathReadyRoot

  {-
  Deferred initialization step to account for Widgets that rely on OpenGL. They
  need the Renderer to be setup before handleWidgetInit is called, and it is
  safer to initialize the watcher after this happens.
  -}
  case setupRes of
    RenderSetupMulti -> do
      liftIO . atomically $ writeTChan channel (MsgInit newWenv newRoot)
      liftIO $ watchWindowResize channel
    _ -> return ()

  let loopArgs = MainLoopArgs {
    _mlOS = os,
    _mlTheme = theme,
    _mlMaxFps = maxFps,
    _mlAppStartTs = appStartTs,
    _mlLatestRenderTs = 0,
    _mlFrameStartTs = 0,
    _mlFrameAccumTs = 0,
    _mlFrameCount = 0,
    _mlExitEvents = exitEvents,
    _mlWidgetRoot = newRoot,
    _mlWidgetShared = widgetSharedMVar
  }

  L.mainModel .= _weModel newWenv

  mainLoop window fontManager config loopArgs

mainLoop
  :: (MonomerM sp ep m, WidgetEvent e)
  => SDL.Window
  -> FontManager
  -> AppConfig e
  -> MainLoopArgs sp e ep
  -> m ()
mainLoop window fontManager config loopArgs = do
  let MainLoopArgs{..} = loopArgs

  startTs <- getEllapsedTimestampSince _mlAppStartTs
  events <- SDL.pumpEvents >> SDL.pollEvents

  windowSize <- use L.windowSize
  dpr <- use L.dpr
  epr <- use L.epr
  currentModel <- use L.mainModel
  cursorIcon <- getCurrentCursorIcon
  hovered <- getHoveredPath
  focused <- getFocusedPath
  overlay <- getOverlayPath
  dragged <- getDraggedMsgInfo
  mainPress <- use L.mainBtnPress
  inputStatus <- use L.inputStatus
  mousePos <- liftIO $ getCurrentMousePos epr
  currWinSize <- liftIO $ getViewportSize window dpr

  let Size rw rh = windowSize
  let ts = startTs - _mlFrameStartTs
  let eventsPayload = fmap SDL.eventPayload events
  let quit = SDL.QuitEvent `elem` eventsPayload

  let windowResized = currWinSize /= windowSize && isWindowResized eventsPayload
  let windowExposed = isWindowExposed eventsPayload
  let mouseEntered = isMouseEntered eventsPayload
  let invertX = fromMaybe False (_apcInvertWheelX config)
  let invertY = fromMaybe False (_apcInvertWheelY config)
  let convertCfg = ConvertEventsCfg _mlOS dpr epr invertX invertY
  let baseSystemEvents = convertEvents convertCfg mousePos eventsPayload

--  when newSecond $
--    liftIO . putStrLn $ "Frames: " ++ show _mlFrameCount

  when quit $
    L.exitApplication .= True

  when windowExposed $
    L.mainBtnPress .= Nothing

  let newSecond = _mlFrameAccumTs > 1000
  let mainBtn = fromMaybe BtnLeft (_apcMainButton config)
  let contextBtn = fromMaybe BtnRight (_apcContextButton config)
  let wenv = WidgetEnv {
    _weOs = _mlOS,
    _weDpr = dpr,
    _weAppStartTs = _mlAppStartTs,
    _weFontManager = fontManager,
    _weFindBranchByPath = findChildBranchByPath wenv _mlWidgetRoot,
    _weMainButton = mainBtn,
    _weContextButton = contextBtn,
    _weTheme = _mlTheme,
    _weWindowSize = windowSize,
    _weWidgetShared = _mlWidgetShared,
    _weWidgetKeyMap = Map.empty,
    _weCursor = cursorIcon,
    _weHoveredPath = hovered,
    _weFocusedPath = focused,
    _weOverlayPath = overlay,
    _weDragStatus = dragged,
    _weMainBtnPress = mainPress,
    _weModel = currentModel,
    _weInputStatus = inputStatus,
    _weTimestamp = startTs,
    _weThemeChanged = False,
    _weInTopLayer = const True,
    _weLayoutDirection = LayoutNone,
    _weViewport = Rect 0 0 rw rh,
    _weOffset = def
  }
  -- Exit handler
  let baseWidgetId = _mlWidgetRoot ^. L.info . L.widgetId
  let exitMsgs = SendMessage baseWidgetId <$> _mlExitEvents
  let baseReqs
        | quit = Seq.fromList exitMsgs
        | otherwise = Seq.Empty
  let baseStep = (wenv, _mlWidgetRoot, Seq.empty)

  (rqWenv, rqRoot, _) <- handleRequests baseReqs baseStep
  (wtWenv, wtRoot, _) <- handleWidgetTasks rqWenv rqRoot
  (seWenv, seRoot, _) <- handleSystemEvents wtWenv wtRoot baseSystemEvents

  (newWenv, newRoot, _) <- if windowResized
    then do
      L.windowSize .= currWinSize
      handleResizeWidgets (seWenv, seRoot, Seq.empty)
    else return (seWenv, seRoot, Seq.empty)

  endTs <- getEllapsedTimestampSince _mlAppStartTs

  -- Rendering
  renderCurrentReq <- checkRenderCurrent startTs _mlLatestRenderTs

  let renderEvent = any isActionEvent eventsPayload
  let winRedrawEvt = windowResized || windowExposed
  let renderNeeded = winRedrawEvt || renderEvent || renderCurrentReq

  when renderNeeded $ do
    renderMethod <- use L.renderMethod

    case renderMethod of
      Right renderChan -> do
        liftIO . atomically $ writeTChan renderChan (MsgRender newWenv newRoot)
      Left renderer -> do
        let bgColor = newWenv ^. L.theme . L.clearColor

        liftIO $ renderWidgets window dpr renderer bgColor newWenv newRoot

  -- Used in the next rendering cycle
  L.renderRequested .= windowResized

  let fps = realToFrac _mlMaxFps
  let frameLength = round (1000000 / fps)
  let remainingMs = endTs - startTs
  let tempDelay = abs (frameLength - fromIntegral remainingMs * 1000)
  let nextFrameDelay = min frameLength tempDelay
  let latestRenderTs = if renderNeeded then startTs else _mlLatestRenderTs
  let newLoopArgs = loopArgs {
    _mlLatestRenderTs = latestRenderTs,
    _mlFrameStartTs = startTs,
    _mlFrameAccumTs = if newSecond then 0 else _mlFrameAccumTs + ts,
    _mlFrameCount = if newSecond then 0 else _mlFrameCount + 1,
    _mlWidgetRoot = newRoot
  }

  liftIO $ threadDelay nextFrameDelay

  shouldQuit <- use L.exitApplication

  when shouldQuit $
    void $ handleWidgetDispose newWenv newRoot

  unless shouldQuit (mainLoop window fontManager config newLoopArgs)

{-
Attempts to initialize a GL context in a separate OS thread to handle rendering
actions. This allows for continuous content updates when the user resizes the
window.

In case the setup fails, it notifies the parent process so it can fall back to
rendering in the main thread.
-}
startRenderThread
  :: (Eq s, WidgetEvent e)
  => TChan RenderSetupResult
  -> TChan (RenderMsg s e)
  -> SDL.Window
  -> SDL.GLContext
  -> [FontDef]
  -> Double
  -> WidgetEnv s e
  -> WidgetNode s e
  -> IO ()
startRenderThread setupChan msgChan window glCtx fonts dpr wenv root = do
  resp <- try $ SDL.glMakeCurrent window glCtx

  case resp of
    Right{} -> do
      renderer <- liftIO $ makeRenderer fonts dpr
      fontMgr <- liftIO $ makeFontManager fonts dpr

      atomically $ writeTChan setupChan RenderSetupMulti

      waitRenderMsg msgChan window renderer fontMgr state
    Left (SDL.SDLCallFailed _ _ err) -> do
      let msg = T.unpack err
      atomically $ writeTChan setupChan (RenderSetupMakeCurrentFailed msg)
    Left e -> do
      let msg = displayException e
      atomically $ writeTChan setupChan (RenderSetupMakeCurrentFailed msg)
  where
    state = RenderState dpr wenv root

waitRenderMsg
  :: (Eq s, WidgetEvent e)
  => TChan (RenderMsg s e)
  -> SDL.Window
  -> Renderer
  -> FontManager
  -> RenderState s e
  -> IO ()
waitRenderMsg msgChan window renderer fontMgr state = do
  msg <- atomically $ readTChan msgChan
  newState <- handleRenderMsg window renderer fontMgr state msg
  waitRenderMsg msgChan window renderer fontMgr newState

handleRenderMsg
  :: (Eq s, WidgetEvent e)
  => SDL.Window
  -> Renderer
  -> FontManager
  -> RenderState s e
  -> RenderMsg s e
  -> IO (RenderState s e)
handleRenderMsg window renderer fontMgr state (MsgInit newWenv newRoot) = do
  let RenderState dpr _ _ = state
  return (RenderState dpr newWenv newRoot)
handleRenderMsg window renderer fontMgr state (MsgRender tmpWenv newRoot) = do
  let RenderState dpr _ _ = state
  let newWenv = tmpWenv
        & L.fontManager .~ fontMgr
  let color = newWenv ^. L.theme . L.clearColor
  renderWidgets window dpr renderer color newWenv newRoot
  return (RenderState dpr newWenv newRoot)
handleRenderMsg window renderer fontMgr state (MsgResize _) = do
  newSize <- getViewportSize window (_rstDpr state)

  let RenderState dpr wenv root = state
  let viewport = Rect 0 0 (newSize ^. L.w) (newSize ^. L.h)
  let newWenv = wenv
        & L.fontManager .~ fontMgr
        & L.windowSize .~ newSize
        & L.viewport .~ viewport
  let color = newWenv ^. L.theme . L.clearColor
  let resizeCheck = const False
  let result = widgetResize (root ^. L.widget) newWenv root viewport resizeCheck
  let newRoot = result ^. L.node

  renderWidgets window dpr renderer color newWenv newRoot
  return state
handleRenderMsg window renderer fontMgr state (MsgRemoveImage name) = do
  deleteImage renderer name
  return state
handleRenderMsg window renderer fontMgr state (MsgRunInRender chan task) = do
  flip catchAny print $ do
    value <- task
    atomically $ writeTChan chan value
  return state

renderWidgets
  :: SDL.Window
  -> Double
  -> Renderer
  -> Color
  -> WidgetEnv s e
  -> WidgetNode s e
  -> IO ()
renderWidgets window dpr renderer clearColor wenv widgetRoot = do
  Size dwW dwH <- getDrawableSize window
  Size vpW vpH <- getViewportSize window dpr

  glViewport 0 0 (round dwW) (round dwH)

  glClearColor r g b a
  glClear GL_COLOR_BUFFER_BIT

  beginFrame renderer vpW vpH
  widgetRender (widgetRoot ^. L.widget) wenv widgetRoot renderer
  endFrame renderer

  renderRawTasks renderer

  beginFrame renderer vpW vpH
  renderOverlays renderer
  endFrame renderer

  renderRawOverlays renderer

  SDL.glSwapWindow window
  where
    r = fromIntegral (clearColor ^. L.r) / 255
    g = fromIntegral (clearColor ^. L.g) / 255
    b = fromIntegral (clearColor ^. L.b) / 255
    a = realToFrac (clearColor ^. L.a)

watchWindowResize :: TChan (RenderMsg s e) -> IO ()
watchWindowResize channel = do
  void . SDL.addEventWatch $ \ev -> do
    case SDL.eventPayload ev of
      SDL.WindowSizeChangedEvent sizeChangeData -> do
        let SDL.V2 nw nh = SDL.windowSizeChangedEventSize sizeChangeData
        let newSize = Size (fromIntegral nw) (fromIntegral nh)

        atomically $ writeTChan channel (MsgResize newSize)
      _ -> return ()

checkRenderCurrent :: (MonomerM s e m) => Millisecond -> Millisecond -> m Bool
checkRenderCurrent currTs renderTs = do
  renderCurrent <- use L.renderRequested
  schedule <- use L.renderSchedule
  L.renderSchedule .= Map.filter (renderScheduleActive currTs) schedule
  return (renderCurrent || renderNext schedule)
  where
    requiresRender = renderScheduleReq currTs renderTs
    renderNext schedule = any requiresRender schedule

renderScheduleReq :: Millisecond -> Millisecond -> RenderSchedule -> Bool
renderScheduleReq currTs renderTs schedule = required where
  RenderSchedule _ start ms _ = schedule
  stepCount = floor (fromIntegral (currTs - start) / fromIntegral ms)
  stepTs = start + ms * stepCount
  required = renderTs < stepTs

renderScheduleActive :: Millisecond -> RenderSchedule -> Bool
renderScheduleActive currTs schedule = scheduleActive where
  RenderSchedule _ start ms count = schedule
  stepCount = floor (fromIntegral (currTs - start) / fromIntegral ms)
  scheduleActive = maybe True (> stepCount) count

isWindowResized :: [SDL.EventPayload] -> Bool
isWindowResized eventsPayload = not status where
  status = null [ e | e@SDL.WindowResizedEvent {} <- eventsPayload ]

isWindowExposed :: [SDL.EventPayload] -> Bool
isWindowExposed eventsPayload = not status where
  status = null [ e | e@SDL.WindowExposedEvent {} <- eventsPayload ]

isMouseEntered :: [SDL.EventPayload] -> Bool
isMouseEntered eventsPayload = not status where
  status = null [ e | e@SDL.WindowGainedMouseFocusEvent {} <- eventsPayload ]

getCurrentTimestamp :: MonadIO m => m Millisecond
getCurrentTimestamp = toMs <$> liftIO getCurrentTime
  where
    toMs = floor . (1e3 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

getEllapsedTimestampSince :: MonadIO m => Millisecond -> m Millisecond
getEllapsedTimestampSince start = do
  ts <- getCurrentTimestamp
  return (ts - start)
