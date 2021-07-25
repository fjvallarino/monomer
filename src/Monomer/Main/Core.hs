{-|
Module      : Monomer.Main.Core
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Core glue for running an application.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Main.Core (
  AppEventResponse(..),
  AppEventHandler(..),
  AppUIBuilder(..),
  startApp
) where

import Control.Concurrent (MVar, forkIO, forkOS, newMVar, threadDelay)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)
import Control.Lens ((&), (^.), (.=), (.~), use)
import Control.Monad (unless, void, when)
import Control.Monad.Catch
import Control.Monad.Extra
import Control.Monad.State
import Control.Monad.STM (atomically)
import Data.Default
import Data.Maybe
import Data.Map (Map)
import Data.List (foldl')
import Data.Text (Text)

import qualified Data.Map as Map
import qualified Graphics.Rendering.OpenGL as GL
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
import Monomer.Widgets.Composite

import qualified Monomer.Lens as L

-- | Type of response an App event handler can return, with __s__ being the
-- | model and __e__ the user's event type.
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
  _mlAppStartTs :: Int,
  _mlMaxFps :: Int,
  _mlLatestRenderTs :: Int,
  _mlFrameStartTs :: Int,
  _mlFrameAccumTs :: Int,
  _mlFrameCount :: Int,
  _mlExitEvents :: [e],
  _mlWidgetRoot :: WidgetNode sp ep,
  _mlWidgetShared :: MVar (Map Text WidgetShared),
  _mlChannel :: TChan (RenderMsg sp ep)
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
  winSize <- getWindowSize window
  channel <- newTChanIO

  let monomerCtx = initMonomerCtx window channel winSize dpr epr model

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

  let maxFps = fromMaybe 60 (_apcMaxFps config)
  let fonts = _apcFonts config
  let theme = fromMaybe def (_apcTheme config)
  let exitEvents = _apcExitEvent config
  let mainBtn = fromMaybe BtnLeft (_apcMainButton config)
  let contextBtn = fromMaybe BtnRight (_apcContextButton config)

  startTs <- fmap fromIntegral SDL.ticks
  model <- use L.mainModel
  os <- getPlatform
  widgetSharedMVar <- liftIO $ newMVar Map.empty
  fontManager <- liftIO $ makeFontManager fonts dpr

  let wenv = WidgetEnv {
    _weOs = os,
    _weFontManager = fontManager,
    _weFindByPath = const Nothing,
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
    _weTimestamp = startTs,
    _weThemeChanged = False,
    _weInTopLayer = const True,
    _weLayoutDirection = LayoutNone,
    _weViewport = Rect 0 0 (winSize ^. L.w) (winSize ^. L.h),
    _weOffset = def
  }
  let pathReadyRoot = widgetRoot
        & L.info . L.path .~ rootPath
        & L.info . L.widgetId .~ WidgetId (wenv ^. L.timestamp) rootPath

  handleResourcesInit
  (newWenv, newRoot, _) <- handleWidgetInit wenv pathReadyRoot

  let loopArgs = MainLoopArgs {
    _mlOS = os,
    _mlTheme = theme,
    _mlMaxFps = maxFps,
    _mlAppStartTs = 0,
    _mlLatestRenderTs = 0,
    _mlFrameStartTs = startTs,
    _mlFrameAccumTs = 0,
    _mlFrameCount = 0,
    _mlExitEvents = exitEvents,
    _mlWidgetRoot = newRoot,
    _mlWidgetShared = widgetSharedMVar,
    _mlChannel = channel
  }

  L.mainModel .= _weModel newWenv

  liftIO . forkOS . void $
    startRenderThread channel window glCtx fonts dpr newWenv newRoot

  liftIO $ watchWindowResize channel
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

  startTicks <- fmap fromIntegral SDL.ticks
  events <- SDL.pollEvents

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
  mousePos <- getCurrentMousePos epr
  currWinSize <- getWindowSize window

  let Size rw rh = windowSize
  let ts = startTicks - _mlFrameStartTs
  let eventsPayload = fmap SDL.eventPayload events
  let quit = SDL.QuitEvent `elem` eventsPayload

  let windowResized = currWinSize /= windowSize && isWindowResized eventsPayload
  let windowExposed = isWindowExposed eventsPayload
  let mouseEntered = isMouseEntered eventsPayload
  let baseSystemEvents = convertEvents dpr epr mousePos eventsPayload

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
    _weFontManager = fontManager,
    _weFindByPath = findWidgetByPath wenv _mlWidgetRoot,
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
    _weTimestamp = startTicks,
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

  endTicks <- fmap fromIntegral SDL.ticks

  -- Rendering
  renderCurrentReq <- checkRenderCurrent startTicks _mlFrameStartTs

  let renderEvent = any isActionEvent eventsPayload
  let winRedrawEvt = windowResized || windowExposed
  let renderNeeded = winRedrawEvt || renderEvent || renderCurrentReq

  when renderNeeded $
    liftIO . atomically $ writeTChan _mlChannel (MsgRender newWenv newRoot)

  L.renderRequested .= windowResized

  let fps = realToFrac _mlMaxFps
  let frameLength = round (1000000 / fps)
  let remainingMs = endTicks - startTicks
  let tempDelay = abs (frameLength - remainingMs * 1000)
  let nextFrameDelay = min frameLength tempDelay
  let latestRenderTs = if renderNeeded then startTicks else _mlLatestRenderTs
  let newLoopArgs = loopArgs {
    _mlAppStartTs = _mlAppStartTs + ts,
    _mlLatestRenderTs = latestRenderTs,
    _mlFrameStartTs = startTicks,
    _mlFrameAccumTs = if newSecond then 0 else _mlFrameAccumTs + ts,
    _mlFrameCount = if newSecond then 0 else _mlFrameCount + 1,
    _mlWidgetRoot = newRoot
  }

  liftIO $ threadDelay nextFrameDelay

  shouldQuit <- use L.exitApplication

  when shouldQuit $
    void $ handleWidgetDispose newWenv newRoot

  unless shouldQuit (mainLoop window fontManager config newLoopArgs)

startRenderThread
  :: (Eq s, WidgetEvent e)
  => TChan (RenderMsg s e)
  -> SDL.Window
  -> SDL.GLContext
  -> [FontDef]
  -> Double
  -> WidgetEnv s e
  -> WidgetNode s e
  -> IO ()
startRenderThread channel window glCtx fonts dpr wenv root = do
  SDL.glMakeCurrent window glCtx
  renderer <- liftIO $ makeRenderer fonts dpr
  fontMgr <- liftIO $ makeFontManager fonts dpr

  waitRenderMsg channel window renderer fontMgr state
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
waitRenderMsg channel window renderer fontMgr state = do
  msg <- liftIO . atomically $ readTChan channel
  newState <- handleRenderMsg window renderer fontMgr state msg
  waitRenderMsg channel window renderer fontMgr newState

handleRenderMsg
  :: (Eq s, WidgetEvent e)
  => SDL.Window
  -> Renderer
  -> FontManager
  -> RenderState s e
  -> RenderMsg s e
  -> IO (RenderState s e)
handleRenderMsg window renderer fontMgr state (MsgRender tmpWenv newRoot) = do
  let RenderState dpr _ _ = state
  let newWenv = tmpWenv
        & L.fontManager .~ fontMgr
  let color = newWenv ^. L.theme . L.clearColor
  renderWidgets window renderer color newWenv newRoot
  return (RenderState dpr newWenv newRoot)
handleRenderMsg window renderer fontMgr state (MsgResize newSize) = do
  let RenderState dpr wenv root = state
  let viewport = Rect 0 0 (newSize ^. L.w) (newSize ^. L.h)
  let newWenv = wenv
        & L.fontManager .~ fontMgr
        & L.windowSize .~ newSize
        & L.viewport .~ viewport
  let color = newWenv ^. L.theme . L.clearColor
  let result = widgetResize (root ^. L.widget) newWenv root viewport
  let newRoot = result ^. L.node

  renderWidgets window renderer color newWenv newRoot
  return state
handleRenderMsg window renderer fontMgr state (MsgRemoveImage name) = do
  deleteImage renderer name
  return state

renderWidgets
  :: SDL.Window
  -> Renderer
  -> Color
  -> WidgetEnv s e
  -> WidgetNode s e
  -> IO ()
renderWidgets !window renderer clearColor wenv widgetRoot = do
  Size winW winH <- getWindowSize window

  liftIO $ GL.clearColor GL.$= clearColor4
  liftIO $ GL.clear [GL.ColorBuffer]

  liftIO $ beginFrame renderer winW winH
  liftIO $ widgetRender (widgetRoot ^. L.widget) wenv widgetRoot renderer
  liftIO $ endFrame renderer

  liftIO $ renderRawTasks renderer

  liftIO $ beginFrame renderer winW winH
  liftIO $ renderOverlays renderer
  liftIO $ endFrame renderer

  liftIO $ renderRawOverlays renderer

  SDL.glSwapWindow window
  where
    r = fromIntegral (clearColor ^. L.r) / 255
    g = fromIntegral (clearColor ^. L.g) / 255
    b = fromIntegral (clearColor ^. L.b) / 255
    a = clearColor ^. L.a
    clearColor4 = GL.Color4 r g b (realToFrac a)

watchWindowResize :: TChan (RenderMsg s e) -> IO ()
watchWindowResize channel = do
  void . SDL.addEventWatch $ \ev -> do
    case SDL.eventPayload ev of
      SDL.WindowSizeChangedEvent sizeChangeData -> do
        let SDL.V2 nw nh = SDL.windowSizeChangedEventSize sizeChangeData
        let newSize = Size (fromIntegral nw) (fromIntegral nh)

        atomically $ writeTChan channel (MsgResize newSize)
      _ -> return ()

checkRenderCurrent :: (MonomerM s e m) => Int -> Int -> m Bool
checkRenderCurrent currTs renderTs = do
  renderNext <- use L.renderRequested
  schedule <- use L.renderSchedule
  L.renderSchedule .= Map.filter (renderScheduleActive currTs renderTs) schedule
  return (renderNext || nextRender schedule)
  where
    foldHelper acc curr = acc || renderScheduleReq currTs renderTs curr
    nextRender schedule = foldl' foldHelper False schedule

renderScheduleReq :: Int -> Int -> RenderSchedule -> Bool
renderScheduleReq currTs renderTs schedule = nextStep < currTs where
  RenderSchedule _ start ms _ = schedule
  stepsDone = floor (fromIntegral (renderTs - start) / fromIntegral ms)
  currStep = start + ms * stepsDone
  nextStep
    | currStep >= renderTs = currStep
    | otherwise = currStep + ms

renderScheduleActive :: Int -> Int -> RenderSchedule -> Bool
renderScheduleActive currTs renderTs schedule = scheduleActive where
  RenderSchedule _ start ms count = schedule
  stepsDone = floor (fromIntegral (renderTs - start) / fromIntegral ms)
  scheduleActive = maybe True (> stepsDone) count

isWindowResized :: [SDL.EventPayload] -> Bool
isWindowResized eventsPayload = not status where
  status = null [ e | e@SDL.WindowResizedEvent {} <- eventsPayload ]

isWindowExposed :: [SDL.EventPayload] -> Bool
isWindowExposed eventsPayload = not status where
  status = null [ e | e@SDL.WindowExposedEvent {} <- eventsPayload ]

isMouseEntered :: [SDL.EventPayload] -> Bool
isMouseEntered eventsPayload = not status where
  status = null [ e | e@SDL.WindowGainedMouseFocusEvent {} <- eventsPayload ]
