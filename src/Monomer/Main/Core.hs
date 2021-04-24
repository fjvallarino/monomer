{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Main.Core (
  AppEventResponse,
  AppEventHandler,
  AppUIBuilder,
  EventResponse(..),
  simpleApp,
  runApp
) where

import Control.Concurrent (threadDelay)
import Control.Lens ((&), (^.), (.=), (.~), use)
import Control.Monad.Catch
import Control.Monad.Extra
import Control.Monad.State
import Data.Default
import Data.Maybe
import Data.List (foldl')
import Data.Text (Text)
import Safe

import qualified Data.Map as Map
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL
import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
--import Monomer.Lens
import Monomer.Main.Handlers
import Monomer.Main.Platform
import Monomer.Main.Types
import Monomer.Main.Util
import Monomer.Main.WidgetTask
import Monomer.Graphics
import Monomer.Widgets.Composite

import qualified Monomer.Lens as L

type AppEventResponse s e = EventResponse s e ()
type AppEventHandler s e
  = WidgetEnv s e -> WidgetNode s e -> s -> e -> [AppEventResponse s e]
type AppUIBuilder s e = UIBuilder s e

data MainLoopArgs s e ep = MainLoopArgs {
  _mlOS :: Text,
  _mlTheme :: Theme,
  _mlAppStartTs :: Int,
  _mlMaxFps :: Int,
  _mlLatestRenderTs :: Int,
  _mlFrameStartTs :: Int,
  _mlFrameAccumTs :: Int,
  _mlFrameCount :: Int,
  _mlExitEvents :: [e],
  _mlWidgetRoot :: WidgetNode s ep
}

simpleApp
  :: (Eq s, WidgetModel s, WidgetEvent e)
  => s
  -> AppEventHandler s e
  -> AppUIBuilder s e
  -> [AppConfig e]
  -> IO ()
simpleApp model eventHandler uiBuilder configs = do
  (window, dpr) <- initSDLWindow config
  winSize <- getDrawableSize window

  let monomerCtx = initMonomerCtx model window winSize useHdpi dpr

  runStateT (runApp window appWidget config) monomerCtx
  detroySDLWindow window
  where
    config = mconcat configs
    useHdpi = fromMaybe defaultUseHdpi (_apcHdpi config)
    compCfgs
      = (onInit <$> _apcInitEvent config)
      ++ (onDispose <$> _apcDisposeEvent config)
      ++ (onResize <$> _apcResizeEvent config)
    appWidget = composite_ "app" id uiBuilder eventHandler compCfgs

runApp
  :: (MonomerM s m, WidgetEvent e)
  => SDL.Window
  -> WidgetNode s ep
  -> AppConfig e
  -> m ()
runApp window widgetRoot config = do
  useHiDPI <- use L.hdpi
  devicePixelRate <- use L.dpr
  Size rw rh <- use L.windowSize

  let dpr = if useHiDPI then devicePixelRate else 1
  let newWindowSize = Size (rw / dpr) (rh / dpr)
  let maxFps = fromMaybe 60 (_apcMaxFps config)
  let fonts = _apcFonts config
  let theme = fromMaybe def (_apcTheme config)
  let exitEvents = _apcExitEvent config
  let mainBtn = fromMaybe LeftBtn (_apcMainButton config)

  resizeWindow window
  startTs <- fmap fromIntegral SDL.ticks
  model <- use L.mainModel
  os <- getPlatform
  renderer <- liftIO $ makeRenderer fonts dpr
  -- Hack, otherwise glyph positions are invalid until nanovg is initialized
  liftIO $ beginFrame renderer (round rw) (round rh)
  liftIO $ endFrame renderer

  let wenv = WidgetEnv {
    _weOS = os,
    _weRenderer = renderer,
    _weFindByPath = const Nothing,
    _weMainButton = mainBtn,
    _weTheme = theme,
    _weWindowSize = newWindowSize,
    _weGlobalKeys = Map.empty,
    _weCursor = Nothing,
    _weHoveredPath = Nothing,
    _weFocusedPath = emptyPath,
    _weOverlayPath = Nothing,
    _weDragStatus = Nothing,
    _weMainBtnPress = Nothing,
    _weModel = model,
    _weInputStatus = def,
    _weTimestamp = startTs,
    _weInTopLayer = const True,
    _weLayoutDirection = LayoutNone,
    _weViewport = Rect 0 0 rw rh,
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
    _mlWidgetRoot = newRoot
  }

  L.mainModel .= _weModel newWenv

  mainLoop window renderer config loopArgs

mainLoop
  :: (MonomerM s m, WidgetEvent e)
  => SDL.Window
  -> Renderer
  -> AppConfig e
  -> MainLoopArgs s e ep
  -> m ()
mainLoop window renderer config loopArgs = do
  startTicks <- fmap fromIntegral SDL.ticks
  events <- SDL.pollEvents
  mousePos <- getCurrentMousePos

  windowSize <- use L.windowSize
  useHiDPI <- use L.hdpi
  devicePixelRate <- use L.dpr
  currentModel <- use L.mainModel
  currCursor <- getCurrentCursor
  hovered <- getHoveredPath
  focused <- getFocusedPath
  overlay <- getOverlayPath
  dragged <- getDraggedMsgInfo
  mainPress <- use L.mainBtnPress
  inputStatus <- use L.inputStatus

  let MainLoopArgs{..} = loopArgs
  let Size rw rh = windowSize
  let ts = startTicks - _mlFrameStartTs
  let eventsPayload = fmap SDL.eventPayload events
  let quit = SDL.QuitEvent `elem` eventsPayload

  let windowResized = isWindowResized eventsPayload
  let windowExposed = isWindowExposed eventsPayload
  let mouseEntered = isMouseEntered eventsPayload
  let mousePixelRate = if not useHiDPI then devicePixelRate else 1
  let baseSystemEvents = convertEvents mousePixelRate mousePos eventsPayload

--  when newSecond $
--    liftIO . putStrLn $ "Frames: " ++ show _mlFrameCount

  when quit $
    L.exitApplication .= True

  when windowExposed $
    L.mainBtnPress .= Nothing

  let newSecond = _mlFrameAccumTs > 1000
  let mainBtn = fromMaybe LeftBtn (_apcMainButton config)
  let wenv = WidgetEnv {
    _weOS = _mlOS,
    _weRenderer = renderer,
    _weFindByPath = const Nothing,
    _weMainButton = mainBtn,
    _weTheme = _mlTheme,
    _weWindowSize = windowSize,
    _weGlobalKeys = Map.empty,
    _weCursor = currCursor,
    _weHoveredPath = hovered,
    _weFocusedPath = focused,
    _weOverlayPath = overlay,
    _weDragStatus = dragged,
    _weMainBtnPress = mainPress,
    _weModel = currentModel,
    _weInputStatus = inputStatus,
    _weTimestamp = startTicks,
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
  (seWenv, seRoot, _) <- handleSystemEvents wtWenv baseSystemEvents wtRoot

  (newWenv, newRoot, _) <- if windowResized
    then do
      resizeWindow window
      handleResizeWidgets (seWenv, seRoot, Seq.empty)
    else return (seWenv, seRoot, Seq.empty)

  endTicks <- fmap fromIntegral SDL.ticks

  -- Rendering
  renderCurrentReq <- checkRenderCurrent startTicks _mlFrameStartTs

  let renderEvent = any isActionEvent eventsPayload
  let winRedrawEvt = windowResized || windowExposed
  let renderNeeded = winRedrawEvt || renderEvent || renderCurrentReq

  when renderNeeded $
    renderWidgets window renderer (_themeClearColor _mlTheme) newWenv newRoot

  L.renderRequested .= windowResized

  let fps = realToFrac _mlMaxFps
  let frameLength = round (1000000 / fps)
  let newTs = endTicks - startTicks
  let tempDelay = abs (frameLength - newTs * 1000)
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

  unless shouldQuit (mainLoop window renderer config newLoopArgs)

checkRenderCurrent :: (MonomerM s m) => Int -> Int -> m Bool
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

renderWidgets
  :: (MonomerM s m)
  => SDL.Window
  -> Renderer
  -> Color
  -> WidgetEnv s e
  -> WidgetNode s e
  -> m ()
renderWidgets !window renderer clearColor wenv widgetRoot = do
  SDL.V2 fbWidth fbHeight <- SDL.glGetDrawableSize window

  liftIO $ GL.clearColor GL.$= clearColor4
  liftIO $ GL.clear [GL.ColorBuffer]
  liftIO $ beginFrame renderer (fromIntegral fbWidth) (fromIntegral fbHeight)

  liftIO $ widgetRender (widgetRoot ^. L.widget) renderer wenv widgetRoot
  liftIO $ renderOverlays renderer

  liftIO $ endFrame renderer
  SDL.glSwapWindow window
  where
    r = fromIntegral (clearColor ^. L.r) / 255
    g = fromIntegral (clearColor ^. L.g) / 255
    b = fromIntegral (clearColor ^. L.b) / 255
    a = clearColor ^. L.a
    clearColor4 = GL.Color4 r g b (realToFrac a)

resizeWindow
  :: (MonomerM s m)
  => SDL.Window
  -> m ()
resizeWindow window = do
  dpr <- use L.dpr
  drawableSize <- getDrawableSize window
  windowSize <- getWindowSize window dpr

  let position = GL.Position 0 0
  let size = GL.Size (round $ _sW drawableSize) (round $ _sH drawableSize)

  L.windowSize .= windowSize
  liftIO $ GL.viewport GL.$= (position, size)

isWindowResized :: [SDL.EventPayload] -> Bool
isWindowResized eventsPayload = not status where
  status = null [ e | e@SDL.WindowResizedEvent {} <- eventsPayload ]

isWindowExposed :: [SDL.EventPayload] -> Bool
isWindowExposed eventsPayload = not status where
  status = null [ e | e@SDL.WindowExposedEvent {} <- eventsPayload ]

isMouseEntered :: [SDL.EventPayload] -> Bool
isMouseEntered eventsPayload = not status where
  status = null [ e | e@SDL.WindowGainedMouseFocusEvent {} <- eventsPayload ]
