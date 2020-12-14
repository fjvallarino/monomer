{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Main.Core (
  AppEventResponse,
  AppEventHandler,
  AppUIBuilder,
  EventResponse(..),
  simpleApp,
  simpleApp_,
  runApp
) where

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad.Extra
import Control.Monad.State
import Data.Default
import Data.Maybe
import Data.List (foldl')
import Data.Text (Text)
import Data.Typeable (Typeable)

import qualified Data.Map as Map
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL
import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event
import Monomer.Lens
import Monomer.Main.Handlers
import Monomer.Main.Platform
import Monomer.Main.Types
import Monomer.Main.Util
import Monomer.Main.WidgetTask
import Monomer.Graphics
import Monomer.Widgets.Composite

import qualified Monomer.Lens as L

type AppEventResponse s e = EventResponse s e ()
type AppEventHandler s e = s -> e -> [AppEventResponse s e]
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
  _mlExitEvent :: Maybe e,
  _mlWidgetRoot :: WidgetNode s ep
}

simpleApp
  :: (Eq s, Typeable s, Typeable e)
  => s
  -> AppEventHandler s e
  -> AppUIBuilder s e
  -> IO ()
simpleApp model eventHandler uiBuilder =
  simpleApp_ model eventHandler uiBuilder def

simpleApp_
  :: (Eq s, Typeable s, Typeable e)
  => s
  -> AppEventHandler s e
  -> AppUIBuilder s e
  -> [AppConfig e]
  -> IO ()
simpleApp_ model eventHandler uiBuilder configs = do
  (window, dpr) <- initSDLWindow config
  winSize <- getDrawableSize window

  let monomerContext = initMonomerContext model window winSize useHdpi dpr

  runStateT (runApp window maxFps fonts theme exitEvent appWidget) monomerContext
  detroySDLWindow window
  where
    config = mconcat configs
    useHdpi = fromMaybe defaultUseHdpi (_apcHdpi config)
    maxFps = fromMaybe 30 (_apcMaxFps config)
    fonts = _apcFonts config
    theme = fromMaybe def (_apcTheme config)
    initEvent = _apcInitEvent config
    exitEvent = _apcExitEvent config
    appWidget = composite "app" id initEvent uiBuilder eventHandler

runApp
  :: (MonomerM s m, Typeable e)
  => SDL.Window
  -> Int
  -> [FontDef]
  -> Theme
  -> Maybe e
  -> WidgetNode s ep
  -> m ()
runApp window maxFps fonts theme exitEvent widgetRoot = do
  useHiDPI <- use hdpi
  devicePixelRate <- use dpr
  Size rw rh <- use L.windowSize

  let dpr = if useHiDPI then devicePixelRate else 1
  let newWindowSize = Size (rw / dpr) (rh / dpr)

  L.windowSize .= newWindowSize
  startTs <- fmap fromIntegral SDL.ticks
  model <- use mainModel
  os <- getPlatform
  renderer <- liftIO $ makeRenderer fonts dpr
  let wenv = WidgetEnv {
    _weOS = os,
    _weRenderer = renderer,
    _weTheme = theme,
    _weWindowSize = newWindowSize,
    _weGlobalKeys = Map.empty,
    _weCurrentCursor = CursorArrow,
    _weFocusedPath = rootPath,
    _weOverlayPath = Nothing,
    _wePressedPath = Nothing,
    _weModel = model,
    _weInputStatus = def,
    _weTimestamp = startTs,
    _weInTopLayer = const True
  }
  let pathReadyRoot = widgetRoot & L.info . L.path .~ Seq.singleton 0

  handleResourcesInit
  (newWenv, _, initializedRoot) <- handleWidgetInit wenv pathReadyRoot

  let resizedRoot = resizeRoot newWenv newWindowSize initializedRoot
  let loopArgs = MainLoopArgs {
    _mlOS = os,
    _mlTheme = theme,
    _mlMaxFps = maxFps,
    _mlAppStartTs = 0,
    _mlLatestRenderTs = 0,
    _mlFrameStartTs = startTs,
    _mlFrameAccumTs = 0,
    _mlFrameCount = 0,
    _mlExitEvent = exitEvent,
    _mlWidgetRoot = resizedRoot
  }

  mainModel .= _weModel newWenv
  focusedPath .= findNextFocus newWenv FocusFwd rootPath Nothing resizedRoot

  mainLoop window renderer loopArgs

mainLoop
  :: (MonomerM s m, Typeable e)
  => SDL.Window
  -> Renderer
  -> MainLoopArgs s e ep
  -> m ()
mainLoop window renderer loopArgs = do
  windowSize <- use L.windowSize
  useHiDPI <- use hdpi
  devicePixelRate <- use dpr
  startTicks <- fmap fromIntegral SDL.ticks
  events <- SDL.pollEvents
  mousePos <- getCurrentMousePos
  currentModel <- use mainModel
  currentCursor <- use currentCursor
  focused <- use focusedPath
  overlay <- use overlayPath
  pressed <- use pressedPath

  let MainLoopArgs{..} = loopArgs
  let !ts = startTicks - _mlFrameStartTs
  let eventsPayload = fmap SDL.eventPayload events
  let quit = SDL.QuitEvent `elem` eventsPayload
  let windowResized = isWindowResized eventsPayload
  let windowExposed = isWindowExposed eventsPayload
  let mouseEntered = isMouseEntered eventsPayload
  let mousePixelRate = if not useHiDPI then devicePixelRate else 1
  let baseSystemEvents = convertEvents mousePixelRate mousePos eventsPayload
  let newSecond = _mlFrameAccumTs > 1000
  let isMouseFocused = isJust pressed

  inputStatus <- updateInputStatus baseSystemEvents

  when quit $ exitApplication .= True

  let wenv = WidgetEnv {
    _weOS = _mlOS,
    _weRenderer = renderer,
    _weTheme = _mlTheme,
    _weWindowSize = windowSize,
    _weGlobalKeys = Map.empty,
    _weCurrentCursor = currentCursor,
    _weFocusedPath = focused,
    _weOverlayPath = overlay,
    _wePressedPath = pressed,
    _weModel = currentModel,
    _weInputStatus = inputStatus,
    _weTimestamp = startTicks,
    _weInTopLayer = const True
  }

  when newSecond $
    liftIO . putStrLn $ "Frames: " ++ show _mlFrameCount

  sysEvents <- preProcessEvents wenv _mlWidgetRoot baseSystemEvents

  let isMainBtnPressed = isButtonPressed inputStatus LeftBtn
  -- Exit handler
  let exitMsg = SendMessage (Seq.fromList [0]) _mlExitEvent
  let baseReqs = Seq.fromList [ exitMsg | quit ]
  let baseStep = (wenv, Seq.empty, _mlWidgetRoot)

  when (mouseEntered && isMainBtnPressed && isMouseFocused) $
    pressedPath .= Nothing

  (rqWenv, _, rqRoot) <- handleRequests baseReqs baseStep
  (wtWenv, _, wtRoot) <- handleWidgetTasks rqWenv rqRoot
  (seWenv, _, seRoot) <- handleSystemEvents wtWenv sysEvents wtRoot

  newRoot <- if windowResized then resizeWindow window seWenv seRoot
                              else return seRoot

  endTicks <- fmap fromIntegral SDL.ticks

  -- Rendering
  renderCurrentReq <- checkRenderCurrent startTicks _mlFrameStartTs

  let renderEvent = any isActionEvent eventsPayload
  let windowRedrawEvt = windowResized || windowExposed
  let renderNeeded = windowRedrawEvt || renderEvent || renderCurrentReq

  when renderNeeded $
    renderWidgets window renderer seWenv newRoot

  renderRequested .= windowResized

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

  shouldQuit <- use exitApplication

  when shouldQuit $
    void $ handleWidgetDispose seWenv seRoot

  unless shouldQuit (mainLoop window renderer newLoopArgs)

checkRenderCurrent :: (MonomerM s m) => Int -> Int -> m Bool
checkRenderCurrent currTs renderTs = do
  renderNext <- use renderRequested
  schedule <- use renderSchedule
  return (renderNext || nextRender schedule)
  where
    foldHelper acc curr = acc || renderScheduleDone currTs renderTs curr
    nextRender schedule = foldl' foldHelper False schedule

renderScheduleDone :: Int -> Int -> RenderSchedule -> Bool
renderScheduleDone currTs renderTs schedule = nextStep < currTs where
  RenderSchedule _ start ms = schedule
  stepsDone = round (fromIntegral (renderTs - start) / fromIntegral ms)
  currStep = start + ms * stepsDone
  nextStep
    | currStep >= renderTs = currStep
    | otherwise = currStep + ms

renderWidgets
  :: (MonomerM s m)
  => SDL.Window
  -> Renderer
  -> WidgetEnv s e
  -> WidgetNode s e
  -> m ()
renderWidgets !window renderer wenv widgetRoot = do
  SDL.V2 fbWidth fbHeight <- SDL.glGetDrawableSize window

  liftIO $ GL.clear [GL.ColorBuffer]
  liftIO $ beginFrame renderer (fromIntegral fbWidth) (fromIntegral fbHeight)

  liftIO $ widgetRender (widgetRoot ^. L.widget) renderer wenv widgetRoot
  liftIO $ renderOverlays renderer

  liftIO $ endFrame renderer
  SDL.glSwapWindow window

-- Pre process events (change focus, add Enter/Leave events, etc)
preProcessEvents
  :: (MonomerM s m)
  => WidgetEnv s e -> WidgetNode s e -> [SystemEvent] -> m [SystemEvent]
preProcessEvents wenv widgets events =
  concatMapM (preProcessEvent wenv widgets) events

preProcessEvent
  :: (MonomerM s m)
  => WidgetEnv s e -> WidgetNode s e -> SystemEvent -> m [SystemEvent]
preProcessEvent wenv widgetRoot evt@(Move point) = do
  overlay <- use L.overlayPath
  hover <- use hoveredPath
  let startPath = fromMaybe rootPath overlay
  let widget = widgetRoot ^. L.widget
  let current = widgetFindByPoint widget wenv startPath point widgetRoot
  let hoverChanged = current /= hover
  let enter = [Enter (fromJust current) point | isJust current && hoverChanged]
  let leave = [Leave (fromJust hover) point | isJust hover && hoverChanged]

  when hoverChanged $
    hoveredPath .= current

  return $ leave ++ enter ++ [evt]
preProcessEvent wenv widgetRoot evt@(ButtonAction point btn PressedBtn) = do
  overlay <- use L.overlayPath
  let startPath = fromMaybe rootPath overlay
  let widget = widgetRoot ^. L.widget
  let current = widgetFindByPoint widget wenv startPath point widgetRoot

  pressedPath .= current
  return [evt]
preProcessEvent wenv widgetRoot evt@(ButtonAction point btn ReleasedBtn) = do
  overlay <- use L.overlayPath
  pressed <- use pressedPath
  let startPath = fromMaybe rootPath overlay
  let widget = widgetRoot ^. L.widget
  let current = widgetFindByPoint widget wenv startPath point widgetRoot
  let extraEvt = [Click point btn | current == pressed]

  pressedPath .= Nothing
  return $ extraEvt ++ [evt]
preProcessEvent wenv widgetRoot event = return [event]

updateInputStatus :: (MonomerM s m) => [SystemEvent] -> m InputStatus
updateInputStatus systemEvents = do
  mapM_ evtToInputStatus systemEvents
  use inputStatus

evtToInputStatus :: (MonomerM s m) => SystemEvent -> m ()
evtToInputStatus (Move point) = do
  status <- use inputStatus
  inputStatus . mousePosPrev .= status ^. mousePos
  inputStatus . mousePos .= point
evtToInputStatus (ButtonAction _ btn btnState) =
  inputStatus . buttons . at btn ?= btnState
evtToInputStatus (KeyAction kMod kCode kStatus) = do
  inputStatus . keyMod .= kMod
  inputStatus . keys . at kCode ?= kStatus
evtToInputStatus _ = return ()

isWindowResized :: [SDL.EventPayload] -> Bool
isWindowResized eventsPayload = not status where
  status = null [ e | e@SDL.WindowResizedEvent {} <- eventsPayload ]

isWindowExposed :: [SDL.EventPayload] -> Bool
isWindowExposed eventsPayload = not status where
  status = null [ e | e@SDL.WindowExposedEvent {} <- eventsPayload ]

isMouseEntered :: [SDL.EventPayload] -> Bool
isMouseEntered eventsPayload = not status where
  status = null [ e | e@SDL.WindowGainedMouseFocusEvent {} <- eventsPayload ]
