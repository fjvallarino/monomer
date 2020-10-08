{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Main.Core (
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
import Data.Text (Text)
import Data.Typeable (Typeable)

import qualified Data.Map as M
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

data MainLoopArgs s e = MainLoopArgs {
  _mlOS :: Text,
  _mlTheme :: Theme,
  _mlAppStartTs :: Int,
  _mlFrameStartTs :: Int,
  _mlFrameAccumTs :: Int,
  _mlFrameCount :: Int,
  _mlWidgetRoot :: WidgetInstance s e
}

simpleApp
  :: (Eq s, Typeable s, Typeable e)
  => s
  -> EventHandler s e ()
  -> UIBuilder s e
  -> IO ()
simpleApp model eventHandler uiBuilder =
  simpleApp_ model eventHandler uiBuilder def

simpleApp_
  :: (Eq s, Typeable s, Typeable e)
  => s
  -> EventHandler s e ()
  -> UIBuilder s e
  -> [AppConfig e]
  -> IO ()
simpleApp_ model eventHandler uiBuilder configs = do
  window <- initSDLWindow config
  winSize <- getDrawableSize window

  let dpr = _sW winSize / fromIntegral winW
  let monomerContext = initMonomerContext () winSize useHdpi dpr

  runStateT (runApp window theme fonts appWidget) monomerContext
  detroySDLWindow window
  where
    config = mconcat configs
    (winW, winH) = fromMaybe defaultWindowSize (_apcWindowSize config)
    useHdpi = fromMaybe defaultUseHdpi (_apcHdpi config)
    fonts = _apcFonts config
    theme = fromMaybe def (_apcTheme config)
    initEvent = _apcInitEvent config
    appWidget = composite "app" model initEvent eventHandler uiBuilder

runApp
  :: (MonomerM s m)
  => SDL.Window
  -> Theme
  -> [FontDef]
  -> WidgetInstance s e
  -> m ()
runApp window theme fonts widgetRoot = do
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
    _weScreenSize = newWindowSize,
    _weGlobalKeys = M.empty,
    _weFocusedPath = rootPath,
    _weModel = model,
    _weInputStatus = def,
    _weTimestamp = startTs
  }
  let pathReadyRoot = widgetRoot {
    _wiPath = Seq.singleton 0
  }
  (newWenv, _, initializedRoot) <- handleWidgetInit wenv pathReadyRoot

  let resizedRoot = resizeWidget newWenv newWindowSize initializedRoot
  let loopArgs = MainLoopArgs {
    _mlOS = os,
    _mlTheme = theme,
    _mlAppStartTs = 0,
    _mlFrameStartTs = startTs,
    _mlFrameAccumTs = 0,
    _mlFrameCount = 0,
    _mlWidgetRoot = resizedRoot
  }

  mainModel .= _weModel newWenv
  pathFocus .= findNextFocus newWenv FocusFwd rootPath resizedRoot

  mainLoop window renderer loopArgs

mainLoop :: (MonomerM s m) => SDL.Window -> Renderer -> MainLoopArgs s e -> m ()
mainLoop window renderer loopArgs = do
  windowSize <- use L.windowSize
  useHiDPI <- use hdpi
  devicePixelRate <- use dpr
  startTicks <- fmap fromIntegral SDL.ticks
  events <- SDL.pollEvents
  mousePos <- getCurrentMousePos
  currentModel <- use mainModel
  focused <- use pathFocus

  let MainLoopArgs{..} = loopArgs
  let !ts = startTicks - _mlFrameStartTs
  let eventsPayload = fmap SDL.eventPayload events
  let quit = SDL.QuitEvent `elem` eventsPayload
  let windowResized = isWindowResized eventsPayload
  let mouseEntered = isMouseEntered eventsPayload
  let mousePixelRate = if not useHiDPI then devicePixelRate else 1
  let baseSystemEvents = convertEvents mousePixelRate mousePos eventsPayload
  let newSecond = _mlFrameStartTs + ts > 1000

  inputStatus <- updateInputStatus baseSystemEvents

  let wenv = WidgetEnv {
    _weOS = _mlOS,
    _weRenderer = renderer,
    _weTheme = _mlTheme,
    _weScreenSize = windowSize,
    _weGlobalKeys = M.empty,
    _weFocusedPath = focused,
    _weModel = currentModel,
    _weInputStatus = inputStatus,
    _weTimestamp = startTicks
  }

  --when newSecond $
  --  liftIO . putStrLn $ "Frames: " ++ (show frames)

  sysEvents <- preProcessEvents wenv _mlWidgetRoot baseSystemEvents
  isMouseFocused <- fmap isJust (use pathPressed)

  let isLeftPressed = isButtonPressed inputStatus LeftBtn

  when (mouseEntered && isLeftPressed && isMouseFocused) $
    pathPressed .= Nothing

  (wtWenv, _, wtRoot) <- handleWidgetTasks wenv _mlWidgetRoot
  (seWenv, _, seRoot) <- handleSystemEvents wtWenv sysEvents wtRoot

  newRoot <- if windowResized then resizeWindow window seWenv seRoot
                              else return seRoot

  renderWidgets window renderer seWenv newRoot

  endTicks <- fmap fromIntegral SDL.ticks

  let fps = 30
  let frameLength = 0.9 * 1000000 / fps
  let newTs = fromIntegral $ endTicks - startTicks
  let nextFrameDelay = round . abs $ (frameLength - newTs * 1000)
  let newLoopArgs = loopArgs {
    _mlAppStartTs = _mlAppStartTs + ts,
    _mlFrameStartTs = startTicks,
    _mlFrameAccumTs = if newSecond then 0 else _mlFrameAccumTs + ts,
    _mlFrameCount = if newSecond then 0 else _mlFrameCount + 1,
    _mlWidgetRoot = newRoot
  }

  liftIO $ threadDelay nextFrameDelay
  unless quit (mainLoop window renderer newLoopArgs)

renderWidgets
  :: (MonomerM s m)
  => SDL.Window
  -> Renderer
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> m ()
renderWidgets !window renderer wenv widgetRoot = do
  SDL.V2 fbWidth fbHeight <- SDL.glGetDrawableSize window

  liftIO $ GL.clear [GL.ColorBuffer]
  liftIO $ beginFrame renderer (fromIntegral fbWidth) (fromIntegral fbHeight)

  liftIO $ widgetRender (_wiWidget widgetRoot) renderer wenv widgetRoot
  liftIO $ renderOverlays renderer

  liftIO $ endFrame renderer
  SDL.glSwapWindow window

resizeWindow
  :: (MonomerM s m)
  => SDL.Window
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> m (WidgetInstance s e)
resizeWindow window wenv widgetRoot = do
  dpr <- use dpr
  drawableSize <- getDrawableSize window
  newWindowSize <- getWindowSize window dpr

  let position = GL.Position 0 0
  let size = GL.Size (round $ _sW drawableSize) (round $ _sH drawableSize)

  L.windowSize .= newWindowSize
  liftIO $ GL.viewport GL.$= (position, size)

  return $ resizeWidget wenv newWindowSize widgetRoot

-- Pre process events (change focus, add Enter/Leave events, etc)
preProcessEvents
  :: (MonomerM s m)
  => WidgetEnv s e -> WidgetInstance s e -> [SystemEvent] -> m [SystemEvent]
preProcessEvents wenv widgets events =
  concatMapM (preProcessEvent wenv widgets) events

preProcessEvent
  :: (MonomerM s m)
  => WidgetEnv s e -> WidgetInstance s e -> SystemEvent -> m [SystemEvent]
preProcessEvent wenv widgetRoot evt@(Move point) = do
  hover <- use pathHover
  let widget = _wiWidget widgetRoot
  let current = widgetFindByPoint widget wenv rootPath point widgetRoot
  let hoverChanged = current /= hover
  let enter = [Enter (fromJust current) point | isJust current && hoverChanged]
  let leave = [Leave (fromJust hover) point | isJust hover && hoverChanged]

  when hoverChanged $
    pathHover .= current

  return $ leave ++ enter ++ [evt]
preProcessEvent wenv widgetRoot evt@(ButtonAction point btn PressedBtn) = do
  let widget = _wiWidget widgetRoot
  let current = widgetFindByPoint widget wenv rootPath point widgetRoot

  pathPressed .= current
  return [evt]
preProcessEvent wenv widgetRoot evt@(ButtonAction point btn ReleasedBtn) = do
  pressed <- use pathPressed
  let widget = _wiWidget widgetRoot
  let current = widgetFindByPoint widget wenv rootPath point widgetRoot
  let extraEvt = [Click point btn | current == pressed]

  pathPressed .= Nothing
  return $ extraEvt ++ [evt]
preProcessEvent wenv widgetRoot event = return [event]

updateInputStatus :: (MonomerM s m) => [SystemEvent] -> m InputStatus
updateInputStatus systemEvents = do
  mapM_ evtToInputStatus systemEvents
  use inputStatus

evtToInputStatus :: (MonomerM s m) => SystemEvent -> m ()
evtToInputStatus (Move point) =
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

isMouseEntered :: [SDL.EventPayload] -> Bool
isMouseEntered eventsPayload = not status where
  status = null [ e | e@SDL.WindowGainedMouseFocusEvent {} <- eventsPayload ]
