{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Core (
  EventResponse(..),
  createApp,
  runWidgets
) where

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.State
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq, (><))
import Data.Text (Text)
import Data.Typeable (Typeable)

import qualified Data.Map as M
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL
import qualified Data.Sequence as Seq
import qualified NanoVG as NV

import Monomer.Common.Geometry
import Monomer.Event.Core
import Monomer.Event.Types
import Monomer.Event.Util
import Monomer.Main.Handlers
import Monomer.Main.Platform
import Monomer.Main.Types
import Monomer.Main.Util
import Monomer.Main.WidgetTask
import Monomer.Graphics.NanoVGRenderer
import Monomer.Graphics.Renderer
import Monomer.Widget.CompositeWidget
import Monomer.Widget.WidgetContext
import Monomer.Widget.Types

createApp :: (Eq s, Typeable s, Typeable e) => s -> Maybe e -> EventHandler s e () -> UIBuilder s e -> WidgetInstance () ()
createApp model initEvent eventHandler uiBuilder = composite "app" model initEvent eventHandler uiBuilder

createWidgetPlatform :: (Monad m) => Text -> Renderer m -> WidgetPlatform
createWidgetPlatform os renderer = WidgetPlatform {
  _wpOS = os,
  _wpGetKeyCode = getKeyCode,
  _wpTextBounds = textBounds renderer
}

runWidgets :: (MonomerM s m) => SDL.Window -> NV.Context -> WidgetInstance s e -> m ()
runWidgets window c widgetRoot = do
  useHiDPI <- use useHiDPI
  devicePixelRate <- use devicePixelRate
  Size rw rh <- use windowSize

  let dpr = if useHiDPI then devicePixelRate else 1
  let newWindowSize = Size (rw / dpr) (rh / dpr)

  windowSize .= newWindowSize
  ticks <- fmap fromIntegral SDL.ticks
  ctx <- get
  model <- use mainModel
  os <- getPlatform
  renderer <- makeRenderer c dpr
  let widgetPlatform = createWidgetPlatform os renderer
  let wenv = WidgetEnv {
    _wePlatform = widgetPlatform,
    _weScreenSize = newWindowSize,
    _weGlobalKeys = M.empty,
    _weModel = model,
    _weInputStatus = defInputStatus,
    _weTimestamp = ticks
  }
  (newWctx, _, initializedRoot) <- handleWidgetInit renderer wenv widgetRoot

  let newWidgetRoot = resizeWidget wenv newWindowSize initializedRoot

  mainModel .= _weModel newWctx
  focused .= findNextFocusable newWctx rootPath newWidgetRoot

  mainLoop window c renderer widgetPlatform ticks 0 0 newWidgetRoot

mainLoop :: (MonomerM s m) => SDL.Window -> NV.Context -> Renderer m -> WidgetPlatform -> Int -> Int -> Int -> WidgetInstance s e -> m ()
mainLoop window c renderer widgetPlatform !prevTicks !tsAccum !frames widgetRoot = do
  windowSize <- use windowSize
  useHiDPI <- use useHiDPI
  devicePixelRate <- use devicePixelRate
  startTicks <- fmap fromIntegral SDL.ticks
  events <- SDL.pollEvents
  mousePos <- getCurrentMousePos
  currentModel <- use mainModel
  oldInputStatus <- use inputStatus

  let !ts = startTicks - prevTicks
  let eventsPayload = fmap SDL.eventPayload events
  let quit = SDL.QuitEvent `elem` eventsPayload
  let resized = not $ null [ e | e@SDL.WindowResizedEvent {} <- eventsPayload ]
  let mouseEntered = not $ null [ e | e@SDL.WindowGainedMouseFocusEvent {} <- eventsPayload ]
  let mousePixelRate = if not useHiDPI then devicePixelRate else 1
  let baseSystemEvents = convertEvents mousePixelRate mousePos eventsPayload
  let newSecond = tsAccum + ts > 1000
  let newTsAccum = if newSecond then 0 else tsAccum + ts
  let newFrameCount = if newSecond then 0 else frames + 1
  let oldWctx = WidgetEnv {
    _wePlatform = widgetPlatform,
    _weScreenSize = windowSize,
    _weGlobalKeys = M.empty,
    _weModel = currentModel,
    _weInputStatus = oldInputStatus,
    _weTimestamp = startTicks
  }

  --when newSecond $
  --  liftIO . putStrLn $ "Frames: " ++ (show frames)

  -- Pre process events (change focus, add Enter/Leave events when Move is received, etc)
  systemEvents <- preProcessEvents oldWctx widgetRoot baseSystemEvents
  inputStatus <- use inputStatus
  isMouseFocusedWidget <- fmap isJust (use latestPressed)

  let isLeftPressed = isButtonPressed inputStatus LeftBtn
  let wenv = oldWctx {
    _weInputStatus = oldInputStatus
  }

  when (mouseEntered && isLeftPressed && isMouseFocusedWidget) $
    latestPressed .= Nothing

  (wtWctx, _, wtWidgetRoot) <- handleWidgetTasks renderer wenv widgetRoot
  (seWctx, _, seWidgetRoot) <- handleSystemEvents renderer wtWctx systemEvents wtWidgetRoot

  newWidgetRoot <- if resized then resizeWindow window seWctx seWidgetRoot
                              else return seWidgetRoot

  currentFocus <- use focused
  let ctx = WidgetContext {
    _wcVisible = _instanceVisible newWidgetRoot,
    _wcEnabled = _instanceEnabled newWidgetRoot,
    _wcFocusedPath = currentFocus,
    _wcTargetPath = rootPath,
    _wcCurrentPath = rootPath
  }
  renderWidgets window c renderer seWctx ctx newWidgetRoot
  runOverlays renderer

  endTicks <- fmap fromIntegral SDL.ticks

  let fps = 30
  let frameLength = 0.9 * 1000000 / fps
  let newTs = fromIntegral $ endTicks - startTicks
  let nextFrameDelay = round . abs $ (frameLength - newTs * 1000)

  liftIO $ threadDelay nextFrameDelay
  unless quit (mainLoop window c renderer widgetPlatform startTicks newTsAccum newFrameCount newWidgetRoot)

renderWidgets :: (MonomerM s m) => SDL.Window -> NV.Context -> Renderer m -> WidgetEnv s e -> WidgetContext -> WidgetInstance s e -> m ()
renderWidgets !window !c !renderer wenv ctx widgetRoot =
  doInDrawingContext window c $
    _widgetRender (_instanceWidget widgetRoot) renderer wenv ctx widgetRoot

resizeWindow :: (MonomerM s m) => SDL.Window -> WidgetEnv s e -> WidgetInstance s e -> m (WidgetInstance s e)
resizeWindow window wenv widgetRoot = do
  dpr <- use devicePixelRate
  drawableSize <- getDrawableSize window
  newWindowSize <- getWindowSize window dpr

  windowSize .= newWindowSize
  liftIO $ GL.viewport GL.$= (GL.Position 0 0, GL.Size (round $ _w drawableSize) (round $ _h drawableSize))

  return $ resizeWidget wenv newWindowSize widgetRoot

preProcessEvents :: (MonomerM s m) => WidgetEnv s e -> WidgetInstance s e -> [SystemEvent] -> m [SystemEvent]
preProcessEvents wenv widgets events = do
  systemEvents <- concatMapM (preProcessEvent wenv widgets) events
  mapM_ updateInputStatus systemEvents
  return systemEvents

preProcessEvent :: (MonomerM s m) => WidgetEnv s e -> WidgetInstance s e -> SystemEvent -> m [SystemEvent]
preProcessEvent wenv widgetRoot evt@(Move point) = do
  hover <- use latestHover
  let current = _widgetFind (_instanceWidget widgetRoot) wenv rootPath point widgetRoot
  let hoverChanged = isJust hover && current /= hover
  let enter = [Enter point | isNothing hover || hoverChanged]
  let leave = [Leave (fromJust hover) point | hoverChanged]

  when (isNothing hover || hoverChanged) $
    latestHover .= current

  return $ leave ++ enter ++ [evt]
preProcessEvent wenv widgetRoot evt@(ButtonAction point btn PressedBtn) = do
  let current = _widgetFind (_instanceWidget widgetRoot) wenv rootPath point widgetRoot

  latestPressed .= current
  return [evt]
preProcessEvent wenv widgetRoot evt@(ButtonAction point btn ReleasedBtn) = do
  latestPressed .= Nothing
  return [Click point btn, evt]

preProcessEvent wenv widgetRoot event = return [event]

updateInputStatus :: (MonomerM s m) => SystemEvent -> m ()
updateInputStatus (Move point) = inputStatus %= \status -> status {
    statusMousePos = point
  }
updateInputStatus (ButtonAction _ btn btnState) = inputStatus %= \status -> status {
    statusButtons = M.insert btn btnState (statusButtons status)
  }
updateInputStatus (KeyAction kMod kCode kStatus) = inputStatus %= \status -> status {
    statusKeyMod = kMod,
    statusKeys = M.insert kCode kStatus (statusKeys status)
  }
updateInputStatus _ = return ()
