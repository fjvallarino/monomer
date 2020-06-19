{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Core (
  EventResponse(..),
  createApp,
  runWidgets
) where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.State
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq, (><))
import Data.Typeable (Typeable)
import Lens.Micro.Mtl

import qualified Data.Map as M
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL
import qualified Data.Sequence as Seq
import qualified NanoVG as NV

import Monomer.Common.Geometry
import Monomer.Event.Core
import Monomer.Event.Types
import Monomer.Main.Handlers
import Monomer.Main.Platform
import Monomer.Main.Types
import Monomer.Main.Util
import Monomer.Main.WidgetTask
import Monomer.Graphics.NanoVGRenderer
import Monomer.Graphics.Renderer
import Monomer.Widget.CompositeWidget
import Monomer.Widget.Core
import Monomer.Widget.PathContext
import Monomer.Widget.Types

createApp :: (Eq s, Typeable s, Typeable e) => s -> Maybe e -> EventHandler s e () -> UIBuilder s e -> WidgetInstance () ()
createApp app initEvent eventHandler uiBuilder = composite "app" app initEvent (eventHandlerWrapper eventHandler) uiBuilder

eventHandlerWrapper :: EventHandler s e () -> s -> e -> EventResponse s e ()
eventHandlerWrapper eventHandler app evt = eventHandler app evt

runWidgets :: (MonomerM s m) => SDL.Window -> NV.Context -> WidgetInstance s e -> m ()
runWidgets window c widgetRoot = do
  useHiDPI <- use useHiDPI
  devicePixelRate <- use devicePixelRate
  Rect rx ry rw rh <- use windowSize

  let dpr = if useHiDPI then devicePixelRate else 1
  let newWindowSize = Rect rx ry (rw / dpr) (rh / dpr)

  windowSize .= newWindowSize
  ticks <- SDL.ticks
  ctx <- get
  app <- use appContext
  renderer <- makeRenderer c dpr
  (newApp, _, initializedRoot) <- handleWidgetInit renderer app widgetRoot

  let styledRoot = cascadeStyle mempty initializedRoot
  let newWidgetRoot = resizeUI renderer app newWindowSize styledRoot

  appContext .= newApp
  focused .= findNextFocusable rootPath newWidgetRoot

  mainLoop window c renderer (fromIntegral ticks) 0 0 newWidgetRoot

mainLoop :: (MonomerM s m) => SDL.Window -> NV.Context -> Renderer m -> Int -> Int -> Int -> WidgetInstance s e -> m ()
mainLoop window c renderer !prevTicks !tsAccum !frames widgetRoot = do
  windowSize <- use windowSize
  useHiDPI <- use useHiDPI
  devicePixelRate <- use devicePixelRate
  startTicks <- fmap fromIntegral SDL.ticks
  events <- SDL.pollEvents
  mousePos <- getCurrentMousePos

  let !ts = (startTicks - prevTicks)
  let eventsPayload = fmap SDL.eventPayload events
  let quit = elem SDL.QuitEvent eventsPayload
  let resized = not $ null [ e | e@SDL.WindowResizedEvent {} <- eventsPayload ]
  let mousePixelRate = if not useHiDPI then devicePixelRate else 1
  let baseSystemEvents = convertEvents mousePixelRate mousePos eventsPayload
  let newSecond = tsAccum + ts > 1000
  let newTsAccum = if newSecond then 0 else tsAccum + ts
  let newFrameCount = if newSecond then 0 else frames + 1

  --when newSecond $
  --  liftIO . putStrLn $ "Frames: " ++ (show frames)

  -- Pre process events (change focus, add Enter/Leave events when Move is received, etc)
  currentApp <- use appContext
  systemEvents <- preProcessEvents widgetRoot baseSystemEvents
  (wtApp, _, wtWidgetRoot) <- handleWidgetTasks renderer currentApp widgetRoot
  (seApp, _, seWidgetRoot) <- handleSystemEvents renderer wtApp systemEvents wtWidgetRoot

  newWidgetRoot <- if resized then resizeWindow window renderer seApp seWidgetRoot
                              else return seWidgetRoot

  currentFocus <- use focused
  renderWidgets window c renderer (PathContext currentFocus rootPath rootPath) seApp newWidgetRoot startTicks
  runOverlays renderer

  endTicks <- fmap fromIntegral SDL.ticks

  let fps = 30
  let frameLength = 0.9 * 1000000 / fps
  let newTs = fromIntegral $ (endTicks - startTicks)
  let nextFrameDelay = round . abs $ (frameLength - newTs * 1000)

  liftIO $ threadDelay nextFrameDelay
  unless quit (mainLoop window c renderer startTicks newTsAccum newFrameCount newWidgetRoot)

renderWidgets :: (MonomerM s m) => SDL.Window -> NV.Context -> Renderer m -> PathContext -> s -> WidgetInstance s e -> Int -> m ()
renderWidgets !window !c !renderer ctx app widgetRoot ticks =
  doInDrawingContext window c $ do
    _widgetRender (_instanceWidget widgetRoot) renderer ticks ctx app widgetRoot

resizeUI :: (Monad m) => Renderer m -> s -> Rect -> WidgetInstance s e -> WidgetInstance s e
resizeUI renderer app assignedRect widgetRoot = newWidgetRoot where
  widget = _instanceWidget widgetRoot
  preferredSizes = _widgetPreferredSize widget renderer app widgetRoot
  newWidgetRoot = _widgetResize widget app assignedRect assignedRect widgetRoot preferredSizes

resizeWindow :: (MonomerM s m) => SDL.Window -> Renderer m -> s -> WidgetInstance s e -> m (WidgetInstance s e)
resizeWindow window renderer app widgetRoot = do
  dpr <- use devicePixelRate
  drawableSize <- getDrawableSize window
  newWindowSize <- getWindowSize window dpr

  windowSize .= newWindowSize
  liftIO $ GL.viewport GL.$= (GL.Position 0 0, GL.Size (round $ _rw drawableSize) (round $ _rh drawableSize))

  return $ resizeUI renderer app newWindowSize widgetRoot

preProcessEvents :: (MonomerM s m) => (WidgetInstance s e) -> [SystemEvent] -> m [SystemEvent]
preProcessEvents widgets events = do
  systemEvents <- concatMapM (preProcessEvent widgets) events
  mapM_ updateInputStatus systemEvents
  return systemEvents

preProcessEvent :: (MonomerM s m) => (WidgetInstance s e) -> SystemEvent -> m [SystemEvent]
preProcessEvent widgetRoot evt@(Move point) = do
  hover <- use latestHover
  let current = _widgetFind (_instanceWidget widgetRoot) point widgetRoot
  let hoverChanged = isJust hover && current /= hover
  let enter = if isNothing hover || hoverChanged then [Enter point] else []
  let leave = if hoverChanged then [Leave (fromJust hover) point] else []

  when (isNothing hover || hoverChanged) $
    latestHover .= current

  return $ leave ++ enter ++ [evt]
preProcessEvent widgetRoot event = return [event]

updateInputStatus :: (MonomerM s m) => SystemEvent -> m ()
updateInputStatus (Click _ btn btnState) = inputStatus %= \status -> status {
    statusButtons = M.insert btn btnState (statusButtons status)
  }
updateInputStatus (KeyAction kMod kCode kStatus) = inputStatus %= \status -> status {
    statusKeyMod = kMod,
    statusKeys = M.insert kCode kStatus (statusKeys status)
  }
updateInputStatus _ = return ()
