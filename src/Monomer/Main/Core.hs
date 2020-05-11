{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Core where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Maybe
import Lens.Micro.Mtl

import qualified Data.Map as M
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL
import qualified NanoVG as NV

import Monomer.Common.Core
import Monomer.Common.Types
import Monomer.Common.Util
import Monomer.Event.Core
import Monomer.Event.Types
import Monomer.Main.Handlers
import Monomer.Main.Platform
import Monomer.Main.UserTask
import Monomer.Main.Util
import Monomer.Main.WidgetTask
import Monomer.Graphics.NanoVGRenderer
import Monomer.Graphics.Renderer

runWidgets :: (MonomerM s e m) => SDL.Window -> NV.Context -> MonomerApp s e m -> m ()
runWidgets window c mapp = do
  useHiDPI <- use useHiDPI
  devicePixelRate <- use devicePixelRate
  Rect rx ry rw rh <- use windowSize

  let dpr = if useHiDPI then devicePixelRate else 1
  let renderer = makeRenderer c dpr
  let newWindowSize = Rect rx ry (rw / dpr) (rh / dpr)

  windowSize .= newWindowSize
  ticks <- SDL.ticks
  newUI <- doInDrawingContext window c $ updateUI renderer mapp empty

  mainLoop window c renderer mapp (fromIntegral ticks) 0 0 newUI

mainLoop :: (MonomerM s e m) => SDL.Window -> NV.Context -> Renderer m -> MonomerApp s e m -> Int -> Int -> Int -> WidgetNode s e m -> m ()
mainLoop window c renderer mapp !prevTicks !tsAccum !frames widgets = do
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

  when newSecond $
    liftIO . putStrLn $ "Frames: " ++ (show frames)

  -- Pre process events (change focus, add Enter/Leave events when Move is received, etc)
  currentApp <- use appContext
  systemEvents <- preProcessEvents widgets baseSystemEvents
  uTasksEvents <- checkUserTasks
  (wTasksWidgets, wTasksEvents, wTasksResize) <- checkWidgetTasks mapp widgets
  (seApp, seWidgets, seAppEvents) <- handleSystemEvents renderer mapp currentApp systemEvents wTasksWidgets

  newApp <- handleAppEvents mapp seApp (seAppEvents ++ uTasksEvents ++ wTasksEvents)

  newWidgets <- bindIf (currentApp /= newApp) (updateUI renderer mapp) seWidgets
            >>= bindIf (resized || wTasksResize) (resizeWindow window renderer newApp)

  renderWidgets window c renderer newApp newWidgets startTicks

  endTicks <- fmap fromIntegral SDL.ticks

  let fps = 30
  let frameLength = 0.9 * 1000000 / fps
  let newTs = fromIntegral $ (endTicks - startTicks)
  let nextFrameDelay = round . abs $ (frameLength - newTs * 1000)

  liftIO $ threadDelay nextFrameDelay
  unless quit (mainLoop window c renderer mapp startTicks newTsAccum newFrameCount newWidgets)

handleAppEvents :: (MonomerM s e m) => MonomerApp s e m -> s -> [e] -> m s
handleAppEvents mapp app events = do
  let (newApp, tasks) = reduceAppEvents (_appEventHandler mapp) app events

  appContext .= newApp
  launchUserTasks tasks

  return newApp

reduceAppEvents :: AppEventHandler s e -> s -> [e] -> (s, [IO (Maybe e)])
reduceAppEvents appEventHandler app events = foldl reducer (app, []) events where
  reducer (app, tasks) event = case appEventHandler app event of
    State newApp -> (newApp, tasks)
    StateEvent newApp newEvent -> reducer (newApp, tasks) newEvent
    Task newApp task -> (newApp, task : tasks)

renderWidgets :: (MonomerM s e m) => SDL.Window -> NV.Context -> Renderer m -> s -> WidgetNode s e m -> Int -> m ()
renderWidgets !window !c !renderer app widgets ticks =
  doInDrawingContext window c $ do
    handleRender renderer app [0] widgets ticks

updateUI :: (MonomerM s e m) => Renderer m -> MonomerApp s e m -> WidgetNode s e m -> m (WidgetNode s e m)
updateUI renderer mapp oldWidgets = do
  windowSize <- use windowSize
  oldFocus <- getCurrentFocus
  app <- use appContext

  resizedUI <- resizeUI renderer app windowSize (mergeTrees app (_uiBuilder mapp app) oldWidgets)

  let paths = map snd $ filter (isFocusable . fst) $ collectPaths resizedUI []
  focusRing .= rotateUntil oldFocus paths
  currentFocus <- getCurrentFocus

  return $ setFocusedStatus currentFocus True resizedUI

resizeWindow :: (MonomerM s e m) => SDL.Window -> Renderer m -> s -> WidgetNode s e m -> m (WidgetNode s e m)
resizeWindow window renderer app widgetRoot = do
  dpr <- use devicePixelRate
  drawableSize <- getDrawableSize window
  newWindowSize <- getWindowSize window dpr
  newRoot <- resizeUI renderer app newWindowSize widgetRoot

  windowSize .= newWindowSize
  liftIO $ GL.viewport GL.$= (GL.Position 0 0, GL.Size (round $ _rw drawableSize) (round $ _rh drawableSize))

  return newRoot

preProcessEvents :: (MonomerM s e m) => (WidgetNode s e m) -> [SystemEvent] -> m [SystemEvent]
preProcessEvents widgets events = do
  systemEvents <- concatMapM (preProcessEvent widgets) events
  mapM_ updateInputStatus systemEvents
  return systemEvents

preProcessEvent :: (MonomerM s e m) => (WidgetNode s e m) -> SystemEvent -> m [SystemEvent]
preProcessEvent widgets evt@(Move point) = do
  hover <- use latestHover
  let current = findPathFromPoint point widgets
  let hoverChanged = isJust hover && current /= fromJust hover
  let enter = if isNothing hover || hoverChanged then [Enter point] else []
  let leave = if hoverChanged then [Leave (fromJust hover) point] else []

  when (isNothing hover || hoverChanged) $
    latestHover .= Just current

  return $ leave ++ enter ++ [evt]
preProcessEvent widgets event = return [event]

updateInputStatus :: (MonomerM s e m) => SystemEvent -> m ()
updateInputStatus (Click _ btn btnState) = inputStatus %= \ist -> ist {
    statusButtons = M.insert btn btnState (statusButtons ist)
  }
updateInputStatus (KeyAction kMod kCode kStatus) = inputStatus %= \ist -> ist {
    statusKeyMod = kMod,
    statusKeys = M.insert kCode kStatus (statusKeys ist)
  }
updateInputStatus _ = return ()
