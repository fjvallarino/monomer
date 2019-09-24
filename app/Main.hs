{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Default
import Foreign.C.Types
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import NanoVG
import SDL (($=))
import Unsafe.Coerce

import Debug.Trace

import System.Remote.Monitoring

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Sequence as SQ
import qualified Data.Text as T
import qualified Data.Vector.Generic as V
import qualified Foreign.C.String as STR
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL
import qualified SDL.Vect
import qualified SDL.Input.Mouse as Mouse
import qualified SDL.Raw.Error as SRE

import Types
import GUI.Core
import qualified GUI.Data.Tree as TR
import qualified GUI.NanoVGRenderer as NV
import qualified GUI.Widget.Core as W
import qualified GUI.Widget.Style as S
import qualified GUI.Widget.Widgets as WS

foreign import ccall unsafe "initGlew" glewInit :: IO CInt

data AppEvent = Action1 Int | Action2 deriving (Show, Eq)

type WidgetM = StateT App IO
type LocalWidget = W.Widget AppEvent WidgetM
type WidgetTree = TR.Tree (W.WidgetNode AppEvent WidgetM)

(screenWidth, screenHeight) = (640, 480)
windowSize = (Rect 0 0 (fromIntegral screenWidth) (fromIntegral screenHeight))

main :: IO ()
main = do
--  forkServer "localhost" 8000

  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  let customOpenGL = SDL.OpenGLConfig {
    SDL.glColorPrecision = SDL.V4 8 8 8 0,
    SDL.glDepthPrecision = 24,
    SDL.glStencilPrecision = 8,
    SDL.glProfile = SDL.Core SDL.Debug 3 2,
    SDL.glMultisampleSamples = 1
  }

  window <-
    SDL.createWindow
      "SDL / OpenGL Example"
      SDL.defaultWindow {SDL.windowInitialSize = SDL.V2 screenWidth screenHeight,
                         SDL.windowOpenGL = Just customOpenGL }

  err <- SRE.getError
  err <- STR.peekCString err
  putStrLn err

  _ <- SDL.glCreateContext window

  _ <- glewInit

  c@(Context c') <- createGL3 (S.fromList [Antialias, StencilStrokes, Debug])

  fontRes <- createFont c "sans" (FileName "./assets/fonts/Roboto-Regular.ttf")

  runStateT (runWidgets window c) def

  putStrLn "About to destroyWindow"
  SDL.destroyWindow window
  SDL.quit

handleAppEvent :: AppEvent -> WidgetM ()
handleAppEvent evt = do
  case evt of
    Action1 v -> do
      when (v == 0) $ clickCount += 1
      count <- use clickCount
      liftIO . putStrLn $ "Clicked button: " ++ (show v) ++ " - Count is: " ++ (show count)
    Action2   -> liftIO . putStrLn $ "I don't know what's this"

buildUI :: App -> WidgetTree
buildUI model = styledTree where
  border1 = S.border 5 (RGB 0 255 0) 20
  border2 = S.borderLeft 20 (RGB 200 200 0) <> S.borderRight 20 (RGB 200 0 200)
  style1 = S.bgColor (RGB 0 0 255) <> S.textSize 64 <> border1 <> border2 <> S.bgRadius 20
  textStyle = S.textColor (RGB 0 255 0)
  --extraWidgets = if _clickCount model < 3 then [] else [WS.button (Action1 0)] -- map (\i -> WS.button (Action1 0)) [0..(_clickCount model)]
  extraWidgets = map (\i -> WS.button (Action1 i)) [1..(_clickCount model)]
  widgetTree = WS.vgrid_ ([
      WS.textField_ `W.style` textStyle,
      WS.hgrid_ [
        WS.button (Action1 10) `W.style` style1,
        WS.button (Action1 10) `W.style` style1,
        WS.button (Action1 10) `W.style` style1
      ],
      WS.button (Action1 0) `W.style` style1
    ] ++ extraWidgets)
--  widgetTree = WS.container_ [
--      WS.button (Action1 1) `W.style` style1,
--      WS.button (Action1 2),
--      WS.button (Action1 3),
--      WS.container `W.style` S.bgColor (RGB 0 0 255) `W.children` [
--        WS.button (Action1 4),
--        WS.button (Action1 5) `W.style` (S.bgColor (RGB 255 0 255) <> S.bgRadius 10),
--        WS.button (Action1 6) `W.style` border1,
--        WS.button (Action1 7)
--      ]
--    ]
  styledTree = W.cascadeStyle mempty widgetTree

runWidgets :: SDL.Window -> Context -> WidgetM ()
runWidgets window c = do
  let renderer = NV.makeRenderer c

  ticks <- SDL.ticks
  app <- get
  resizedUI <- W.resizeUI renderer windowSize (buildUI app)

  mainLoop window c renderer (fromIntegral ticks) resizedUI

mainLoop :: SDL.Window -> Context -> Renderer WidgetM -> Int -> WidgetTree -> WidgetM ()
mainLoop window c renderer prevTicks widgets = do
  ticks <- fmap fromIntegral SDL.ticks
  events <- SDL.pollEvents

  let frameLength = 1000 `div` 30
  let nextFrame = \t -> if t >= frameLength then 0 else frameLength - t
  let !ts = (ticks - prevTicks)
  let eventsPayload = fmap SDL.eventPayload events
  let quit = elem SDL.QuitEvent eventsPayload

  newWidgets <- handleSystemEvents renderer (convertEvents eventsPayload) widgets
  renderWidgets window c renderer newWidgets ticks
  liftIO $ threadDelay $ (nextFrame ts) * 1000
  unless quit (mainLoop window c renderer ticks newWidgets)

handleSystemEvents :: Renderer WidgetM -> [W.SystemEvent] -> WidgetTree -> WidgetM WidgetTree
handleSystemEvents renderer systemEvents widgets = updatedWidgets where
  (eventsWidgets, appEvents) = W.handleEvents widgets systemEvents
  updatedWidgets = if | length appEvents == 0 -> return eventsWidgets
                      | otherwise -> handleAppEvents renderer appEvents eventsWidgets

handleAppEvents :: Renderer WidgetM -> SQ.Seq AppEvent -> WidgetTree -> WidgetM WidgetTree
handleAppEvents renderer appEvents oldWidgets = do
  app <- get
  forM_ appEvents handleAppEvent
  newApp <- get

  let newWidgets = W.mergeTrees (buildUI newApp) oldWidgets
  let mergedWidgets = if | app /= newApp -> W.resizeUI renderer windowSize newWidgets
                         | otherwise -> return oldWidgets

  when (app /= newApp) $ liftIO $ putStrLn "App changed!"

  mergedWidgets

renderWidgets :: SDL.Window -> Context -> Renderer WidgetM -> WidgetTree -> Int -> WidgetM ()
renderWidgets !window !c !renderer widgets ticks = do
  SDL.V2 fbWidth fbHeight <- SDL.glGetDrawableSize window
  let !pxRatio = fromIntegral fbWidth / fromIntegral fbHeight
  let !w = fromIntegral screenWidth
  let !h = fromIntegral screenHeight

  liftIO $ GL.clear [GL.ColorBuffer]
  liftIO $ beginFrame c screenWidth screenHeight pxRatio

  mapM_ (\widgetNode -> W.handleRender renderer ticks widgetNode) widgets

  liftIO $ endFrame c
  SDL.glSwapWindow window

convertEvents :: [SDL.EventPayload] -> [W.SystemEvent]
convertEvents events = newEvents
  where
    newEvents = mouseEvents ++ keyboardEvents
    mouseEvents = mouseClick events
    keyboardEvents = keyboardEvent events
    --SDL.P (SDL.V2 mouseX mouseY) <- Mouse.getAbsoluteMouseLocation

mouseClick :: [SDL.EventPayload] -> [W.SystemEvent]
mouseClick events =
  case clickEvent of
    Just (SDL.MouseButtonEvent SDL.MouseButtonEventData
          { SDL.mouseButtonEventMotion = motion,
            SDL.mouseButtonEventButton = button,
            SDL.mouseButtonEventPos = SDL.P (SDL.V2 x y) }) -> leftClicked ++ leftReleased ++ rightClicked ++ rightReleased
      where isLeft = button == SDL.ButtonLeft
            isRight = button == SDL.ButtonRight
            isClicked = motion == SDL.Pressed
            isReleased = motion == SDL.Released
            mousePos = Point (fromIntegral x) (fromIntegral y)
            leftClicked = if isLeft && isClicked then [W.Click mousePos W.LeftBtn W.PressedBtn] else []
            leftReleased = if isLeft && isReleased then [W.Click mousePos W.LeftBtn W.ReleasedBtn] else []
            rightClicked = if isRight && isClicked then [W.Click mousePos W.RightBtn W.PressedBtn] else []
            rightReleased = if isRight && isReleased then [W.Click mousePos W.RightBtn W.ReleasedBtn] else []

    otherwhise -> []
  where clickEvent = L.find (\evt -> case evt of
                                     SDL.MouseButtonEvent _ -> True
                                     otherwhise -> False
                          ) events

keyboardEvent :: [SDL.EventPayload] -> [W.SystemEvent]
keyboardEvent events = activeKeys
  where
    activeKeys = map (\(SDL.KeyboardEvent k) -> W.KeyAction (keyCode k) (keyMotion k)) (unsafeCoerce keyboardEvents)
    keyCode event = fromIntegral $ SDL.unwrapKeycode $ SDL.keysymKeycode $ SDL.keyboardEventKeysym event
    keyMotion event = if SDL.keyboardEventKeyMotion event == SDL.Pressed then W.KeyPressed else W.KeyReleased
    keyboardEvents = filter (\e -> case e of
                                      SDL.KeyboardEvent k -> True
                                      _ -> False) events
