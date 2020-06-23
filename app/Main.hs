{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

import Control.Concurrent (threadDelay)
import Control.Monad.State
import Data.Default
import Foreign.C.Types
import Lens.Micro
import NanoVG (Context(..), createGL3, CreateFlags(..), createFont, FileName(..), beginFrame, endFrame)
import SDL (($=))
import TextShow

import System.Remote.Monitoring

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Foreign.C.String as STR
import qualified SDL
import qualified SDL.Raw.Error as SRE
import qualified SDL.Raw.Event as SREv

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Graphics.Color
import Monomer.Main.Core
import Monomer.Main.Platform
import Monomer.Main.Types
import Monomer.Main.Util
import Monomer.Widget.Util
import Monomer.Widget.Types
import Monomer.Widgets

import KeysComposite
import TestComposite
import Types

foreign import ccall unsafe "initGlew" glewInit :: IO CInt

--type AppContext = MonomerContext App AppEvent
--type AppM = StateT AppContext IO
--type WidgetTree = Tree (WidgetInstance App AppEvent AppM)

main :: IO ()
main = do
  --forkServer "localhost" 28000

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

  let (screenWidth, screenHeight) = (640, 480)
      windowHiDPI = True
      useHiDPI = True

  window <-
    SDL.createWindow
      "SDL / OpenGL Example"
      SDL.defaultWindow {
        SDL.windowInitialSize = SDL.V2 screenWidth screenHeight,
        SDL.windowHighDPI = windowHiDPI,
        SDL.windowResizable = True,
        SDL.windowOpenGL = Just customOpenGL
      }

  err <- SRE.getError
  err <- STR.peekCString err
  putStrLn err

  _ <- SDL.glCreateContext window

  _ <- glewInit

  c@(Context c') <- createGL3 (S.fromList [Antialias, StencilStrokes, Debug])

  fontRes <- createFont c "sans" (FileName "./assets/fonts/Roboto-Regular.ttf")

  SREv.startTextInput

  winSize <- getDrawableSize window

  let devicePixelRate = _w winSize / fromIntegral screenWidth
  let appWidget = createApp def (Just InitApp) handleAppEvent buildUI

  runStateT (runWidgets window c appWidget) (initMonomerContext () winSize useHiDPI devicePixelRate)

  putStrLn "About to destroyWindow"
  SDL.destroyWindow window
  SDL.quit

--handleAppEvent :: App -> AppEvent -> EventResponse App AppEvent
handleAppEvent app evt = do
  case evt of
    InitApp -> Task $ do
      putStrLn $ "Initialized application"
      return Nothing
    RunShortTask -> Model (app & textField1 .~ "Updated!")
    RunLongTask -> Task $ do
      threadDelay $ 1 * 1000 * 1000
      return $ Just (UpdateText "HOLA")
    PrintTextFields -> Task $ do
      putStrLn $ "Current text 1 is: " ++ show (app ^. textField1)
      return Nothing
    AppButton -> Message (WidgetKey "kcmp") RotateChildren <> Model (app & clickCount %~ (+1)) <> (Task $ do
      putStrLn $ "Clicked button"
      return Nothing)
    IncreaseMessage -> Model (app & msgCount %~ (+1))
    UpdateText txt -> Model (app & textField1 .~ txt)

buildUI app = trace "Created main UI" $ widgetTree where
  widgetTree =
    vstack [
      dropdown dropdown1 (fmap (\i -> "Value " <> showt i) [1..10::Int]) id,
      hgrid [
        button ("Increase: " <> (showt $ _clickCount app)) AppButton,
        label $ "Messages: " <> (showt $ _msgCount app)
      ]
      --testComposite
      --keysComposite `key` "kcmp"
    ]
