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
import Monomer.Widgets

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

  winSize@(Rect rx ry rw rh) <- getDrawableSize window

  let devicePixelRate = _rw winSize / fromIntegral screenWidth
  let mapp = MonomerApp buildUI handleAppEvent

  runStateT (runWidgets window c mapp) (initMonomerContext def winSize useHiDPI devicePixelRate)

  putStrLn "About to destroyWindow"
  SDL.destroyWindow window
  SDL.quit

--handleAppEvent :: App -> AppEvent -> EventResponse App AppEvent
handleAppEvent app evt = do
  case evt of
    RunShortTask -> State $ app & textField3 .~ "Updated!"
    RunLongTask -> Task app $ do
      threadDelay $ 1 * 1000 * 1000

      return $ Just (UpdateText3 "HOLA")
    PrintTextFields -> Task app $ do
      putStrLn $ "Current text 1 is: " ++ show (app ^. textField1)
      putStrLn $ "Current text 2 is: " ++ show (app ^. textField2)
      putStrLn $ "Current text 3 is: " ++ show (app ^. textField3)
      return Nothing
    IncreaseCount v -> Task (app & clickCount %~ (+1)) $ do
      putStrLn $ "Clicked button: " ++ (show v) ++ " - Count is: " ++ show (app ^. clickCount)

      return Nothing
    UpdateText3 txt -> State $ app & textField3 .~ txt

buildUI app = trace "Created main UI" $ widgetTree where
  widgetTree =
    vstack [
      testComposite,
      button "Increase" (IncreaseCount 1)
    ]

buildUI2 app = widgetTree where
  widgetTree =
    vstack [
      label "This is label 1" `style` bgColor blue,
      label "This is label 2" `style` bgColor black,
      label "This is label 3" `style` bgColor blue,
      hstack [
        textField textField1,
        button "Update state" (UpdateText3 $ app ^. textField1),
        textField textField3
      ],
      flip style (sheight 300) $ scroll $ vstack [
        label "This is label in scroll 01" `style` bgColor lightGray,
        label "This is label in scroll 02" `style` bgColor gray,
        label "This is label in scroll 03" `style` bgColor darkGray,
        label "This is label in scroll 04" `style` bgColor lightGray,
        label "This is label in scroll 05" `style` bgColor gray,
        label "This is label in scroll 06" `style` bgColor darkGray,
        label "This is label in scroll 07" `style` bgColor lightGray,
        label "This is label in scroll 08" `style` bgColor gray,
        label "This is label in scroll 09" `style` bgColor darkGray,
        label "This is label in scroll 10" `style` bgColor lightGray,
        label "This is label in scroll 11" `style` bgColor gray,
        label "This is label in scroll 12" `style` bgColor darkGray,
        label "This is label in scroll 13" `style` bgColor lightGray,
        label "This is label in scroll 14" `style` bgColor gray,
        label "This is label in scroll 15" `style` bgColor darkGray,
        label "This is label in scroll 16" `style` bgColor lightGray,
        label "This is label in scroll 17" `style` bgColor gray,
        label "This is label in scroll 18" `style` bgColor darkGray
      ]
    ]
