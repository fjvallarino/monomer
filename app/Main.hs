{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

import Control.Concurrent (threadDelay)
import Control.Lens (Lens', (&), (<&>), (^.), (.~), (%~))
import Control.Monad.State
import Data.Default
import Foreign.C.Types
import NanoVG (
    Context(..), CreateFlags(..), FileName(..),
    beginFrame, createFont, createGL3, endFrame
  )
import SDL (($=))
import TextShow

import System.Remote.Monitoring

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Foreign.C.String as STR
import qualified SDL
import qualified SDL.Raw.Error as SRE
import qualified SDL.Raw.Event as SREv

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.StyleCombinators
import Monomer.Graphics.Color
import Monomer.Main.Core
import Monomer.Main.Platform
import Monomer.Main.Util
import Monomer.Widget.Util
import Monomer.Widget.Types
import Monomer.Widgets

import qualified Monomer.Common.LensStyle as S

import KeysComposite
import TestComposite
import Types

foreign import ccall unsafe "initGlew" glewInit :: IO CInt

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
        SDL.windowGraphicsContext = SDL.OpenGLContext customOpenGL
      }

  err <- SRE.getError
  err <- STR.peekCString err
  putStrLn err

  _ <- SDL.glCreateContext window

  _ <- glewInit

  c@(Context c') <- createGL3 (Set.fromList [Antialias, StencilStrokes, Debug])

  fontRes <- createFont c "sans" (FileName "./assets/fonts/Roboto-Regular.ttf")

  SREv.startTextInput

  winSize <- getDrawableSize window

  let devicePixelRate = _sW winSize / fromIntegral screenWidth
  let appWidget = createApp def (Just InitApp) handleAppEvent buildUI
  let monomerContext = initMonomerContext () winSize useHiDPI devicePixelRate
  let theme = def
        & S.basic . S.fgColor .~ blue
        & S.hover . S.fgColor .~ white
        & S.focus . S.fgColor .~ white

  runStateT (runWidgets window c theme appWidget) monomerContext

  putStrLn "About to destroyWindow"
  SDL.destroyWindow window
  SDL.quit

handleAppEvent2 model evt = traceShow evt $ traceShow model $
  case evt of
    InitApp -> Task $ do
      putStrLn "Initialized application"
      return Nothing
    RunShortTask -> Model (model & textField1 .~ "Updated!")
    RunLongTask -> Task $ do
      threadDelay $ 1 * 1000 * 1000
      return $ Just (UpdateText "HOLA")
    PrintTextFields -> Task $ do
      putStrLn $ "Current text 1 is: " ++ show (model ^. textField1)
      return Nothing
    AppButton -> Message (WidgetKey "kcmp") RotateChildren
      <> Model (model & clickCount %~ (+1))
      <> (Task $ do
        putStrLn "Clicked button"
        return Nothing)
    IncreaseMessage -> Model (model & msgCount %~ (+1))
    UpdateText txt -> Model (model & textField1 .~ txt)
    _ -> Model model

handleAppEvent model evt = case evt of
  IncButton -> Model (model & clickCount %~ (+1))
  _ -> Model model

buildUI model = trace "Creating UI" widgetTree where
  widgetTree1 = scroll $ vstack (newLabel <$> [0..100::Int])
  widgetTree = vstack [
      label (showt $ model ^. clickCount),
      textField textField1 `style` bgColor lightGray,
      hstack [
        radio fruit Apple,
        radio fruit Orange,
        radio fruit Pear
      ],
      hstack [
        checkbox condition1,
        checkbox condition2,
        checkbox condition3,
        checkbox condition1
      ],
        --`style` bgColor lightGray <> textSize 40
        --`focus` bgColor darkGray <> textSize 400,
      --hstack labels `key` "Labels",
      --hstack [
      --  label "Label 1",
      --  label "Label 2"
      --],
      listView textField1 items id,
      button IncButton "Click!"
    ] `key` "Main"
  widgetTree3 = vstack
    [hstack [
      radio fruit Apple,
      radio fruit Orange,
      radio fruit Pear
    ]]
  newLabel i = label ("New: " <> showt i) `style` altColor i
  altColor i = bgColor (if even i then gray else darkGray)
  labels = newLabel <$> [0..(model ^. clickCount - 1)]
  items = fmap showt [1..100::Int]
