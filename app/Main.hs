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
import SDL (($=))
import TextShow

import System.Remote.Monitoring

import qualified Foreign.C.String as STR
import qualified SDL
import qualified SDL.Raw.Error as SRE

import Monomer.Common.Geometry
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

  winSize <- getDrawableSize window

  let model = def  {
    _textField1 = "This is a test! Or not? Well, we'll see"
  }
  let devicePixelRate = _sW winSize / fromIntegral screenWidth
  let appWidget = createApp model (Just InitApp) handleAppEvent buildUI
  let monomerContext = initMonomerContext () winSize useHiDPI devicePixelRate
  let theme = def
        & S.basic . S.fgColor .~ blue
        & S.hover . S.fgColor .~ white
        & S.focus . S.fgColor .~ white

  runStateT (runApp window theme appWidget) monomerContext

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
  PrintMessage txt -> Task $ do
    print txt
    return Nothing
  CheckboxSt st -> Task $ do
    putStrLn $ "Checkbox is: " ++ show st
    return Nothing
--  RadioSt st -> Task $ do
--    putStrLn $ "Radio is: " ++ show st
--    return Nothing
  RadioSt st -> Model (model & fruit .~ st)
  _ -> Model model

buildUI model = trace "Creating UI" widgetTree where
--  widgetTree1 = scroll $ vstack (newLabel <$> [0..100::Int])
--  widgetTree2 = vstack [
--      label (showt $ model ^. clickCount),
--      textField textField1 `style` bgColor lightGray,
--      hstack [
--        radio fruit Apple,
--        radio fruit Orange,
--        radio fruit Pear
--      ],
--      hstack [
--        checkbox condition1,
--        checkbox condition2,
--        checkbox condition3,
--        checkbox condition1
--      ],
--        --`style` bgColor lightGray <> textSize 40
--        --`focus` bgColor darkGray <> textSize 400,
--      --hstack labels `key` "Labels",
--      --hstack [
--      --  label "Label 1",
--      --  label "Label 2"
--      --],
--      listView textField1 items id,
--      button IncButton "Click!"
--    ] `key` "Main"
  widgetTree = vstack [
      hstack [
        radioV (model ^. fruit) RadioSt Apple,
        radioV (model ^. fruit) RadioSt Orange,
        radioV (model ^. fruit) RadioSt Pear
      ],
      hstack [
        checkbox condition1,
        checkbox condition2,
        checkbox condition3,
        checkbox_ condition1 [onChange CheckboxSt]
      ],
      hstack [
        label_ "This is a really long label used to check what I did works fine" [textEllipsis],
        label "Short label"
      ],
      label "Text",
      textField_ textField2 [validInput validText2, maxLength 10, onChange PrintMessage, selectOnFocus True]
        `style` if model ^. validText2 then def else [border 1 red],
      label "Floating",
      floatingField_ float1 [validInput validFloat1]
        `style` if model ^. validFloat1 then def else [border 1 red],
      label "Integer",
      integralField_ word1 [validInput validWord1, maxValue 100]
        `style` if model ^. validWord1 then def else [border 1 red],
      integralField_ int1 [validInput validInt1, maxValue 100]
        `style` if model ^. validInt1 then def else [border 1 red],
      integralField_ integer1 [validInput validInteger1, minValue 10, maxValue 100]
        `style` if model ^. validInteger1 then def else [border 1 red],
      listView textField1 items label
    ] `style` [borderT 20 red, borderL 10 blue, borderR 10 green, borderB 10 gray, iradius 50] --, padding 20
  newLabel i = label ("New: " <> showt i) `style` [altColor i]
  altColor i = bgColor (if even i then gray else darkGray)
  labels = newLabel <$> [0..(model ^. clickCount - 1)]
  items = fmap showt [1..100::Int]
