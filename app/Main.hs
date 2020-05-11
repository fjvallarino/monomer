{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.State
import Data.Default
import Foreign.C.Types
import Lens.Micro
import NanoVG (Context(..), createGL3, CreateFlags(..), createFont, FileName(..), beginFrame, endFrame)
import SDL (($=))

import System.Remote.Monitoring

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Foreign.C.String as STR
import qualified SDL
import qualified SDL.Raw.Error as SRE
import qualified SDL.Raw.Event as SREv

import Monomer.Common.Style
import Monomer.Common.Types
import Monomer.Graphics.Color
import Monomer.Graphics.Types
import Monomer.Main.Core
import Monomer.Main.Platform
import Monomer.Main.Types
import Monomer.Main.Util
import Monomer.Widget.Core
import Monomer.Widget.Util
import Monomer.Widgets

import Types

foreign import ccall unsafe "initGlew" glewInit :: IO CInt

data AppEvent = RunLongTask | PrintTextFields | IncreaseCount Int | UpdateText3 T.Text deriving (Show, Eq)

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

--buildUI :: App -> WidgetTree
buildUI model = styledTree where
  border1 = border 5 (rgb 0 255 0) 20
  border2 = borderLeft 20 (rgb 200 200 0) <> borderRight 20 (rgb 200 0 200)
  buttonStyle = bgColor (rgb 0 0 255) <> textSize 64 <> border1 <> border2 <> bgRadius 20
  labelStyle = bgColor (rgb 100 100 100) <> textSize 48
  textStyle = textColor (rgb 0 255 0) <> textAlignH ACenter
  extraWidgets = map (\i -> sandbox (IncreaseCount (10 + i))) [1..(_clickCount model)]
  widgetTree = vstack [
      hstack [
        (scroll $ vstack [
          textField textField1 `style` textStyle,
          spacer `visible` False,
          label "Label 1",
          spacer,
          label "Label 2",
          spacer `visible` False,
          label "Label 3" `visible` False,
          spacer `visible` False,
          label "Label 4",
          spacer,
          label "Label 5",
          spacer,
          label "Label 6",
          spacer,
          label "Label 7",
          spacer,
          label "Label 8",
          spacer,
          label "Label 9",
          spacer,
          label "Label 10",
          spacer,
          label "Label 11",
          spacer,
          label "Label 12"
        ]) `style` (swidth 400 <> sheight 300),
        vstack [
          textField textField2 `style` textStyle,
          scroll $ label "This is a really really really long label, you know?" `style` labelStyle
        ]
      ],
      vgrid ([
        scroll $ hstack [
          label "Short",
          spacer,
          label "Long",
          spacer,
          label "Very Long",
          spacer,
          label "Very Very Long",
          spacer,
          label "Very Very Very Long",
          spacer,
          label "Very Very Very Very Long"
        ],
        hstack [
          sandbox RunLongTask `style` buttonStyle,
          sandbox PrintTextFields `style` buttonStyle,
          sandbox (IncreaseCount 2) `style` buttonStyle
        ],
        button "Add items" (IncreaseCount 1) `style` buttonStyle,
        textField textField3 `style` textStyle
      ] ++ extraWidgets)
    ]
  styledTree = cascadeStyle mempty widgetTree
