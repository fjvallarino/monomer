{-|
Module      : Main
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Main module for the 'Custom OpenGL' example.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Text (Text)
import Monomer

import qualified Data.Map as M
import qualified Monomer.Lens as L

import OpenGLWidget

data AppModel = AppModel {
  _color1 :: Color,
  _color2 :: Color,
  _color3 :: Color,
  _color4 :: Color
} deriving (Eq, Show)

data AppEvent
  = AppInit
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  colorsMap = M.fromList [(red, "Red"), (green, "Green"), (blue, "Blue"), (orange, "Orange")]
  colors = M.keys colorsMap
  colorDropdown field = textDropdown_ field colors (colorsMap M.!) []

  widgetTree = vstack [
      hstack [
        label "Color 1:",
        spacer,
        colorDropdown color1,
        spacer,
        label "Color 2:",
        spacer,
        colorDropdown color2,
        spacer,
        label "Color 3:",
        spacer,
        colorDropdown color3,
        spacer,
        label "Color 4:",
        spacer,
        colorDropdown color4
      ],
      spacer,
      vgrid [
        hgrid [
          openGLWidget (model ^. color1)
            `styleBasic` [padding 20],
          scroll (openGLWidget (model ^. color2) `styleBasic` [width 800, height 800])
            `styleBasic` [padding 20]
        ],
        hgrid [
          openGLWidget (model ^. color3)
            `styleBasic` [padding 20],
          openGLWidget (model ^. color4)
            `styleBasic` [padding 20]
        ]
      ]
    ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "OpenGL",
      appWindowIcon "./assets/images/icon.bmp",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    model = AppModel red green blue orange
