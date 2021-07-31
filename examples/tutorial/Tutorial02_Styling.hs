{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tutorial02_Styling where

import Control.Lens
import Data.Text (Text)
import Monomer

import qualified Monomer.Lens as L

data AppModel = AppModel {
  _sampleText :: Text,
  _showPicker :: Bool,
  _fontName :: Font,
  _fontSize :: Double,
  _fontColor :: Color
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
  widgetTree = vstack [
      titleText "Text",
      box (textField sampleText) `styleBasic` [paddingV 10],

      titleText "Font name",
      hgrid [
        hstack [
          label "Regular: ",
          radio "Regular" fontName,
          filler
        ],
        hstack [
          label "Bold: ",
          radio "Bold" fontName,
          filler
        ],
        hstack [
          label "Italic: ",
          radio "Italic" fontName,
          filler
        ]
      ] `styleBasic` [paddingV 10],

      titleText "Font size",
      hslider fontSize 10 200
        `styleBasic` [paddingV 10, fgColor orange],

      titleText "Font color",
      hstack [
        labeledCheckbox "Show color picker " showPicker,
        filler
      ] `styleBasic` [paddingT 10, paddingB 5],
      colorPicker fontColor
        `visible` (model ^. showPicker)
        `styleBasic` [paddingB 10],

      sampleTextLabel
    ] `styleBasic` [padding 10]

  titleText text = label text
    `styleBasic` [textFont "Medium", textSize 20]

  sampleTextLabel = label_ (model ^. sampleText) [ellipsis]
    `styleBasic` [
      bgColor dimGray,
      border 4 lightGray,
      radius 10,
      textFont (model ^. fontName),
      textSize (model ^. fontSize),
      textColor (model ^. fontColor),
      textCenter,
      flexHeight 100]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []

main02 :: IO ()
main02 = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Tutorial 02 - Styling",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Medium" "./assets/fonts/Roboto-Medium.ttf",
      appFontDef "Italic" "./assets/fonts/Roboto-Italic.ttf",
      appInitEvent AppInit
      ]
    model = AppModel {
      _sampleText = "Hello World!",
      _showPicker = False,
      _fontName = "Regular",
      _fontSize = 24,
      _fontColor = white
    }
