{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tutorial01 where

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
      titleText "Font name",
      hgrid [
        hstack [
          label "Regular: ",
          radio fontName "Regular",
          filler
        ],
        hstack [
          label "Bold: ",
          radio fontName "Bold",
          filler
        ],
        hstack [
          label "Italic: ",
          radio fontName "Italic",
          filler
        ]
      ] `style` [paddingV 10],
      titleText "Font name",
      hslider fontSize 10 200
        `style` [paddingV 10, fgColor orange],
      titleText "Font color",
      hstack [
        label "Show color picker ",
        checkbox showPicker,
        filler
      ] `style` [paddingT 10, paddingB 5],
      colorPicker fontColor
        `visible` (model ^. showPicker)
        `style` [paddingB 10],
      sampleTextLabel
    ] `style` [padding 10]
  titleText text = label text
    `style` [textFont "Bold", textSize 20]
  sampleTextLabel = label_ (model ^. sampleText) [ellipsis]
    `style` [
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

main01 :: IO ()
main01 = do
  simpleApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Tutorial 01",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
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
