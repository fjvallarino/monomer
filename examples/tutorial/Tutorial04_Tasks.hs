{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tutorial04_Tasks where

import Control.Lens
import Data.Text (Text)
import Monomer
import System.Random

import qualified Monomer.Lens as L

data AppModel = AppModel {
  _selected :: Int,
  _hoverButton :: Bool
} deriving (Eq, Show)

data AppEvent
  = AppGenRandom
  | AppSaveRandom Int
  | AppOnEnterBtn
  | AppOnLeaveBtn
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  pushLayers = zstack [
      image_ "./assets/images/red-button.png" [fitFill] `visible` not (model ^. hoverButton),
      image_ "./assets/images/red-button-hover.png" [fitFill] `visible` model ^. hoverButton,
      label "Push!" `styleBasic` [textFont "Bold", textSize 20, textCenter]
    ]
  pushButton = box_ [onClick AppGenRandom, onEnter AppOnEnterBtn, onLeave AppOnLeaveBtn] pushLayers
    `styleBasic` [width 160, height 160, cursorHand]
  numberLabel = labelS (model ^. selected)
    `styleBasic` [textFont "Bold", textSize 100, textColor black, textCenter, width 160]
  numberedImage url idx = scroll (image_ url [fitNone])
    `visible` (model ^. selected == idx)
  imageSet = hstack [
      numberedImage "https://picsum.photos/id/1020/800/600" 1,
      numberedImage "https://picsum.photos/id/1047/800/600" 2,
      numberedImage "https://picsum.photos/id/1047/800/600" 3,
      numberedImage "https://picsum.photos/id/1025/800/600" 4,
      numberedImage "https://picsum.photos/id/1080/800/600" 5,
      numberedImage "https://picsum.photos/id/1059/800/600" 6
    ] `styleBasic` [padding 10]
  widgetTree = vstack [
      hstack [
        tooltip "Click to pick a random number" pushButton
          `styleBasic` [textSize 16, bgColor steelBlue, paddingH 5, radius 5],
        numberLabel
      ],
      imageSet
    ] `styleBasic` [bgColor moccasin]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppGenRandom -> [Task $
      AppSaveRandom <$> randomRIO (1, 6)
    ]
  AppSaveRandom value -> [Model $ model & selected .~ value]
  AppOnEnterBtn -> [Model $ model & hoverButton .~ True]
  AppOnLeaveBtn -> [Model $ model & hoverButton .~ False]

main04 :: IO ()
main04 = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Tutorial 04 - Tasks",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf"
      ]
    model = AppModel {
      _selected = 0,
      _hoverButton = False
    }
