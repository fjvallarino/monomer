{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tutorial04_Tasks where

import Control.Lens
import Data.Text (Text)
import Monomer
import System.Random

import qualified Monomer.Lens as L

newtype AppModel = AppModel {
  _selected :: Int
} deriving (Eq, Show)

data AppEvent
  = AppGenRandom
  | AppSaveRandom Int
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  pushLayers = zstack [
      image_ "./assets/images/red-button.png" [fitFill],
      label "Push!" `style` [textFont "Bold", textSize 20, textCenter]
    ]
  pushButton = box_ [onClick AppGenRandom] pushLayers
    `style` [width 160, height 160, cursorHand]
  numberLabel = labelS (model ^. selected)
    `style` [textFont "Bold", textSize 100, textColor black, textCenter, width 160]
  numberedImage url idx = scroll (image_ url [fitFill])
    `visible` (model ^. selected == idx)
  imageSet = hstack [
      numberedImage "https://picsum.photos/id/1020/800/600" 1,
      numberedImage "https://picsum.photos/id/1047/800/600" 2,
      numberedImage "https://picsum.photos/id/1047/800/600" 3,
      numberedImage "https://picsum.photos/id/1025/800/600" 4,
      numberedImage "https://picsum.photos/id/1080/800/600" 5,
      numberedImage "https://picsum.photos/id/1059/800/600" 6
    ] `style` [padding 10]
  widgetTree = vstack [
      hstack [
        tooltip "Click to pick a random number" pushButton
          `style` [textSize 16, bgColor steelBlue, paddingH 5, radius 5],
        numberLabel
      ],
      imageSet
    ] `style` [bgColor moccasin]

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
      _selected = 0
    }
