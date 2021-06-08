{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tutorial02 where

import Control.Lens
import Data.Text (Text)
import Monomer
import System.Random

import qualified Monomer.Lens as L

newtype AppModel = AppModel {
  _selected :: Int
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppGenRandom
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
    `style` [width 160, height 160, cursorIcon CursorHand]
  numberLabel = labelS (model ^. selected)
    `style` [textFont "Bold", textSize 100, textColor black, textCenter, width 160]
  numberedImage url idx = scroll (image_ url [fitFill])
    `visible` (model ^. selected == idx)
  imageSet = hstack [
      numberedImage "https://picsum.photos/id/1020/800/600" 1,
      numberedImage "https://picsum.photos/id/1047/800/600" 2,
      numberedImage "https://picsum.photos/id/1051/800/600" 3
    ] `style` [padding 10]
  widgetTree = vstack [
      hstack [
        pushButton,
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
  AppInit -> []
  AppGenRandom -> [Task $
      AppSaveRandom <$> randomRIO (1, 3)
    ]
  AppSaveRandom value -> [Model $ model & selected .~ value]

main02 :: IO ()
main02 = do
  simpleApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Tutorial 02",
      appTheme darkTheme,
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appInitEvent AppInit
      ]
    model = AppModel {
      _selected = 0
    }
