{-|
Module      : Tutorial08_Themes
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Main module for the '08 - Themes' tutorial.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tutorial08_Themes where

import Control.Lens
import Data.Text (Text)
import Monomer
import Monomer.Core.Themes.BaseTheme
import TextShow

import qualified Monomer.Lens as L

data ActiveTheme
  = DarkTheme
  | LightTheme
  | CustomTheme
  deriving (Eq, Enum, Show)

data AppModel = AppModel {
  _clickCount :: Int,
  _checked :: Bool,
  _activeTheme :: ActiveTheme
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppIncrease
  | AppDecrease
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  theme = case model ^. activeTheme of
    DarkTheme -> darkTheme
    LightTheme -> lightTheme
    CustomTheme -> customTheme
  widgetTree = themeSwitch_ theme [themeClearBg] $ vstack [
      hstack [
        label "Select theme:",
        spacer,
        textDropdownS activeTheme (enumFrom (toEnum 0))
      ],

      spacer,
      separatorLine,

      spacer,
      hstack [
        labeledCheckbox "Checkbox" checked,
        spacer,
        labeledRadio "Boolean radio (True)" True checked,
        spacer,
        labeledRadio "Boolean radio (False)" False checked
      ],

      spacer,
      hstack [
        box $ hslider clickCount 0 100,
        spacer,
        numericField_ clickCount [minValue 0, maxValue 100]
      ],

      spacer,
      hstack [
        label $ "Click count: " <> showt (model ^. clickCount),
        spacer,
        mainButton "Increase count" AppIncrease,
        spacer,
        button "Decrease count" AppDecrease
      ]
    ] `styleBasic` [padding 20]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppIncrease -> [Model (model & clickCount .~ min 100 (count + 1))]
  AppDecrease -> [Model (model & clickCount .~ max 0 (count - 1))]
  where
    count = model ^. clickCount

customTheme :: Theme
customTheme = baseTheme darkThemeColors {
  btnMainBgBasic = rgbHex "#EE9000",
  btnMainBgHover = rgbHex "#FFB522",
  btnMainBgFocus = rgbHex "#FFA500",
  btnMainBgActive = rgbHex "#DD8000",
  btnMainBgDisabled = rgbHex "#BB8800",
  btnMainText = rgbHex "000000"
}

main08 :: IO ()
main08 = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Tutorial 08 - Themes",
      appWindowIcon "./assets/images/icon.bmp",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    model = AppModel 0 False LightTheme
