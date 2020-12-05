{-# LANGUAGE RankNTypes #-}

module Monomer.Widgets.Alert (
  alert,
  alert_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (.~))
import Data.Default
import Data.Maybe
import Data.Text (Text)

import Monomer.Core
import Monomer.Event
import Monomer.Graphics

import Monomer.Widgets.Box
import Monomer.Widgets.Button
import Monomer.Widgets.Container
import Monomer.Widgets.Label
import Monomer.Widgets.Spacer
import Monomer.Widgets.Stack

import qualified Monomer.Lens as L

data AlertCfg = AlertCfg {
  _alcTitle :: Maybe Text,
  _alcClose :: Maybe Text
}

instance Default AlertCfg where
  def = AlertCfg {
    _alcTitle = Nothing,
    _alcClose = Nothing
  }

instance Semigroup AlertCfg where
  (<>) a1 a2 = AlertCfg {
    _alcTitle = _alcTitle a2 <|> _alcTitle a1,
    _alcClose = _alcClose a2 <|> _alcClose a1
  }

instance Monoid AlertCfg where
  mempty = def

instance CmbCloseCaption AlertCfg where
  closeCaption t = def {
    _alcClose = Just t
  }

alert :: Text -> e -> WidgetNode s e
alert message evt = alert_ message evt def

alert_ :: Text -> e -> [AlertCfg] -> WidgetNode s e
alert_ message evt configs = createThemed "alert" factory where
  config = mconcat configs
  factory wenv = makeAlert wenv message evt config

makeAlert :: WidgetEnv s e -> Text -> e -> AlertCfg -> WidgetNode s e
makeAlert wenv message evt config = alertBox where
  title = fromMaybe "" (_alcTitle config)
  close = fromMaybe "Close" (_alcClose config)
  emptyOverlayColor = themeEmptyOverlayColor wenv
  dismissButton = mainButton close evt
  alertTree = vstack [
      label title
        & L.widgetInstance . L.style .~ themeDialogTitle wenv,
      label_ message [textMultiLine]
        & L.widgetInstance . L.style .~ themeDialogBody wenv,
      box_ dismissButton [alignLeft]
        & L.widgetInstance . L.style .~ themeDialogButtons wenv
    ] & L.widgetInstance . L.style .~ themeDialogFrame wenv
  alertBox = box_ alertTree [onClickEmpty evt]
    & L.widgetInstance . L.style .~ emptyOverlayColor
