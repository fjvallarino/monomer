module Monomer.Widgets.Alert (
  alert,
  alert_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (.~))
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Data.Typeable (Typeable)

import Monomer.Core
import Monomer.Core.Combinators

import Monomer.Widgets.Box
import Monomer.Widgets.Button
import Monomer.Widgets.Composite
import Monomer.Widgets.Label
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

alert :: (Typeable s, Typeable e) => Text -> e -> WidgetNode s e
alert message evt = alert_ message evt def

alert_
  :: (Typeable s, Typeable e) => Text -> e -> [AlertCfg] -> WidgetNode s e
alert_ message evt configs = newNode where
  config = mconcat configs
  createUI = buildUI message evt config
  newNode = compositeExt "alert" () Nothing createUI handleEvent

buildUI :: Text -> e -> AlertCfg -> WidgetEnv s e -> s -> WidgetNode s e
buildUI message evt config wenv model = alertBox where
  title = fromMaybe "" (_alcTitle config)
  close = fromMaybe "Close" (_alcClose config)
  emptyOverlayColor = themeEmptyOverlayColor wenv
  dismissButton = mainButton close evt
  alertTree = vstack [
      label title
        & L.info . L.style .~ themeDialogTitle wenv,
      label_ message [textMultiLine]
        & L.info . L.style .~ themeDialogBody wenv,
      box_ dismissButton [alignLeft]
        & L.info . L.style .~ themeDialogButtons wenv
    ] & L.info . L.style .~ themeDialogFrame wenv
  alertBox = box_ alertTree [onClickEmpty evt]
    & L.info . L.style .~ emptyOverlayColor

handleEvent :: s -> e -> [EventResponse s e e]
handleEvent model evt = [Report evt]
