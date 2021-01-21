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
import Monomer.Core.Combinators

import Monomer.Widgets.Box
import Monomer.Widgets.Button
import Monomer.Widgets.Composite
import Monomer.Widgets.Icon
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

data AlertEvt e
  = ParentEvt e
  | VisibleChanged
  deriving (Eq, Show)

alert
  :: (WidgetModel sp, WidgetEvent ep)
  => Text
  -> ep
  -> WidgetNode sp ep
alert message evt = alert_ message evt def

alert_
  :: (WidgetModel sp, WidgetEvent ep)
  => Text
  -> ep
  -> [AlertCfg]
  -> WidgetNode sp ep
alert_ message evt configs = newNode where
  config = mconcat configs
  createUI = buildUI message evt config
  newNode = compositeExt "alert" () createUI handleEvent

buildUI
  :: Text
  -> ep
  -> AlertCfg
  -> WidgetEnv s (AlertEvt ep)
  -> s
  -> WidgetNode s (AlertEvt ep)
buildUI message pCancelEvt config wenv model = alertBox where
  cancelEvt = ParentEvt pCancelEvt
  title = fromMaybe "" (_alcTitle config)
  close = fromMaybe "Close" (_alcClose config)
  emptyOverlayColor = themeEmptyOverlayColor wenv
  dismissButton = mainButton close cancelEvt
  closeIcon = icon IconClose & L.info . L.style .~ themeDialogCloseIcon wenv
  alertTree = vstack [
      hstack [
        label title & L.info . L.style .~ themeDialogTitle wenv,
        box_ closeIcon [onClick cancelEvt]
      ],
      label_ message [textMultiLine]
        & L.info . L.style .~ themeDialogBody wenv,
      box_ dismissButton [alignLeft]
        & L.info . L.style .~ themeDialogButtons wenv
    ] & L.info . L.style .~ themeDialogFrame wenv
  alertBox = box_ alertTree [onClickEmpty cancelEvt]
    & L.info . L.style .~ emptyOverlayColor

handleEvent
  :: WidgetEnv s (AlertEvt ep)
  -> s
  -> AlertEvt ep
  -> [EventResponse s e ep]
handleEvent wenv model evt = case evt of
  ParentEvt pevt -> [Report pevt]
  VisibleChanged -> []
