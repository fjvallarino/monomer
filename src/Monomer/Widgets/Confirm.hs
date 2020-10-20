{-# LANGUAGE RankNTypes #-}

module Monomer.Widgets.Confirm (
  confirm,
  confirm_
) where

import Control.Applicative ((<|>))
import Control.Lens (Lens', (&), (^.), (^?), (.~), (?~), (<>~), non)
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

data ConfirmCfg = ConfirmCfg {
  _cfcTitle :: Maybe Text,
  _cfcAccept :: Maybe Text,
  _cfcCancel :: Maybe Text
}

instance Default ConfirmCfg where
  def = ConfirmCfg {
    _cfcTitle = Nothing,
    _cfcAccept = Nothing,
    _cfcCancel = Nothing
  }

instance Semigroup ConfirmCfg where
  (<>) a1 a2 = ConfirmCfg {
    _cfcTitle = _cfcTitle a2 <|> _cfcTitle a1,
    _cfcAccept = _cfcAccept a2 <|> _cfcAccept a1,
    _cfcCancel = _cfcCancel a2 <|> _cfcCancel a1
  }

instance Monoid ConfirmCfg where
  mempty = def

confirm :: Text -> e -> e -> WidgetInstance s e
confirm message acceptEvt cancelEvt = createThemed "confirm" factory where
  factory wenv = confirm_ wenv message acceptEvt cancelEvt def

confirm_ :: WidgetEnv s e -> Text -> e -> e -> ConfirmCfg -> WidgetInstance s e
confirm_ wenv message acceptEvt cancelEvt config = confirmBox where
  title = fromMaybe "" (_cfcTitle config)
  accept = fromMaybe "Accept" (_cfcAccept config)
  cancel = fromMaybe "Cancel" (_cfcCancel config)
  emptyOverlayColor = themeEmptyOverlayColor wenv
  acceptBtn = button accept acceptEvt & L.style .~ themeBtnMain wenv
  cancelBtn = button cancel cancelEvt & L.style .~ themeBtn wenv
  buttons = hstack [ acceptBtn, spacer, cancelBtn ] `style` [bgColor red]
  confirmTree = vstack [
      label title & L.style .~ themeDialogTitle wenv,
      label message & L.style .~ themeDialogBody wenv,
      box_ buttons [alignLeft] & L.style <>~ themeDialogButtons wenv
    ] & L.style .~ themeDialogFrame wenv
  confirmBox = box_ confirmTree [onClickEmpty cancelEvt]
    & L.style .~ emptyOverlayColor
