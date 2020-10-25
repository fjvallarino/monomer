{-# LANGUAGE RankNTypes #-}

module Monomer.Widgets.Confirm (
  confirm,
  confirm_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (.~), (<>~))
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

instance AcceptCaption ConfirmCfg where
  acceptCaption t = def {
    _cfcAccept = Just t
  }

instance CancelCaption ConfirmCfg where
  cancelCaption t = def {
    _cfcCancel = Just t
  }

confirm :: Text -> e -> e -> WidgetInstance s e
confirm message acceptEvt cancelEvt = confirm_ message acceptEvt cancelEvt def

confirm_ :: Text -> e -> e -> [ConfirmCfg] -> WidgetInstance s e
confirm_ message acceptEvt cancelEvt configs = newInst where
  config = mconcat configs
  factory wenv = makeConfirm wenv message acceptEvt cancelEvt config
  newInst = createThemed "confirm" factory

makeConfirm :: WidgetEnv s e -> Text -> e -> e -> ConfirmCfg -> WidgetInstance s e
makeConfirm wenv message acceptEvt cancelEvt config = confirmBox where
  title = fromMaybe "" (_cfcTitle config)
  accept = fromMaybe "Accept" (_cfcAccept config)
  cancel = fromMaybe "Cancel" (_cfcCancel config)
  emptyOverlayColor = themeEmptyOverlayColor wenv
  acceptBtn = mainButton accept acceptEvt
  cancelBtn = button cancel cancelEvt
  buttons = hstack [ acceptBtn, spacer, cancelBtn ]
  confirmTree = vstack [
      label title & L.style .~ themeDialogTitle wenv,
      label message & L.style .~ themeDialogBody wenv,
      box_ buttons [alignLeft] & L.style <>~ themeDialogButtons wenv
    ] & L.style .~ themeDialogFrame wenv
  confirmBox = box_ confirmTree [onClickEmpty cancelEvt]
    & L.style .~ emptyOverlayColor
