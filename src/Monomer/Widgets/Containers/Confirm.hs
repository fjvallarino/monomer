{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Monomer.Widgets.Containers.Confirm (
  ConfirmEvt(..),
  confirm,
  confirm_,
  confirmMsg,
  confirmMsg_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~), (<>~))
import Data.Default
import Data.Maybe
import Data.Text (Text)

import Monomer.Core
import Monomer.Core.Combinators

import Monomer.Widgets.Composite
import Monomer.Widgets.Containers.Box
import Monomer.Widgets.Containers.Keystroke
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.Button
import Monomer.Widgets.Singles.Icon
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.Spacer

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

instance CmbTitleCaption ConfirmCfg where
  titleCaption t = def {
    _cfcTitle = Just t
  }

instance CmbAcceptCaption ConfirmCfg where
  acceptCaption t = def {
    _cfcAccept = Just t
  }

instance CmbCancelCaption ConfirmCfg where
  cancelCaption t = def {
    _cfcCancel = Just t
  }

newtype ConfirmEvt e
  = ConfirmParentEvt e
  deriving (Eq, Show)

confirm
  :: (WidgetModel sp, WidgetEvent ep)
  => WidgetNode () (ConfirmEvt ep)
  -> ep
  -> ep
  -> WidgetNode sp ep
confirm dialogBody acceptEvt cancelEvt = newNode where
  newNode = confirm_ dialogBody acceptEvt cancelEvt def

confirm_
  :: (WidgetModel sp, WidgetEvent ep)
  => WidgetNode () (ConfirmEvt ep)
  -> ep
  -> ep
  -> [ConfirmCfg]
  -> WidgetNode sp ep
confirm_ dialogBody acceptEvt cancelEvt configs = newNode where
  config = mconcat configs
  createUI = buildUI (const dialogBody) acceptEvt cancelEvt config
  compCfg = [compositeMergeReqs mergeReqs]
  newNode = compositeExt_ "confirm" () createUI handleEvent compCfg

confirmMsg
  :: (WidgetModel sp, WidgetEvent ep)
  => Text
  -> ep
  -> ep
  -> WidgetNode sp ep
confirmMsg msg acceptEvt cancelEvt = confirmMsg_ msg acceptEvt cancelEvt def

confirmMsg_
  :: (WidgetModel sp, WidgetEvent ep)
  => Text
  -> ep
  -> ep
  -> [ConfirmCfg]
  -> WidgetNode sp ep
confirmMsg_ message acceptEvt cancelEvt configs = newNode where
  config = mconcat configs
  dialogBody wenv = label_ message [multiLine]
    & L.info . L.style .~ themeDialogMsgBody wenv
  createUI = buildUI dialogBody acceptEvt cancelEvt config
  compCfg = [compositeMergeReqs mergeReqs]
  newNode = compositeExt_ "confirm" () createUI handleEvent compCfg

mergeReqs :: MergeReqsHandler s e
mergeReqs wenv newNode oldNode model = reqs where
  acceptPath = SetFocus <$> globalKeyWidgetId wenv "acceptBtn"
  isVisible node = node ^. L.info . L.visible
  reqs
    | not (isVisible oldNode) && isVisible newNode = catMaybes [acceptPath]
    | otherwise = []

buildUI
  :: WidgetEvent ep
  => (WidgetEnv s (ConfirmEvt ep) -> WidgetNode s (ConfirmEvt ep))
  -> ep
  -> ep
  -> ConfirmCfg
  -> WidgetEnv s (ConfirmEvt ep)
  -> s
  -> WidgetNode s (ConfirmEvt ep)
buildUI dialogBody pAcceptEvt pCancelEvt config wenv model = mainTree where
  acceptEvt = ConfirmParentEvt pAcceptEvt
  cancelEvt = ConfirmParentEvt pCancelEvt
  title = fromMaybe "" (_cfcTitle config)
  accept = fromMaybe "Accept" (_cfcAccept config)
  cancel = fromMaybe "Cancel" (_cfcCancel config)
  emptyOverlay = themeEmptyOverlay wenv
  acceptBtn = mainButton accept acceptEvt `key` "acceptBtn"
  cancelBtn = button cancel cancelEvt
  buttons = hstack [ acceptBtn, spacer, cancelBtn ]
  closeIcon = icon IconClose & L.info . L.style .~ themeDialogCloseIcon wenv
  confirmTree = vstack_ [sizeReqUpdater clearExtra] [
      hstack [
        label title & L.info . L.style .~ themeDialogTitle wenv,
        filler,
        box_ [alignTop, onClick cancelEvt] closeIcon
      ],
      dialogBody wenv,
      filler,
      box_ [alignLeft] buttons
        & L.info . L.style <>~ themeDialogButtons wenv
    ] & L.info . L.style .~ themeDialogFrame wenv
  confirmBox = box_ [onClickEmpty cancelEvt] confirmTree
    & L.info . L.style .~ emptyOverlay
  mainTree = keystroke [("Esc", cancelEvt)] confirmBox

handleEvent
  :: WidgetEnv s (ConfirmEvt ep)
  -> WidgetNode s (ConfirmEvt ep)
  -> s
  -> ConfirmEvt ep
  -> [EventResponse s (ConfirmEvt ep) sp ep]
handleEvent wenv node model evt = case evt of
  ConfirmParentEvt pevt -> [Report pevt]
