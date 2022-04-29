{-|
Module      : Monomer.Widgets.Containers.Confirm
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Simple confirm dialog, displaying accept and close buttons and optional title.
Usually embedded in a zstack component and displayed/hidden depending on
context.

Similar to 'Monomer.Widgets.Containers.Alert', but takes two events to handle
the Accept and Cancel actions.

A simple text message can be displayed with 'confirmMsg', providing the message
text and the events to generate when the user interacts with the dialog:

@
confirmMsg "Save changes?" ConfirmAcceptEvent ConfirmCancelEvent
@

Alternatively, a custom widget can be provided to display as content:

@
customConfirm = confirm ConfirmAcceptEvent ConfirmCancelEvent where
  content = hstack [
      label "Save changes?",
      filler,
      label fileName
    ]
@
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Containers.Confirm (
  -- * Configuration
  ConfirmCfg,
  -- * Constructors
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

{-|
Configuration options for confirm:

- 'titleCaption': the title of the alert dialog.
- 'acceptCaption': the caption of the accept button.
- 'closeCaption': the caption of the close button.
-}
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

-- | Creates a confirm dialog with the provided content.
confirm
  :: (WidgetModel s, WidgetEvent e)
  => e                             -- ^ The accept button event.
  -> e                             -- ^ The cancel button event.
  -> WidgetNode () (ConfirmEvt e)  -- ^ The content to display in the dialog.
  -> WidgetNode s e               -- ^ The created dialog.
confirm acceptEvt cancelEvt dialogBody = newNode where
  newNode = confirm_ acceptEvt cancelEvt def dialogBody

-- | Creates an alert dialog with the provided content. Accepts config.
confirm_
  :: (WidgetModel s, WidgetEvent e)
  => e                             -- ^ The accept button event.
  -> e                             -- ^ The cancel button event.
  -> [ConfirmCfg]                   -- ^ The config options for the dialog.
  -> WidgetNode () (ConfirmEvt e)  -- ^ The content to display in the dialog.
  -> WidgetNode s e               -- ^ The created dialog.
confirm_ acceptEvt cancelEvt configs dialogBody = newNode where
  config = mconcat configs
  createUI = buildUI (const dialogBody) acceptEvt cancelEvt config
  compCfg = [compositeMergeReqs mergeReqs]
  newNode = compositeD_ "confirm" (WidgetValue ()) createUI handleEvent compCfg

-- | Creates an alert dialog with a text message as content.
confirmMsg
  :: (WidgetModel s, WidgetEvent e)
  => Text              -- ^ The message to display in the dialog.
  -> e                -- ^ The accept button event.
  -> e                -- ^ The cancel button event.
  -> WidgetNode s e  -- ^ The created dialog.
confirmMsg msg acceptEvt cancelEvt = confirmMsg_ msg acceptEvt cancelEvt def

-- | Creates an alert dialog with a text message as content. Accepts config.
confirmMsg_
  :: (WidgetModel s, WidgetEvent e)
  => Text              -- ^ The message to display in the dialog.
  -> e                -- ^ The accept button event.
  -> e                -- ^ The cancel button event.
  -> [ConfirmCfg]      -- ^ The config options for the dialog.
  -> WidgetNode s e  -- ^ The created dialog.
confirmMsg_ message acceptEvt cancelEvt configs = newNode where
  config = mconcat configs
  dialogBody wenv = label_ message [multiline]
    & L.info . L.style .~ collectTheme wenv L.dialogMsgBodyStyle
  createUI = buildUI dialogBody acceptEvt cancelEvt config
  compCfg = [compositeMergeReqs mergeReqs]
  newNode = compositeD_ "confirm" (WidgetValue ()) createUI handleEvent compCfg

mergeReqs :: MergeReqsHandler s e sp
mergeReqs wenv newNode oldNode parentModel oldModel model = reqs where
  acceptPath = SetFocus <$> widgetIdFromKey wenv "acceptBtn"
  isVisible node = node ^. L.info . L.visible
  reqs
    | not (isVisible oldNode) && isVisible newNode = catMaybes [acceptPath]
    | otherwise = []

buildUI
  :: (WidgetModel s, WidgetEvent ep)
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
  emptyOverlay = collectTheme wenv L.emptyOverlayStyle

  acceptBtn = mainButton accept acceptEvt `nodeKey` "acceptBtn"
  cancelBtn = button cancel cancelEvt
  buttons = hstack [ acceptBtn, spacer, cancelBtn ]

  closeIcon = icon_ IconClose [width 2]
    & L.info . L.style .~ collectTheme wenv L.dialogCloseIconStyle
  confirmTree = vstack_ [sizeReqUpdater clearExtra] [
      hstack [
        label title & L.info . L.style .~ collectTheme wenv L.dialogTitleStyle,
        filler,
        box_ [alignTop, onClick cancelEvt] closeIcon
      ],
      dialogBody wenv,
      filler,
      box_ [alignRight] buttons
        & L.info . L.style <>~ collectTheme wenv L.dialogButtonsStyle
    ] & L.info . L.style .~ collectTheme wenv L.dialogFrameStyle
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
