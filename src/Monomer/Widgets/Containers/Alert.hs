{-|
Module      : Monomer.Widgets.Containers.Alert
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Simple alert dialog, displaying a close button and optional title. Usually
embedded in a zstack component and displayed/hidden depending on context.

Config:

- titleCaption: the title of the alert dialog.
- closeCaption: the caption of the close button.
-}
module Monomer.Widgets.Containers.Alert (
  alert,
  alert_,
  alertMsg,
  alertMsg_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (.~))
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

instance CmbTitleCaption AlertCfg where
  titleCaption t = def {
    _alcTitle = Just t
  }

instance CmbCloseCaption AlertCfg where
  closeCaption t = def {
    _alcClose = Just t
  }

-- | Creates an alert dialog with the provided content.
alert
  :: (WidgetModel sp, WidgetEvent ep)
  => ep                -- ^ The event to raise when the dialog is closed.
  -> WidgetNode () ep  -- ^ The content to display in the dialog.
  -> WidgetNode sp ep  -- ^ The created dialog.
alert evt dialogBody = alert_ evt def dialogBody

-- | Creates an alert dialog with the provided content. Accepts config.
alert_
  :: (WidgetModel sp, WidgetEvent ep)
  => ep                -- ^ The event to raise when the dialog is closed.
  -> [AlertCfg]        -- ^ The config options for the dialog.
  -> WidgetNode () ep  -- ^ The content to display in the dialog.
  -> WidgetNode sp ep  -- ^ The created dialog.
alert_ evt configs dialogBody = newNode where
  config = mconcat configs
  createUI = buildUI (const dialogBody) evt config
  newNode = compositeD_ "alert" (WidgetValue ()) createUI handleEvent []

-- | Creates an alert dialog with a text message as content.
alertMsg
  :: (WidgetModel sp, WidgetEvent ep)
  => Text              -- ^ The message to display.
  -> ep                -- ^ The event to raise when the dialog is closed.
  -> WidgetNode sp ep  -- ^ The created dialog.
alertMsg message evt = alertMsg_ message evt def

-- | Creates an alert dialog with a text message as content. Accepts config.
alertMsg_
  :: (WidgetModel sp, WidgetEvent ep)
  => Text              -- ^ The message to display.
  -> ep                -- ^ The event to raise when the dialog is closed.
  -> [AlertCfg]        -- ^ The config options for the dialog.
  -> WidgetNode sp ep  -- ^ The created dialog.
alertMsg_ message evt configs = newNode where
  config = mconcat configs
  dialogBody wenv = label_ message [multiLine]
    & L.info . L.style .~ collectTheme wenv L.dialogMsgBodyStyle
  createUI = buildUI dialogBody evt config
  newNode = compositeD_ "alert" (WidgetValue ()) createUI handleEvent []

buildUI
  :: (WidgetModel s, WidgetEvent ep)
  => (WidgetEnv s ep -> WidgetNode s ep)
  -> ep
  -> AlertCfg
  -> WidgetEnv s ep
  -> s
  -> WidgetNode s ep
buildUI dialogBody cancelEvt config wenv model = mainTree where
  title = fromMaybe "" (_alcTitle config)
  close = fromMaybe "Close" (_alcClose config)
  emptyOverlay = collectTheme wenv L.emptyOverlayStyle
  dismissButton = hstack [button close cancelEvt]
  closeIcon = icon_ IconClose [width 2]
    & L.info . L.style .~ collectTheme wenv L.dialogCloseIconStyle
  alertTree = vstack_ [sizeReqUpdater clearExtra] [
      hstack [
        label title & L.info . L.style .~ collectTheme wenv L.dialogTitleStyle,
        filler,
        box_ [alignTop, onClick cancelEvt] closeIcon
      ],
      dialogBody wenv,
      filler,
      box_ [alignRight] dismissButton
        & L.info . L.style .~ collectTheme wenv L.dialogButtonsStyle
    ] & L.info . L.style .~ collectTheme wenv L.dialogFrameStyle
  alertBox = box_ [onClickEmpty cancelEvt] alertTree
    & L.info . L.style .~ emptyOverlay
  mainTree = keystroke [("Esc", cancelEvt)] alertBox

handleEvent
  :: WidgetEnv s ep
  -> WidgetNode s ep
  -> s
  -> ep
  -> [EventResponse s e sp ep]
handleEvent wenv node model evt = [Report evt]
