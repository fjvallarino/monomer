{-|
Module      : Monomer.Widgets.Singles.ToggleButton
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Toggle button widget, used for boolean values.

@
toggleButton \"Toggle\" booleanLens
@

Its behavior is equivalent to "Monomer.Widgets.Singles.Checkbox" and
"Monomer.Widgets.Singles.LabeledCheckbox", with a different visual
representation.

See "Monomer.Widgets.Singles.OptionButton" for detailed notes.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Monomer.Widgets.Singles.ToggleButton (
  -- * Configuration
  ToggleButtonCfg,
  toggleButtonOffStyle,
  -- * Constructors
  toggleButton,
  toggleButton_,
  toggleButtonV,
  toggleButtonV_,
  toggleButtonD_
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (^?), (.~), (?~), _Just)
import Data.Default
import Data.Text (Text)

import qualified Data.Sequence as Seq

import Monomer.Widgets.Single
import Monomer.Widgets.Singles.OptionButton

import qualified Monomer.Lens as L

{-|
Configuration options for toggleButton:

- 'ignoreTheme': whether to load default style from theme or start empty.
- 'toggleButtonOffStyle': style to use when the option is not active.
- 'trimSpaces': whether to remove leading/trailing spaces in the caption.
- 'ellipsis': if ellipsis should be used for overflown text.
- 'multiline': if text may be split in multiple lines.
- 'maxLines': maximum number of text lines to show.
- 'resizeFactor': flexibility to have more or less spaced assigned.
- 'resizeFactorW': flexibility to have more or less horizontal spaced assigned.
- 'resizeFactorH': flexibility to have more or less vertical spaced assigned.
- 'onFocus': event to raise when focus is received.
- 'onFocusReq': 'WidgetRequest' to generate when focus is received.
- 'onBlur': event to raise when focus is lost.
- 'onBlurReq': 'WidgetRequest' to generate when focus is lost.
- 'onClick': event to raise when the value is clicked.
- 'onClickReq': 'WidgetRequest' to generate when the value is clicked.
- 'onChange': event to raise when the value changes.
- 'onChangeReq': 'WidgetRequest' to generate when the value changes.
-}
type ToggleButtonCfg s e = OptionButtonCfg s e Bool

-- | Sets the style for the Off state of the toggle button.
toggleButtonOffStyle :: Style -> ToggleButtonCfg s e
toggleButtonOffStyle = optionButtonOffStyle

-- | Creates a toggleButton using the given lens.
toggleButton
  :: Text            -- ^ The caption.
  -> ALens' s Bool   -- ^ The lens into the model.
  -> WidgetNode s e  -- ^ The created toggle button.
toggleButton caption field = toggleButton_ caption field def

-- | Creates a toggleButton using the given lens. Accepts config.
toggleButton_
  :: Text                   -- ^ The caption.
  -> ALens' s Bool          -- ^ The lens into the model.
  -> [ToggleButtonCfg s e]  -- ^ The config options.
  -> WidgetNode s e         -- ^ The created toggle button.
toggleButton_ caption field cfgs = newNode where
  newNode = toggleButtonD_ caption (WidgetLens field) cfgs

-- | Creates a toggleButton using the given value and 'onChange' event handler.
toggleButtonV
  :: WidgetEvent e
  => Text            -- ^ The caption.
  -> Bool            -- ^ The current value.
  -> (Bool -> e)     -- ^ The event to raise on change.
  -> WidgetNode s e  -- ^ The created toggle button.
toggleButtonV caption value handler = newNode where
  newNode = toggleButtonV_ caption value handler def

-- | Creates a toggleButton using the given value and 'onChange' event handler.
--   Accepts config.
toggleButtonV_
  :: WidgetEvent e
  => Text                   -- ^ The caption.
  -> Bool                   -- ^ The current value.
  -> (Bool -> e)            -- ^ The event to raise on change.
  -> [ToggleButtonCfg s e]  -- ^ The config options.
  -> WidgetNode s e         -- ^ The created toggle button.
toggleButtonV_ caption value handler configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newNode = toggleButtonD_ caption widgetData newConfigs

-- | Creates a toggleButton providing a 'WidgetData' instance and config.
toggleButtonD_
  :: Text                   -- ^ The caption.
  -> WidgetData s Bool      -- ^ The 'WidgetData' to retrieve the value from.
  -> [ToggleButtonCfg s e]  -- ^ The config options.
  -> WidgetNode s e         -- ^ The created toggle button.
toggleButtonD_ caption widgetData configs = toggleButtonNode where
  config = mconcat configs
  makeWithStyle = makeOptionButton L.toggleBtnOnStyle L.toggleBtnOffStyle
  widget = makeWithStyle widgetData caption id not config
  toggleButtonNode = defaultWidgetNode "toggleButton" widget
    & L.info . L.focusable .~ True
