{-|
Module      : Monomer.Widgets.Singles.ToggleButton
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Toggle button widget, used for boolean properties.

Its behavior is equivalent to 'Monomer.Widgets.Singles.Checkbox' and
'Monomer.Widgets.Singles.LabeledCheckbox', with a different visual
representation.

See 'OptionButton' for detailed notes.
-}
{-# LANGUAGE BangPatterns #-}
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
- 'onChange': event to raise when the value changes/is clicked.
- 'onChangeReq': 'WidgetRequest' to generate when the value changes/is clicked.
-}
type ToggleButtonCfg = OptionButtonCfg

-- | Sets the style for the Off state of the toggle button.
toggleButtonOffStyle :: Style -> ToggleButtonCfg s e a
toggleButtonOffStyle = optionButtonOffStyle

-- | Creates a toggleButton using the given lens.
toggleButton
  :: Text
  -> ALens' s Bool
  -> WidgetNode s e
toggleButton caption field = toggleButton_ caption field def

-- | Creates a toggleButton using the given lens. Accepts config.
toggleButton_
  :: Text
  -> ALens' s Bool
  -> [ToggleButtonCfg s e Bool]
  -> WidgetNode s e
toggleButton_ caption field cfgs = newNode where
  newNode = toggleButtonD_ caption (WidgetLens field) cfgs

-- | Creates a toggleButton using the given value and 'onChange' event handler.
toggleButtonV
  :: WidgetEvent e
  => Text
  -> Bool
  -> (Bool -> e)
  -> WidgetNode s e
toggleButtonV caption value handler = newNode where
  newNode = toggleButtonV_ caption value handler def

-- | Creates a toggleButton using the given value and 'onChange' event handler.
--   Accepts config.
toggleButtonV_
  :: WidgetEvent e
  => Text
  -> Bool
  -> (Bool -> e)
  -> [ToggleButtonCfg s e Bool]
  -> WidgetNode s e
toggleButtonV_ caption value handler configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newNode = toggleButtonD_ caption widgetData newConfigs

-- | Creates a toggleButton providing a 'WidgetData' instance and config.
toggleButtonD_
  :: Text
  -> WidgetData s Bool
  -> [ToggleButtonCfg s e Bool]
  -> WidgetNode s e
toggleButtonD_ caption widgetData configs = toggleButtonNode where
  config = mconcat configs
  makeWithStyle = makeOptionButton L.toggleBtnOnStyle L.toggleBtnOffStyle
  widget = makeWithStyle widgetData caption id not config
  toggleButtonNode = defaultWidgetNode "toggleButton" widget
    & L.info . L.focusable .~ True
