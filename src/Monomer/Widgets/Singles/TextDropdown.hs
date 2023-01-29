{-|
Module      : Monomer.Widgets.Singles.TextDropdown
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Dropdown widget, allowing selection of a single item from a collapsable list.
Both header and list content are text based.

@
textDropdown textLens ["Option 1", "Option 2", "Option 3"]
@

In case a customizable version is needed, to display rich content in the header
or list items, "Monomer.Widgets.Containers.Dropdown" can be used.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Singles.TextDropdown (
  -- * Configuration
  TextDropdownItem,
  -- * Constructors
  textDropdown,
  textDropdown_,
  textDropdownV,
  textDropdownV_,
  textDropdownS,
  textDropdownS_,
  textDropdownSV,
  textDropdownSV_
) where

import Control.Lens (ALens')
import Data.Default
import Data.Text (Text, pack)
import TextShow

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Widgets.Containers.Dropdown
import Monomer.Widgets.Singles.Label

-- | Constraints for an item handled by textDropdown.
type TextDropdownItem a = DropdownItem a

{-|
Creates a text dropdown using the given lens. The type must be have a 'TextShow'
instance.
-}
textDropdown
  :: (WidgetModel s, WidgetEvent e, Traversable t, TextDropdownItem a, TextShow a)
  => ALens' s a      -- ^ The lens into the model.
  -> t a             -- ^ The list of items.
  -> WidgetNode s e  -- ^ The created text dropdown.
textDropdown field items = newNode where
  newNode = textDropdown_ field items showt def

{-|
Creates a text dropdown using the given lens. Takes a function for converting
the type to Text. Accepts config.
-}
textDropdown_
  :: (WidgetModel s, WidgetEvent e, Traversable t, TextDropdownItem a)
  => ALens' s a           -- ^ The lens into the model.
  -> t a                  -- ^ The list of items.
  -> (a -> Text)          -- ^ The function for converting to Text.
  -> [DropdownCfg s e a]  -- ^ The config options.
  -> WidgetNode s e       -- ^ The created text dropdown.
textDropdown_ field items toText configs = newNode where
  newNode = textDropdownD_ (WidgetLens field) items toText configs

{-|
Creates a text dropdown using the given value and 'onChange' event handler.
Takes a function for converting the type to Text.
-}
textDropdownV
  :: (WidgetModel s, WidgetEvent e, Traversable t, TextDropdownItem a, TextShow a)
  => a               -- ^ The current value.
  -> (a -> e)        -- ^ The event to raise on change.
  -> t a             -- ^ The list of items.
  -> WidgetNode s e  -- ^ The created text dropdown.
textDropdownV value handler items = newNode where
  newNode = textDropdownV_ value handler items showt def

{-|
Creates a text dropdown using the given value and 'onChange' event handler.
Takes a function for converting the type to Text. Accepts config.
-}
textDropdownV_
  :: (WidgetModel s, WidgetEvent e, Traversable t, TextDropdownItem a)
  => a                    -- ^ The current value.
  -> (a -> e)             -- ^ The event to raise on change.
  -> t a                  -- ^ The list of items.
  -> (a -> Text)          -- ^ The function for converting to Text.
  -> [DropdownCfg s e a]  -- ^ The config options.
  -> WidgetNode s e       -- ^ The created text dropdown.
textDropdownV_ value handler items toText configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newNode = textDropdownD_ widgetData items toText newConfigs

{-|
Creates a text dropdown providing a 'WidgetData' instance and config. Takes
a function for converting the type to Text.
-}
textDropdownD_
  :: (WidgetModel s, WidgetEvent e, Traversable t, TextDropdownItem a)
  => WidgetData s a       -- ^ The 'WidgetData' to retrieve the value from.
  -> t a                  -- ^ The list of items.
  -> (a -> Text)          -- ^ The function for converting to Text.
  -> [DropdownCfg s e a]  -- ^ The config options.
  -> WidgetNode s e       -- ^ The created text dropdown.
textDropdownD_ widgetData items toText configs = newNode where
  makeMain t = label_ (toText t) [resizeFactorW 0.01]
  makeRow t = label_ (toText t) [resizeFactorW 0.01]
  newNode = dropdownD_ widgetData items makeMain makeRow configs

{-|
Creates a text dropdown using the given lens. The type must be have a 'Show'
instance.
-}
textDropdownS
  :: (WidgetModel s, WidgetEvent e, Traversable t, TextDropdownItem a, Show a)
  => ALens' s a      -- ^ The lens into the model.
  -> t a             -- ^ The list of items.
  -> WidgetNode s e  -- ^ The created text dropdown.
textDropdownS field items = newNode where
  newNode = textDropdownS_ field items def

{-|
Creates a text dropdown using the given lens. The type must be have a 'Show'
instance. Accepts config.
-}
textDropdownS_
  :: (WidgetModel s, WidgetEvent e, Traversable t, TextDropdownItem a, Show a)
  => ALens' s a           -- ^ The lens into the model.
  -> t a                  -- ^ The list of items.
  -> [DropdownCfg s e a]  -- ^ The config options.
  -> WidgetNode s e       -- ^ The created text dropdown.
textDropdownS_ field items configs = newNode where
  newNode = textDropdownDS_ (WidgetLens field) items configs

{-|
Creates a text dropdown using the given value and 'onChange' event handler. The
type must be have a 'Show' instance.
-}
textDropdownSV
  :: (WidgetModel s, WidgetEvent e, Traversable t, TextDropdownItem a, Show a)
  => a               -- ^ The current value.
  -> (a -> e)        -- ^ The event to raise on change.
  -> t a             -- ^ The list of items.
  -> WidgetNode s e  -- ^ The created text dropdown.
textDropdownSV value handler items = newNode where
  newNode = textDropdownSV_ value handler items def

{-|
Creates a text dropdown using the given value and 'onChange' event handler. The
type must be have a 'Show' instance. Accepts config.
-}
textDropdownSV_
  :: (WidgetModel s, WidgetEvent e, Traversable t, TextDropdownItem a, Show a)
  => a                    -- ^ The current value.
  -> (a -> e)             -- ^ The event to raise on change.
  -> t a                  -- ^ The list of items.
  -> [DropdownCfg s e a]  -- ^ The config options.
  -> WidgetNode s e       -- ^ The created text dropdown.
textDropdownSV_ value handler items configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newNode = textDropdownDS_ widgetData items newConfigs

{-|
Creates a text dropdown providing a 'WidgetData' instance and config. The
type must be have a 'Show' instance.
-}
textDropdownDS_
  :: (WidgetModel s, WidgetEvent e, Traversable t, TextDropdownItem a, Show a)
  => WidgetData s a       -- ^ The 'WidgetData' to retrieve the value from.
  -> t a                  -- ^ The list of items.
  -> [DropdownCfg s e a]  -- ^ The config options.
  -> WidgetNode s e       -- ^ The created text dropdown.
textDropdownDS_ widgetData items configs = newNode where
  toText = pack . show
  makeMain t = label_ (toText t) [resizeFactorW 0.01]
  makeRow t = label_ (toText t) [resizeFactorW 0.01]
  newNode = dropdownD_ widgetData items makeMain makeRow configs
