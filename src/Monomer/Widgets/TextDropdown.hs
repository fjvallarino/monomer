{-# LANGUAGE ConstraintKinds #-}
module Monomer.Widgets.TextDropdown (
  textDropdown,
  textDropdown_,
  textDropdownV,
  textDropdownV_,
  textDropdownD_
) where

import Control.Lens (ALens')
import Data.Default
import Data.Text (Text)
import Data.Typeable (Typeable)

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Widgets.Label
import Monomer.Widgets.Dropdown

type TextDropdownItem a = DropdownItem a

textDropdown
  :: (Traversable t, TextDropdownItem a)
  => ALens' s a
  -> t a
  -> (a -> Text)
  -> WidgetNode s e
textDropdown field items toText = newNode where
  newNode = textDropdown_ field items toText def

textDropdown_
  :: (Traversable t, TextDropdownItem a)
  => ALens' s a
  -> t a
  -> (a -> Text)
  -> [DropdownCfg s e a]
  -> WidgetNode s e
textDropdown_ field items toText configs = newNode where
  newNode = textDropdownD_ (WidgetLens field) items toText configs

textDropdownV
  :: (Traversable t, TextDropdownItem a)
  => a
  -> (a -> e)
  -> t a
  -> (a -> Text)
  -> WidgetNode s e
textDropdownV value handler items toText = newNode where
  newNode = textDropdownV_ value handler items toText def

textDropdownV_
  :: (Traversable t, TextDropdownItem a)
  => a
  -> (a -> e)
  -> t a
  -> (a -> Text)
  -> [DropdownCfg s e a]
  -> WidgetNode s e
textDropdownV_ value handler items toText configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newNode = textDropdownD_ widgetData items toText newConfigs

textDropdownD_
  :: (Traversable t, TextDropdownItem a)
  => WidgetData s a
  -> t a
  -> (a -> Text)
  -> [DropdownCfg s e a]
  -> WidgetNode s e
textDropdownD_ widgetData items toText configs = newNode where
  makeMain = label . toText
  makeRow = label . toText
  newNode = dropdownD_ widgetData items makeMain makeRow configs
