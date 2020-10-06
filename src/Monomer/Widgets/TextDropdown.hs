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

import Monomer.Core
import Monomer.Widgets.Label
import Monomer.Widgets.Dropdown

textDropdown
  :: (Traversable t, Eq a)
  => ALens' s a
  -> t a
  -> (a -> Text)
  -> WidgetInstance s e
textDropdown field items toText = newInst where
  newInst = textDropdown_ field items toText def

textDropdown_
  :: (Traversable t, Eq a)
  => ALens' s a
  -> t a
  -> (a -> Text)
  -> [DropdownCfg s e a]
  -> WidgetInstance s e
textDropdown_ field items toText configs = newInst where
  newInst = textDropdownD_ (WidgetLens field) items toText configs

textDropdownV
  :: (Traversable t, Eq a)
  => a
  -> (a -> e)
  -> t a
  -> (a -> Text)
  -> WidgetInstance s e
textDropdownV value handler items toText = newInst where
  newInst = textDropdownV_ value handler items toText def

textDropdownV_
  :: (Traversable t, Eq a)
  => a
  -> (a -> e)
  -> t a
  -> (a -> Text)
  -> [DropdownCfg s e a]
  -> WidgetInstance s e
textDropdownV_ value handler items toText configs = newInst where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newInst = textDropdownD_ widgetData items toText newConfigs

textDropdownD_
  :: (Traversable t, Eq a)
  => WidgetData s a
  -> t a
  -> (a -> Text)
  -> [DropdownCfg s e a]
  -> WidgetInstance s e
textDropdownD_ widgetData items toText configs = newInst where
  makeMain = toText
  makeRow = label . toText
  newInst = dropdownD_ widgetData items makeMain makeRow configs
