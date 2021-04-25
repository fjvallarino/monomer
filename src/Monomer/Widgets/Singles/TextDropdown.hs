{-# LANGUAGE ConstraintKinds #-}

module Monomer.Widgets.Singles.TextDropdown (
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

type TextDropdownItem a = DropdownItem a

textDropdown
  :: (Traversable t, TextDropdownItem a, TextShow a, WidgetEvent e)
  => ALens' s a
  -> t a
  -> WidgetNode s e
textDropdown field items = newNode where
  newNode = textDropdown_ field items showt def

textDropdown_
  :: (Traversable t, TextDropdownItem a, WidgetEvent e)
  => ALens' s a
  -> t a
  -> (a -> Text)
  -> [DropdownCfg s e a]
  -> WidgetNode s e
textDropdown_ field items toText configs = newNode where
  newNode = textDropdownD_ (WidgetLens field) items toText configs

textDropdownV
  :: (Traversable t, TextDropdownItem a, TextShow a, WidgetEvent e)
  => a
  -> (a -> e)
  -> t a
  -> WidgetNode s e
textDropdownV value handler items = newNode where
  newNode = textDropdownV_ value handler items showt def

textDropdownV_
  :: (Traversable t, TextDropdownItem a, WidgetEvent e)
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
  :: (Traversable t, TextDropdownItem a, WidgetEvent e)
  => WidgetData s a
  -> t a
  -> (a -> Text)
  -> [DropdownCfg s e a]
  -> WidgetNode s e
textDropdownD_ widgetData items toText configs = newNode where
  makeMain t = label_ (toText t) [resizeFactorW 0.01]
  makeRow t = label_ (toText t) [resizeFactorW 0.01]
  newNode = dropdownD_ widgetData items makeMain makeRow configs

textDropdownS
  :: (Traversable t, TextDropdownItem a, Show a, WidgetEvent e)
  => ALens' s a
  -> t a
  -> WidgetNode s e
textDropdownS field items = newNode where
  newNode = textDropdownS_ field items def

textDropdownS_
  :: (Traversable t, TextDropdownItem a, Show a, WidgetEvent e)
  => ALens' s a
  -> t a
  -> [DropdownCfg s e a]
  -> WidgetNode s e
textDropdownS_ field items configs = newNode where
  newNode = textDropdownDS_ (WidgetLens field) items configs

textDropdownSV
  :: (Traversable t, TextDropdownItem a, Show a, WidgetEvent e)
  => a
  -> (a -> e)
  -> t a
  -> WidgetNode s e
textDropdownSV value handler items = newNode where
  newNode = textDropdownSV_ value handler items def

textDropdownSV_
  :: (Traversable t, TextDropdownItem a, Show a, WidgetEvent e)
  => a
  -> (a -> e)
  -> t a
  -> [DropdownCfg s e a]
  -> WidgetNode s e
textDropdownSV_ value handler items configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newNode = textDropdownDS_ widgetData items newConfigs

textDropdownDS_
  :: (Traversable t, TextDropdownItem a, Show a, WidgetEvent e)
  => WidgetData s a
  -> t a
  -> [DropdownCfg s e a]
  -> WidgetNode s e
textDropdownDS_ widgetData items configs = newNode where
  toText = pack . show
  makeMain t = label_ (toText t) [resizeFactorW 0.01]
  makeRow t = label_ (toText t) [resizeFactorW 0.01]
  newNode = dropdownD_ widgetData items makeMain makeRow configs
