{-# LANGUAGE RecordWildCards #-}

module Monomer.Widgets.Util.Style (
  activeStyle,
  activeTheme
) where

import Control.Applicative ((<|>))
import Data.Default
import Data.Maybe

import Monomer.Core
import Monomer.Event
import Monomer.Graphics
import Monomer.Widgets.Util.Misc
import Monomer.Widgets.Util.Widget

activeStyle :: WidgetEnv s e -> WidgetInstance s e -> StyleState
activeStyle wenv inst = fromMaybe def styleState where
  Style{..} = _wiStyle inst
  isEnabled = _wiEnabled inst
  isHover = isHovered wenv inst
  isFocus = isFocused wenv inst
  styleState
    | not isEnabled = _styleDisabled
    | isHover && isFocus = _styleHover <> _styleFocus
    | isHover = _styleHover
    | isFocus = _styleFocus
    | otherwise = _styleBasic

activeTheme :: WidgetEnv s e -> WidgetInstance s e -> ThemeState
activeTheme wenv inst = themeState where
  theme = _weTheme wenv
  isEnabled = _wiEnabled inst
  isHover = isHovered wenv inst
  isFocus = isFocused wenv inst
  themeState
    | not isEnabled = _themeDisabled theme
    | isHover = _themeHover theme
    | isFocus = _themeFocus theme
    | otherwise = _themeBasic theme
