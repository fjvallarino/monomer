{-# LANGUAGE RecordWildCards #-}

module Monomer.Widgets.Util.Style (
  getContentRect,
  activeStyle,
  activeTheme,
  instanceStyle,
  mergeThemeStyle
) where

import Control.Applicative ((<|>))
import Data.Default
import Data.Maybe

import Monomer.Core
import Monomer.Event
import Monomer.Graphics
import Monomer.Widgets.Util.Misc
import Monomer.Widgets.Util.Widget

getContentRect :: StyleState -> WidgetInstance s e -> Rect
getContentRect style inst = removeOuterBounds style (_wiRenderArea inst)

activeStyle :: WidgetEnv s e -> WidgetInstance s e -> StyleState
activeStyle wenv inst = fromMaybe def styleState where
  Style{..} = _wiStyle inst
  mousePos = _ipsMousePos $ _weInputStatus wenv
  isEnabled = _wiEnabled inst
  isHover = pointInViewport mousePos inst
  isFocus = isFocused wenv inst
  styleState
    | not isEnabled = _styleBasic <> _styleDisabled
    | isHover && isFocus = _styleBasic <> _styleFocus <> _styleHover
    | isHover = _styleBasic <> _styleHover
    | isFocus = _styleBasic <> _styleFocus
    | otherwise = _styleBasic

activeTheme :: WidgetEnv s e -> WidgetInstance s e -> ThemeState
activeTheme wenv inst = themeState where
  theme = _weTheme wenv
  mousePos = _ipsMousePos $ _weInputStatus wenv
  isEnabled = _wiEnabled inst
  isHover = pointInViewport mousePos inst
  isFocus = isFocused wenv inst
  themeState
    | not isEnabled = _themeDisabled theme
    | isHover = _themeHover theme
    | isFocus = _themeFocus theme
    | otherwise = _themeBasic theme

instanceStyle :: WidgetEnv s e -> WidgetInstance s e -> StyleState
instanceStyle wenv inst = mergeThemeStyle theme style where
  style = activeStyle wenv inst
  theme = activeTheme wenv inst

mergeThemeStyle :: ThemeState -> StyleState -> StyleState
mergeThemeStyle theme style = newStyle where
  themeFgColor = Just $ _thsFgColor theme
  themeHlColor = Just $ _thsHlColor theme
  themeTextNormal = Just $ _thsText theme
  newStyle = style {
    _sstFgColor = _sstFgColor style <|> themeFgColor,
    _sstHlColor = _sstHlColor style <|> themeHlColor,
    _sstText = _sstText style <> themeTextNormal
  }
