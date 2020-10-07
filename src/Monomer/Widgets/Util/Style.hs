{-# LANGUAGE RecordWildCards #-}

module Monomer.Widgets.Util.Style (
  getContentRect,
  activeStyle,
  activeTheme,
  instanceStyle,
  mergeThemeStyle,
  styleFont,
  styleFontSize,
  styleFontColor,
  styleFgColor,
  styleHlColor,
) where

import Control.Lens ((^.), (^?), _Just)
import Data.Default
import Data.Maybe

import Monomer.Core
import Monomer.Event
import Monomer.Graphics
import Monomer.Widgets.Util.Misc
import Monomer.Widgets.Util.Widget

import qualified Monomer.Core.Lens as L

getContentRect :: StyleState -> WidgetInstance s e -> Rect
getContentRect style inst = removeOuterBounds style (_wiRenderArea inst)

activeStyle :: WidgetEnv s e -> WidgetInstance s e -> StyleState
activeStyle wenv inst = fromMaybe def styleState where
  Style{..} = _wiStyle inst
  mousePos = _ipsMousePos $ _weInputStatus wenv
  isHover = pointInViewport mousePos inst
  isFocus = isFocused wenv inst
  styleState
    | isHover && isFocus = _styleBasic <> _styleFocus <> _styleHover
    | isHover = _styleBasic <> _styleHover
    | isFocus = _styleBasic <> _styleFocus
    | otherwise = _styleBasic

activeTheme :: WidgetEnv s e -> WidgetInstance s e -> ThemeState
activeTheme wenv inst = themeState where
  theme = _weTheme wenv
  mousePos = _ipsMousePos $ _weInputStatus wenv
  isHover = pointInViewport mousePos inst
  isFocus = isFocused wenv inst
  themeState
    | isHover = _themeHover theme
    | isFocus = _themeFocus theme
    | otherwise = _themeBasic theme

instanceStyle :: WidgetEnv s e -> WidgetInstance s e -> StyleState
instanceStyle wenv inst = mergeThemeStyle theme style where
  style = activeStyle wenv inst
  theme = activeTheme wenv  inst

mergeThemeStyle :: ThemeState -> StyleState -> StyleState
mergeThemeStyle theme style = newStyle where
  themeFgColor = Just $ _thsFgColor theme
  themeHlColor = Just $ _thsHlColor theme
  themeText = Just def {
    _txsFont = Just $ _thsFont theme,
    _txsFontSize = Just $ _thsFontSize theme,
    _txsFontColor = Just $ _thsFontColor theme
  }
  newStyle = style {
    _sstFgColor = _sstFgColor style <> themeFgColor,
    _sstHlColor = _sstHlColor style <> themeHlColor,
    _sstText = _sstText style <> themeText
  }

styleFont :: StyleState -> Font
styleFont style = fromMaybe def font where
  font = style ^? L.text . _Just  . L.font . _Just

styleFontSize :: StyleState -> FontSize
styleFontSize style = fromMaybe def fontSize where
  fontSize = style ^? L.text . _Just . L.fontSize . _Just

styleFontColor :: StyleState -> Color
styleFontColor style = fromMaybe def fontColor where
  fontColor = style ^? L.text . _Just . L.fontColor . _Just

styleFgColor :: StyleState -> Color
styleFgColor style = fromMaybe def fgColor where
  fgColor = style ^? L.fgColor . _Just

styleHlColor :: StyleState -> Color
styleHlColor style = fromMaybe def hlColor where
  hlColor = style ^? L.hlColor . _Just
