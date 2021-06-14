{-|
Module      : Monomer.Core.Themes.BaseTheme
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Provides a base theme, with fixed sizes and padding but configurable colors.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Core.Themes.BaseTheme (
  BaseThemeColors(..),
  baseTheme
) where

import Control.Lens ((&), (^.), (.~), (?~), non)
import Data.Default

import Monomer.Core.Combinators
import Monomer.Core.Style
import Monomer.Graphics.Color
import Monomer.Graphics.Types

import qualified Monomer.Core.Lens as L

-- | Creates a theme using the provided colors.
baseTheme :: BaseThemeColors -> Theme
baseTheme themeMod = Theme {
  _themeClearColor = clearColor themeMod,
  _themeBasic = baseBasic themeMod,
  _themeHover = baseHover themeMod,
  _themeFocus = baseFocus themeMod,
  _themeFocusHover = baseFocusHover themeMod,
  _themeActive = baseActive themeMod,
  _themeDisabled = baseDisabled themeMod
}

-- | Customizable colors for the theme.
data BaseThemeColors = BaseThemeColors {
  clearColor :: Color,
  btnBgBasic :: Color,
  btnBgHover :: Color,
  btnBgActive :: Color,
  btnBgDisabled :: Color,
  btnText :: Color,
  btnMainBgBasic :: Color,
  btnMainBgHover :: Color,
  btnMainBgActive :: Color,
  btnMainBgDisabled :: Color,
  btnMainText :: Color,
  dialogBg :: Color,
  dialogBorder :: Color,
  dialogText :: Color,
  dialogTitleText :: Color,
  disabledBg :: Color,
  disabledText :: Color,
  emptyOverlay :: Color,
  externalLinkColor :: Color,
  focusBorder :: Color,
  iconFg :: Color,
  inputBorder :: Color,
  inputBgBasic :: Color,
  inputBgHover :: Color,
  inputBgFocus :: Color,
  inputBgActive :: Color,
  inputFgBasic :: Color,
  inputFgHover :: Color,
  inputFgFocus :: Color,
  inputFgActive :: Color,
  inputSndBasic :: Color,
  inputSndHover :: Color,
  inputSndFocus :: Color,
  inputSndActive :: Color,
  inputHlBasic :: Color,
  inputHlHover :: Color,
  inputHlFocus :: Color,
  inputHlActive :: Color,
  inputSelBasic :: Color,
  inputSelFocus :: Color,
  inputText :: Color,
  labelText :: Color,
  scrollBarBasic :: Color,
  scrollThumbBasic :: Color,
  scrollBarHover :: Color,
  scrollThumbHover :: Color,
  slMainBg :: Color,
  slNormalBgBasic :: Color,
  slNormalBgHover :: Color,
  slNormalText :: Color,
  slSelectedBgBasic :: Color,
  slSelectedBgHover :: Color,
  slSelectedText :: Color,
  slNormalFocusBorder :: Color,
  slSelectedFocusBorder :: Color,
  tooltipBorder :: Color,
  tooltipBg :: Color,
  tooltipText :: Color
} deriving (Eq, Show)

borderFocus :: BaseThemeColors -> Border
borderFocus themeMod = border 1 (focusBorder themeMod)

textPadding :: Padding
textPadding = padding 3

smallFont :: TextStyle
smallFont = def
  & L.font ?~ Font "Regular"
  & L.fontSize ?~ FontSize 12

normalFont :: TextStyle
normalFont = def
  & L.font ?~ Font "Regular"
  & L.fontSize ?~ FontSize 16

titleFont :: TextStyle
titleFont = def
  & L.font ?~ Font "Bold"
  & L.fontSize ?~ FontSize 20

btnStyle :: BaseThemeColors -> StyleState
btnStyle themeMod = def
  & L.text ?~ (normalFont & L.fontColor ?~ btnText themeMod) <> textCenter
  & L.bgColor ?~ btnBgBasic themeMod
  & L.border ?~ border 1 (btnBgBasic themeMod)
  & L.padding ?~ (paddingV 3 <> paddingH 5)

btnMainStyle :: BaseThemeColors -> StyleState
btnMainStyle themeMod = btnStyle themeMod
  & L.text . non def . L.fontColor ?~ btnMainText themeMod
  & L.bgColor ?~ btnMainBgBasic themeMod
  & L.border ?~ border 1 (btnMainBgBasic themeMod)

textInputStyle :: BaseThemeColors -> StyleState
textInputStyle themeMod = def
  & L.text ?~ (normalFont & L.fontColor ?~ inputText themeMod)
  & L.bgColor ?~ inputBgBasic themeMod
  & L.border ?~ border 1 (inputBorder themeMod)
  & L.hlColor ?~ inputSelBasic themeMod
  & L.padding ?~ textPadding

numericInputStyle :: BaseThemeColors -> StyleState
numericInputStyle themeMod = textInputStyle themeMod
  & L.text . non def . L.alignH ?~ ATRight

dateInputStyle :: BaseThemeColors -> StyleState
dateInputStyle themeMod = textInputStyle themeMod
  & L.text . non def . L.alignH ?~ ATRight

selectListItemStyle :: BaseThemeColors -> StyleState
selectListItemStyle themeMod = def
  & L.text ?~ (normalFont & L.fontColor ?~ slNormalText themeMod)
  & L.text . non def . L.alignH ?~ ATLeft
  & L.bgColor ?~ slNormalBgBasic themeMod
  & L.border ?~ border 1 (slNormalBgBasic themeMod)
  & L.padding ?~ paddingH 5

selectListItemSelectedStyle :: BaseThemeColors -> StyleState
selectListItemSelectedStyle themeMod = selectListItemStyle themeMod
  & L.text . non def . L.fontColor ?~ slSelectedText themeMod
  & L.bgColor ?~ slSelectedBgBasic themeMod
  & L.border ?~ border 1 (slSelectedBgBasic themeMod)

baseBasic :: BaseThemeColors -> ThemeState
baseBasic themeMod = def
  & L.emptyOverlayStyle .~ bgColor (emptyOverlay themeMod)
  & L.emptyOverlayStyle . L.padding ?~ padding 10
  & L.btnStyle .~ btnStyle themeMod
  & L.btnMainStyle .~ btnMainStyle themeMod
  & L.checkboxWidth .~ 20
  & L.checkboxStyle . L.fgColor ?~ inputFgBasic themeMod
  & L.dateFieldStyle .~ dateInputStyle themeMod
  & L.dialWidth .~ 50
  & L.dialStyle . L.fgColor ?~ inputFgBasic themeMod
  & L.dialStyle . L.sndColor ?~ inputSndBasic themeMod
  & L.dialogTitleStyle . L.text ?~ (titleFont & L.fontColor ?~ dialogTitleText themeMod)
  & L.dialogFrameStyle . L.padding ?~ padding 5
  & L.dialogFrameStyle . L.bgColor ?~ dialogBg themeMod
  & L.dialogFrameStyle . L.border ?~ border 1 (dialogBorder themeMod)
  & L.dialogCloseIconStyle . L.fgColor ?~ iconFg themeMod
  & L.dialogCloseIconStyle . L.sizeReqW ?~ width 16
  & L.dialogCloseIconStyle . L.sizeReqH ?~ width 16
  & L.dialogButtonsStyle . L.padding ?~ paddingT 5
  & L.dialogMsgBodyStyle . L.text
    ?~ (normalFont & L.fontColor ?~ dialogText themeMod)
  & L.dialogMsgBodyStyle . L.sizeReqW ?~ maxWidth 600
  & L.dropdownStyle .~ textInputStyle themeMod
  & L.dropdownStyle . L.fgColor ?~ iconFg themeMod
  & L.dropdownStyle . L.padding ?~ paddingH 5
  & L.dropdownStyle . L.text . non def . L.alignH ?~ ATLeft
  & L.dropdownMaxHeight .~ 200
  & L.dropdownListStyle . L.bgColor ?~ slMainBg themeMod
  & L.dropdownItemStyle .~ selectListItemStyle themeMod
  & L.dropdownItemSelectedStyle .~ selectListItemSelectedStyle themeMod
  & L.externalLinkStyle . L.text ?~ (normalFont & L.fontColor ?~ externalLinkColor themeMod)
  & L.labelStyle . L.text
    ?~ (normalFont & L.fontColor ?~ labelText themeMod) <> textLeft
  & L.labelStyle . L.padding ?~ textPadding
  & L.numericFieldStyle .~ numericInputStyle themeMod
  & L.selectListStyle . L.bgColor ?~ slMainBg themeMod
  & L.selectListStyle . L.border ?~ border 1 (slMainBg themeMod)
  & L.selectListItemStyle .~ selectListItemStyle themeMod
  & L.selectListItemSelectedStyle .~ selectListItemSelectedStyle themeMod
  & L.radioWidth .~ 20
  & L.radioStyle . L.fgColor ?~ inputFgBasic themeMod
  & L.scrollOverlay .~ False
  & L.scrollFollowFocus .~ True
  & L.scrollBarColor .~ scrollBarBasic themeMod
  & L.scrollThumbColor .~ scrollThumbBasic themeMod
  & L.scrollBarWidth .~ 10
  & L.scrollThumbWidth .~ 10
  & L.scrollThumbRadius .~ 4
  & L.scrollWheelRate .~ 10
  & L.separatorLineWidth .~ 1
  & L.separatorLineStyle . L.fgColor ?~ inputFgBasic themeMod
  & L.sliderRadius ?~ 2
  & L.sliderThumbFactor .~ 1.25
  & L.sliderWidth .~ 10
  & L.sliderStyle . L.fgColor ?~ inputFgBasic themeMod
  & L.sliderStyle . L.hlColor ?~ inputHlBasic themeMod
  & L.sliderStyle . L.sndColor ?~ inputSndBasic themeMod
  & L.textAreaStyle .~ textInputStyle themeMod
  & L.textFieldStyle .~ textInputStyle themeMod
  & L.tooltipStyle . L.text ?~ (smallFont & L.fontColor ?~ tooltipText themeMod)
  & L.tooltipStyle . L.bgColor ?~ tooltipBg themeMod
  & L.tooltipStyle . L.border ?~ border 1 (tooltipBorder themeMod)
  & L.tooltipStyle . L.padding ?~ textPadding

baseHover :: BaseThemeColors -> ThemeState
baseHover themeMod = baseBasic themeMod
  & L.btnStyle . L.bgColor ?~ btnBgHover themeMod
  & L.btnStyle . L.border ?~ border 1 (btnBgHover themeMod)
  & L.btnStyle . L.cursorIcon ?~ CursorHand
  & L.btnMainStyle . L.bgColor ?~ btnMainBgHover themeMod
  & L.btnMainStyle . L.border ?~ border 1 (btnMainBgHover themeMod)
  & L.btnMainStyle . L.cursorIcon ?~ CursorHand
  & L.checkboxStyle . L.fgColor ?~ inputFgHover themeMod
  & L.checkboxStyle . L.cursorIcon ?~ CursorHand
  & L.dateFieldStyle . L.cursorIcon ?~ CursorIBeam
  & L.dialStyle . L.fgColor ?~ inputFgHover themeMod
  & L.dialStyle . L.sndColor ?~ inputSndHover themeMod
  & L.dialStyle . L.cursorIcon ?~ CursorSizeV
  & L.dialogCloseIconStyle . L.cursorIcon ?~ CursorHand
  & L.dropdownStyle . L.bgColor ?~ inputBgHover themeMod
  & L.dropdownStyle . L.cursorIcon ?~ CursorHand
  & L.dropdownItemStyle . L.bgColor ?~ slNormalBgHover themeMod
  & L.dropdownItemStyle . L.border ?~ border 1 (slNormalBgHover themeMod)
  & L.dropdownItemStyle . L.cursorIcon ?~ CursorHand
  & L.dropdownItemSelectedStyle . L.bgColor ?~ slSelectedBgHover themeMod
  & L.dropdownItemSelectedStyle . L.border ?~ border 1 (slSelectedBgHover themeMod)
  & L.dropdownItemSelectedStyle . L.cursorIcon ?~ CursorHand
  & L.externalLinkStyle . L.text . non def . L.underline ?~ True
  & L.externalLinkStyle . L.cursorIcon ?~ CursorHand
  & L.numericFieldStyle . L.cursorIcon ?~ CursorIBeam
  & L.selectListItemStyle . L.bgColor ?~ slNormalBgHover themeMod
  & L.selectListItemStyle . L.border ?~ border 1 (slNormalBgHover themeMod)
  & L.selectListItemStyle . L.cursorIcon ?~ CursorHand
  & L.selectListItemSelectedStyle . L.bgColor ?~ slSelectedBgHover themeMod
  & L.selectListItemSelectedStyle . L.border ?~ border 1 (slSelectedBgHover themeMod)
  & L.selectListItemSelectedStyle . L.cursorIcon ?~ CursorHand
  & L.radioStyle . L.fgColor ?~ inputFgHover themeMod
  & L.radioStyle . L.cursorIcon ?~ CursorHand
  & L.scrollBarColor .~ scrollBarHover themeMod
  & L.scrollThumbColor .~ scrollThumbHover themeMod
  & L.sliderStyle . L.fgColor ?~ inputFgHover themeMod
  & L.sliderStyle . L.hlColor ?~ inputHlHover themeMod
  & L.sliderStyle . L.sndColor ?~ inputSndHover themeMod
  & L.sliderStyle . L.cursorIcon ?~ CursorHand
  & L.textAreaStyle . L.cursorIcon ?~ CursorIBeam
  & L.textFieldStyle . L.cursorIcon ?~ CursorIBeam

baseFocus :: BaseThemeColors -> ThemeState
baseFocus themeMod = baseBasic themeMod
  & L.btnStyle . L.border ?~ borderFocus themeMod
  & L.btnMainStyle . L.border ?~ borderFocus themeMod
  & L.checkboxStyle . L.fgColor ?~ inputFgFocus themeMod
  & L.dateFieldStyle . L.border ?~ borderFocus themeMod
  & L.dateFieldStyle . L.hlColor ?~ inputSelFocus themeMod
  & L.dialStyle . L.fgColor ?~ inputFgFocus themeMod
  & L.dialStyle . L.sndColor ?~ inputSndFocus themeMod
  & L.dropdownStyle . L.border ?~ borderFocus themeMod
  & L.dropdownListStyle . L.border ?~ borderFocus themeMod
  & L.dropdownItemStyle . L.border ?~ border 1 (slNormalFocusBorder themeMod)
  & L.dropdownItemSelectedStyle . L.border ?~ border 1 (slSelectedFocusBorder themeMod)
  & L.numericFieldStyle . L.border ?~ borderFocus themeMod
  & L.numericFieldStyle . L.hlColor ?~ inputSelFocus themeMod
  & L.selectListStyle . L.border ?~ borderFocus themeMod
  & L.selectListItemStyle . L.border ?~ border 1 (slNormalFocusBorder themeMod)
  & L.selectListItemSelectedStyle . L.border ?~ border 1 (slSelectedFocusBorder themeMod)
  & L.radioStyle . L.fgColor ?~ inputFgFocus themeMod
  & L.sliderStyle . L.fgColor ?~ inputFgFocus themeMod
  & L.sliderStyle . L.hlColor ?~ inputHlFocus themeMod
  & L.sliderStyle . L.sndColor ?~ inputSndFocus themeMod
  & L.textAreaStyle . L.border ?~ borderFocus themeMod
  & L.textAreaStyle . L.hlColor ?~ inputSelFocus themeMod
  & L.textFieldStyle . L.border ?~ borderFocus themeMod
  & L.textFieldStyle . L.hlColor ?~ inputSelFocus themeMod

baseFocusHover :: BaseThemeColors -> ThemeState
baseFocusHover themeMod = (baseHover themeMod <> baseFocus themeMod)
  & L.btnStyle . L.bgColor ?~ btnBgHover themeMod
  & L.btnMainStyle . L.bgColor ?~ btnMainBgHover themeMod
  & L.dropdownListStyle . L.border ?~ borderFocus themeMod

baseActive :: BaseThemeColors -> ThemeState
baseActive themeMod = baseFocusHover themeMod
  & L.btnStyle . L.bgColor ?~ btnBgActive themeMod
  & L.btnStyle . L.border ?~ borderFocus themeMod
  & L.btnMainStyle . L.bgColor ?~ btnMainBgActive themeMod
  & L.btnMainStyle . L.border ?~ borderFocus themeMod
  & L.checkboxStyle . L.fgColor ?~ inputFgActive themeMod
  & L.dateFieldStyle . L.border ?~ borderFocus themeMod
  & L.dateFieldStyle . L.hlColor ?~ inputSelFocus themeMod
  & L.dialStyle . L.fgColor ?~ inputFgActive themeMod
  & L.dialStyle . L.sndColor ?~ inputSndActive themeMod
  & L.dropdownStyle . L.bgColor ?~ inputBgActive themeMod
  & L.dropdownStyle . L.border ?~ borderFocus themeMod
  & L.numericFieldStyle . L.border ?~ borderFocus themeMod
  & L.numericFieldStyle . L.hlColor ?~ inputSelFocus themeMod
  & L.radioStyle . L.fgColor ?~ inputFgActive themeMod
  & L.sliderStyle . L.fgColor ?~ inputFgActive themeMod
  & L.sliderStyle . L.hlColor ?~ inputHlActive themeMod
  & L.sliderStyle . L.sndColor ?~ inputSndActive themeMod
  & L.textAreaStyle . L.border ?~ borderFocus themeMod
  & L.textAreaStyle . L.hlColor ?~ inputSelFocus themeMod
  & L.textFieldStyle . L.border ?~ borderFocus themeMod
  & L.textFieldStyle . L.hlColor ?~ inputSelFocus themeMod

baseDisabled :: BaseThemeColors -> ThemeState
baseDisabled themeMod = baseBasic themeMod
  & L.btnStyle . L.text . non def . L.fontColor ?~ disabledText themeMod
  & L.btnStyle . L.bgColor ?~ btnBgDisabled themeMod
  & L.btnStyle . L.border ?~ border 1 (btnBgDisabled themeMod)
  & L.btnMainStyle . L.text . non def . L.fontColor ?~ disabledText themeMod
  & L.btnMainStyle . L.bgColor ?~ btnMainBgDisabled themeMod
  & L.btnMainStyle . L.border ?~ border 1 (btnMainBgDisabled themeMod)
  & L.dropdownStyle . L.text . non def . L.fontColor ?~ disabledText themeMod
  & L.dropdownStyle . L.bgColor ?~ disabledBg themeMod
  & L.dropdownStyle . L.border ?~ border 1 (disabledBg themeMod)
