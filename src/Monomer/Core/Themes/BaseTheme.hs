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

import qualified Monomer.Lens as L

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
  lvMainBg :: Color,
  lvNormalBgBasic :: Color,
  lvNormalBgHover :: Color,
  lvNormalText :: Color,
  lvSelectedBgBasic :: Color,
  lvSelectedBgHover :: Color,
  lvSelectedText :: Color,
  lvNormalFocusBorder :: Color,
  lvSelectedFocusBorder :: Color,
  scrollBarBasic :: Color,
  scrollThumbBasic :: Color,
  scrollBarHover :: Color,
  scrollThumbHover :: Color,
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
  & L.text ?~ (normalFont & L.fontColor ?~ btnText themeMod)
  & L.bgColor ?~ btnBgBasic themeMod
  & L.border ?~ border 1 (btnBgBasic themeMod)
  & L.padding ?~ (paddingV 3 <> paddingH 5)

btnMainStyle :: BaseThemeColors -> StyleState
btnMainStyle themeMod = btnStyle themeMod
  & L.text . non def . L.fontColor ?~ btnMainText themeMod
  & L.bgColor ?~ btnMainBgBasic themeMod
  & L.border ?~ border 1 (btnMainBgBasic themeMod)

inputStyle :: BaseThemeColors -> StyleState
inputStyle themeMod = def
  & L.text ?~ (normalFont & L.fontColor ?~ inputText themeMod)
  & L.bgColor ?~ inputBgBasic themeMod
  & L.border ?~ border 1 (inputBorder themeMod)
  & L.hlColor ?~ inputSelBasic themeMod
  & L.padding ?~ textPadding

numericInputStyle :: BaseThemeColors -> StyleState
numericInputStyle themeMod = inputStyle themeMod
  & L.text . non def . L.alignH ?~ ATRight

listViewItemStyle :: BaseThemeColors -> StyleState
listViewItemStyle themeMod = def
  & L.text ?~ (normalFont & L.fontColor ?~ lvNormalText themeMod)
  & L.text . non def . L.alignH ?~ ATLeft
  & L.bgColor ?~ lvNormalBgBasic themeMod
  & L.border ?~ border 1 (lvNormalBgBasic themeMod)
  & L.padding ?~ paddingH 5

listViewItemSelectedStyle :: BaseThemeColors -> StyleState
listViewItemSelectedStyle themeMod = listViewItemStyle themeMod
  & L.text . non def . L.fontColor ?~ lvSelectedText themeMod
  & L.bgColor ?~ lvSelectedBgBasic themeMod
  & L.border ?~ border 1 (lvSelectedBgBasic themeMod)

baseBasic :: BaseThemeColors -> ThemeState
baseBasic themeMod = def
  & L.emptyOverlay .~ bgColor (emptyOverlay themeMod)
  & L.emptyOverlay . L.padding ?~ padding 10
  & L.btnStyle .~ btnStyle themeMod
  & L.btnMainStyle .~ btnMainStyle themeMod
  & L.checkboxWidth .~ 20
  & L.checkboxStyle . L.fgColor ?~ inputFgBasic themeMod
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
  & L.dialogMsgBodyStyle . L.sizeReqW ?~ rangeWidth 300 600
  & L.dialogMsgBodyStyle . L.sizeReqH ?~ rangeHeight 250 400
  & L.dropdownStyle .~ inputStyle themeMod
  & L.dropdownStyle . L.fgColor ?~ iconFg themeMod
  & L.dropdownStyle . L.padding ?~ paddingH 5
  & L.dropdownStyle . L.text . non def . L.alignH ?~ ATLeft
  & L.dropdownMaxHeight .~ 200
  & L.dropdownListStyle . L.bgColor ?~ lvMainBg themeMod
  & L.dropdownItemStyle .~ listViewItemStyle themeMod
  & L.dropdownItemSelectedStyle .~ listViewItemSelectedStyle themeMod
  & L.inputNumericStyle .~ numericInputStyle themeMod
  & L.inputTextStyle .~ inputStyle themeMod
  & L.labelStyle . L.text
    ?~ (normalFont & L.fontColor ?~ labelText themeMod) <> textLeft
  & L.labelStyle . L.padding ?~ textPadding
  & L.listViewStyle . L.bgColor ?~ lvMainBg themeMod
  & L.listViewStyle . L.border ?~ border 1 (lvMainBg themeMod)
  & L.listViewItemStyle .~ listViewItemStyle themeMod
  & L.listViewItemSelectedStyle .~ listViewItemSelectedStyle themeMod
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
  & L.dialStyle . L.fgColor ?~ inputFgHover themeMod
  & L.dialStyle . L.sndColor ?~ inputSndHover themeMod
  & L.dialStyle . L.cursorIcon ?~ CursorSizeV
  & L.dialogCloseIconStyle . L.cursorIcon ?~ CursorHand
  & L.dropdownStyle . L.bgColor ?~ inputBgHover themeMod
  & L.dropdownStyle . L.cursorIcon ?~ CursorHand
  & L.dropdownItemStyle . L.bgColor ?~ lvNormalBgHover themeMod
  & L.dropdownItemStyle . L.border ?~ border 1 (lvNormalBgHover themeMod)
  & L.dropdownItemStyle . L.cursorIcon ?~ CursorHand
  & L.dropdownItemSelectedStyle . L.bgColor ?~ lvSelectedBgHover themeMod
  & L.dropdownItemSelectedStyle . L.border ?~ border 1 (lvSelectedBgHover themeMod)
  & L.dropdownItemSelectedStyle . L.cursorIcon ?~ CursorHand
  & L.inputNumericStyle . L.cursorIcon ?~ CursorIBeam
  & L.inputTextStyle . L.cursorIcon ?~ CursorIBeam
  & L.listViewItemStyle . L.bgColor ?~ lvNormalBgHover themeMod
  & L.listViewItemStyle . L.border ?~ border 1 (lvNormalBgHover themeMod)
  & L.listViewItemStyle . L.cursorIcon ?~ CursorHand
  & L.listViewItemSelectedStyle . L.bgColor ?~ lvSelectedBgHover themeMod
  & L.listViewItemSelectedStyle . L.border ?~ border 1 (lvSelectedBgHover themeMod)
  & L.listViewItemSelectedStyle . L.cursorIcon ?~ CursorHand
  & L.radioStyle . L.fgColor ?~ inputFgHover themeMod
  & L.radioStyle . L.cursorIcon ?~ CursorHand
  & L.scrollBarColor .~ scrollBarHover themeMod
  & L.scrollThumbColor .~ scrollThumbHover themeMod

baseFocus :: BaseThemeColors -> ThemeState
baseFocus themeMod = baseBasic themeMod
  & L.btnStyle . L.border ?~ borderFocus themeMod
  & L.btnMainStyle . L.border ?~ borderFocus themeMod
  & L.checkboxStyle . L.fgColor ?~ inputFgFocus themeMod
  & L.dialStyle . L.fgColor ?~ inputFgFocus themeMod
  & L.dialStyle . L.sndColor ?~ inputSndFocus themeMod
  & L.dropdownStyle . L.border ?~ borderFocus themeMod
  & L.dropdownListStyle . L.border ?~ borderFocus themeMod
  & L.dropdownItemStyle . L.border ?~ border 1 (lvNormalFocusBorder themeMod)
  & L.dropdownItemSelectedStyle . L.border ?~ border 1 (lvSelectedFocusBorder themeMod)
  & L.inputNumericStyle . L.border ?~ borderFocus themeMod
  & L.inputNumericStyle . L.hlColor ?~ inputSelFocus themeMod
  & L.inputTextStyle . L.border ?~ borderFocus themeMod
  & L.inputTextStyle . L.hlColor ?~ inputSelFocus themeMod
  & L.listViewStyle . L.border ?~ borderFocus themeMod
  & L.listViewItemStyle . L.border ?~ border 1 (lvNormalFocusBorder themeMod)
  & L.listViewItemSelectedStyle . L.border ?~ border 1 (lvSelectedFocusBorder themeMod)
  & L.radioStyle . L.fgColor ?~ inputFgFocus themeMod

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
  & L.dialStyle . L.fgColor ?~ inputFgActive themeMod
  & L.dialStyle . L.sndColor ?~ inputSndActive themeMod
  & L.dropdownStyle . L.bgColor ?~ inputBgActive themeMod
  & L.dropdownStyle . L.border ?~ borderFocus themeMod
  & L.inputNumericStyle . L.border ?~ borderFocus themeMod
  & L.inputNumericStyle . L.hlColor ?~ inputSelFocus themeMod
  & L.inputTextStyle . L.border ?~ borderFocus themeMod
  & L.inputTextStyle . L.hlColor ?~ inputSelFocus themeMod
  & L.radioStyle . L.fgColor ?~ inputFgActive themeMod

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
