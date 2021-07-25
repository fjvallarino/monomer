{-|
Module      : Monomer.Core.Themes.SampleThemes
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Provides sample color schemes for the base theme.
-}
module Monomer.Core.Themes.SampleThemes (
  lightTheme,
  lightThemeColors,
  darkTheme,
  darkThemeColors
) where

import Control.Lens ((&), (^.), (.~), (?~), non)

import Monomer.Core.ThemeTypes
import Monomer.Core.Themes.BaseTheme
import Monomer.Graphics

import qualified Monomer.Lens as L

lightTheme :: Theme
lightTheme = baseTheme lightThemeColors

lightThemeColors :: BaseThemeColors
lightThemeColors = BaseThemeColors {
  clearColor = gray10, -- gray12,
  sectionColor = gray09, -- gray11,
  btnFocusBorder = blue08,
  btnBgBasic = gray07,
  btnBgHover = gray08,
  btnBgFocus = gray08,
  btnBgActive = gray06,
  btnBgDisabled = gray05,
  btnText = gray02,
  btnTextDisabled = gray02,
  btnMainFocusBorder = blue09,
  btnMainBgBasic = blue05,
  btnMainBgHover = blue06,
  btnMainBgFocus = blue06,
  btnMainBgActive = blue05,
  btnMainBgDisabled = blue04,
  btnMainText = white,
  btnMainTextDisabled = white,
  dialogBg = white,
  dialogBorder = white,
  dialogText = black,
  dialogTitleText = black,
  emptyOverlay = gray07 & L.a .~ 0.8,
  externalLinkBasic = blue07,
  externalLinkHover = blue08,
  externalLinkFocus = blue07,
  externalLinkActive = blue06,
  externalLinkDisabled = gray06,
  iconBg = gray07,
  iconFg = gray01,
  inputIconFg = black,
  inputBorder = gray06,
  inputFocusBorder = blue07,
  inputBgBasic = gray10,
  inputBgHover = white,
  inputBgFocus = white,
  inputBgActive = gray09,
  inputBgDisabled = gray05,

  inputFgBasic = gray05,
  inputFgHover = blue07,
  inputFgFocus = blue07,
  inputFgActive = blue06,
  inputFgDisabled = gray04,
  inputSndBasic = gray04,
  inputSndHover = gray05,
  inputSndFocus = gray05,
  inputSndActive = gray04,
  inputSndDisabled = gray03,
  inputHlBasic = gray06,
  inputHlHover = blue07,
  inputHlFocus = blue07,
  inputHlActive = blue06,
  inputHlDisabled = gray05,

{-
  inputFgBasic = blue06,
  inputFgHover = blue07,
  inputFgFocus = blue07,
  inputFgActive = blue08,
  inputFgDisabled = gray07,
  inputSndBasic = gray07,
  inputSndHover = gray08,
  inputSndFocus = gray08,
  inputSndActive = gray09,
  inputSndDisabled = gray06,
  inputHlBasic = blue08,
  inputHlHover = blue09,
  inputHlFocus = blue09,
  inputHlActive = blue07,
  inputHlDisabled = gray06,
-}
  inputSelBasic = gray07,
  inputSelFocus = blue08,
  inputText = black,
  inputTextDisabled = gray02,
  labelText = black,
  scrollBarBasic = gray01 & L.a .~ 0.2,
  scrollThumbBasic = gray07 & L.a .~ 0.6,
  scrollBarHover = gray01 & L.a .~ 0.4,
  scrollThumbHover = gray07 & L.a .~ 0.8,
  slMainBg = white,
  slNormalBgBasic = transparent,
  slNormalBgHover = gray09,
  slNormalText = black,
  slNormalFocusBorder = blue07,
  slSelectedBgBasic = gray08,
  slSelectedBgHover = gray09,
  slSelectedText = black,
  slSelectedFocusBorder = blue07,
  tooltipBorder = gray09,
  tooltipBg = gray10,
  tooltipText = black
}

darkTheme :: Theme
darkTheme = baseTheme darkThemeColors

darkThemeColors :: BaseThemeColors
darkThemeColors = BaseThemeColors {
  clearColor = gray03,
  sectionColor = gray02,
  btnFocusBorder = blue09,
  btnBgBasic = gray07,
  btnBgHover = gray09,
  btnBgFocus = gray08,
  btnBgActive = gray06,
  btnBgDisabled = gray05,
  btnText = gray02,
  btnTextDisabled = gray01,
  btnMainFocusBorder = blue08,
  btnMainBgBasic = blue05,
  btnMainBgHover = blue06,
  btnMainBgFocus = blue06,
  btnMainBgActive = blue05,
  btnMainBgDisabled = blue04,
  btnMainText = white,
  btnMainTextDisabled = gray08,
  dialogBg = gray01,
  dialogBorder = gray01,
  dialogText = white,
  dialogTitleText = white,
  emptyOverlay = gray05 & L.a .~ 0.8,
  externalLinkBasic = blue07,
  externalLinkHover = blue08,
  externalLinkFocus = blue07,
  externalLinkActive = blue06,
  externalLinkDisabled = gray06,
  iconBg = gray08,
  iconFg = gray01,
  inputIconFg = black,
  inputBorder = gray02,
  inputFocusBorder = blue08,
  inputBgBasic = gray04,
  inputBgHover = gray06,
  inputBgFocus = gray05,
  inputBgActive = gray03,
  inputBgDisabled = gray07,
  inputFgBasic = gray06,
  inputFgHover = blue08,
  inputFgFocus = blue08,
  inputFgActive = blue07,
  inputFgDisabled = gray07,
  inputSndBasic = gray05,
  inputSndHover = gray06,
  inputSndFocus = gray05,
  inputSndActive = gray05,
  inputSndDisabled = gray03,
  inputHlBasic = gray07,
  inputHlHover = blue08,
  inputHlFocus = blue08,
  inputHlActive = blue08,
  inputHlDisabled = gray08,
  inputSelBasic = gray06,
  inputSelFocus = blue06,
  inputText = white,
  inputTextDisabled = gray02,
  labelText = white,
  scrollBarBasic = gray01 & L.a .~ 0.2,
  scrollThumbBasic = gray07 & L.a .~ 0.6,
  scrollBarHover = gray01 & L.a .~ 0.4,
  scrollThumbHover = gray07 & L.a .~ 0.8,
  slMainBg = gray00,
  slNormalBgBasic = transparent,
  slNormalBgHover = gray05,
  slNormalText = white,
  slNormalFocusBorder = blue08,
  slSelectedBgBasic = gray04,
  slSelectedBgHover = gray05,
  slSelectedText = white,
  slSelectedFocusBorder = blue08,
  tooltipBorder = gray09,
  tooltipBg = gray04,
  tooltipText = white
}

black = rgbHex "#000000"
white = rgbHex "#FFFFFF"

blue01 = rgbHex "#002159"
blue02 = rgbHex "#01337D"
blue03 = rgbHex "#03449E"
blue04 = rgbHex "#0552B5"
blue05 = rgbHex "#0967D2"
blue06 = rgbHex "#2186EB"
blue07 = rgbHex "#47A3F3"
blue08 = rgbHex "#7CC4FA"
blue09 = rgbHex "#BAE3FF"
blue10 = rgbHex "#E6F6FF"

{-
gray01 = rgbHex "#242424"
gray02 = rgbHex "#2E2E2E"
gray03 = rgbHex "#393939"
gray04 = rgbHex "#404040"
gray05 = rgbHex "#575757"
gray06 = rgbHex "#606060"
gray07 = rgbHex "#6E6E6E"
gray08 = rgbHex "#8C8C8C"
gray09 = rgbHex "#A4A4A4"
gray10 = rgbHex "#C4C4C4"
gray11 = rgbHex "#DADADA"
gray12 = rgbHex "#EAEAEA"
-}

gray00 = rgbHex "#222222"
gray01 = rgbHex "#2E2E2E"
gray02 = rgbHex "#393939"
gray03 = rgbHex "#515151"
gray04 = rgbHex "#626262"
gray05 = rgbHex "#7E7E7E"
gray06 = rgbHex "#9E9E9E"
gray07 = rgbHex "#B1B1B1"
gray08 = rgbHex "#CFCFCF"
gray09 = rgbHex "#E1E1E1"
gray10 = rgbHex "#F7F7F7"

{-
gray00 = rgbHex "#0F1923"
gray01 = rgbHex "#1F2933"
gray02 = rgbHex "#323F4B"
gray03 = rgbHex "#3E4C59"
gray04 = rgbHex "#52606D"
gray05 = rgbHex "#616E7C"
gray06 = rgbHex "#7B8794"
gray07 = rgbHex "#9AA5B1"
gray08 = rgbHex "#CBD2D9"
gray09 = rgbHex "#E4E7EB"
gray10 = rgbHex "#F5F7FA"
-}