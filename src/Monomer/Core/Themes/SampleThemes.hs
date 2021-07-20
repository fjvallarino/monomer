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
  clearColor = grayBgLight,
  btnFocusBorder = blueHighlightBtn,
  btnBgBasic = grayLighter,
  btnBgHover = grayBright,
  btnBgActive = grayLight,
  btnBgDisabled = grayDisabled,
  btnText = grayDarker,
  btnTextDisabled = grayDarker,
  btnMainFocusBorder = blueHighlight,
  btnMainBgBasic = blueLight,
  btnMainBgHover = blueLighter,
  btnMainBgActive = blueMid,
  btnMainBgDisabled = blueDisabled,
  btnMainText = white,
  btnMainTextDisabled = white,
  dialogBg = grayDark,
  dialogBorder = grayDark,
  dialogText = white,
  dialogTitleText = white,
  emptyOverlay = grayMid & L.a .~ 0.8,
  externalLinkBasic = blueLink,
  externalLinkHover = blueLighter,
  externalLinkFocus = blueLight,
  externalLinkActive = blueMid,
  externalLinkDisabled = grayDisabled,
  iconBg = grayBright,
  iconFg = grayDark,
  inputIconFg = black,
  inputBorder = grayLighter,
  inputFocusBorder = blueHighlight,
  inputBgBasic = grayInputLight,
  inputBgHover = grayLighter,
  inputBgFocus = grayInputLight,
  inputBgActive = grayLight,
  inputBgDisabled = grayDisabled,
  inputFgBasic = grayMid,
  inputFgHover = grayBright,
  inputFgFocus = blueLight,
  inputFgActive = blueLighter,
  inputFgDisabled = grayMid,
  inputSndBasic = grayLighter,
  inputSndHover = grayLight,
  inputSndFocus = grayMid,
  inputSndActive = grayDark,
  inputSndDisabled = grayLighter,
  inputHlBasic = grayLight,
  inputHlHover = grayLighter,
  inputHlFocus = blueHighlight,
  inputHlActive = blueHighlight,
  inputHlDisabled = grayLight,
  inputSelBasic = grayMid,
  inputSelFocus = blueSelectionLight,
  inputText = black,
  inputTextDisabled = grayDarker,
  labelText = black,
  scrollBarBasic = grayDark & L.a .~ 0.2,
  scrollThumbBasic = grayMid & L.a .~ 0.6,
  scrollBarHover = grayDark & L.a .~ 0.4,
  scrollThumbHover = grayMid & L.a .~ 0.8,
  slMainBg = grayDarker,
  slNormalBgBasic = transparent,
  slNormalBgHover = grayDark,
  slNormalText = black,
  slNormalFocusBorder = blueHighlight,
  slSelectedBgBasic = grayMid,
  slSelectedBgHover = grayLight,
  slSelectedText = black,
  slSelectedFocusBorder = blueHighlight,
  tooltipBorder = grayLighter,
  tooltipBg = grayInputLight,
  tooltipText = black
}

darkTheme :: Theme
darkTheme = baseTheme darkThemeColors

darkThemeColors :: BaseThemeColors
darkThemeColors = BaseThemeColors {
  clearColor = grayBgDark,
  btnFocusBorder = blueHighlightBtn,
  btnBgBasic = grayLighter,
  btnBgHover = grayBright,
  btnBgActive = grayLight,
  btnBgDisabled = grayDisabled,
  btnText = grayDarker,
  btnTextDisabled = grayDarker,
  btnMainFocusBorder = blueHighlight,
  btnMainBgBasic = blueLight,
  btnMainBgHover = blueLighter,
  btnMainBgActive = blueMid,
  btnMainBgDisabled = blueDisabled,
  btnMainText = white,
  btnMainTextDisabled = white,
  dialogBg = grayDark,
  dialogBorder = grayDark,
  dialogText = white,
  dialogTitleText = white,
  emptyOverlay = grayMid & L.a .~ 0.8,
  externalLinkBasic = blueLink,
  externalLinkHover = blueLighter,
  externalLinkFocus = blueLight,
  externalLinkActive = blueMid,
  externalLinkDisabled = grayDisabled,
  iconBg = grayBright,
  iconFg = grayDark,
  inputIconFg = black,
  inputBorder = grayDarker,
  inputFocusBorder = blueHighlight,
  inputBgBasic = grayInputDark,
  inputBgHover = grayMid,
  inputBgFocus = grayInputDark,
  inputBgActive = grayLight,
  inputBgDisabled = grayDisabled,
  inputFgBasic = grayMid,
  inputFgHover = grayLight,
  inputFgFocus = blueLighter,
  inputFgActive = blueLighter,
  inputFgDisabled = grayMid,
  inputSndBasic = grayLighter,
  inputSndHover = grayLight,
  inputSndFocus = grayMid,
  inputSndActive = grayDark,
  inputSndDisabled = grayLighter,
  inputHlBasic = grayLight,
  inputHlHover = grayLighter,
  inputHlFocus = blueHighlight,
  inputHlActive = blueHighlight,
  inputHlDisabled = grayLight,
  inputSelBasic = grayMid,
  inputSelFocus = blueSelectionDark,
  inputText = white,
  inputTextDisabled = grayDarker,
  labelText = white,
  scrollBarBasic = grayDark & L.a .~ 0.2,
  scrollThumbBasic = grayMid & L.a .~ 0.6,
  scrollBarHover = grayDark & L.a .~ 0.4,
  scrollThumbHover = grayMid & L.a .~ 0.8,
  slMainBg = grayDarker,
  slNormalBgBasic = transparent,
  slNormalBgHover = grayDark,
  slNormalText = white,
  slNormalFocusBorder = blueHighlight,
  slSelectedBgBasic = grayMid,
  slSelectedBgHover = grayLight,
  slSelectedText = white,
  slSelectedFocusBorder = blueHighlight,
  tooltipBorder = grayLighter,
  tooltipBg = grayInputDark,
  tooltipText = white
}

black = rgbHex "#000000"
white = rgbHex "#FFFFFF"
blueSelectionLight = rgbHex "#63B7FB"
blueSelectionDark = rgbHex "#1414FF"
blueDisabled = rgbHex "#314C69"
blueMid = rgbHex "#4259DB"
blueLight = rgbHex "#4E8CF7"
blueLighter = rgbHex "#63B7FB"
blueHighlight = rgbHex "#87CEFA"
blueHighlightBtn = rgbHex "#BBE2FA"
blueLink = rgbHex "#58A6FF"
grayBgLight = rgbHex "#EAEAEA"
grayBgDark = rgbHex "#2E2E2E"
grayDisabled = rgbHex "#606060"
grayDarker = rgbHex "#282828"
grayDark = rgbHex "#404040"
grayInputLight = rgbHex "#C4C4C4"
grayInputDark = rgbHex "#575757"
grayMid = rgbHex "#6E6E6E"
grayLight = rgbHex "#8C8C8C"
grayLighter = rgbHex "#A4A4A4"
grayBright = rgbHex "#C4C4C4"
grayBrighter = rgbHex "#E4E4E4"
grayBorder = rgbHex "#393939"
