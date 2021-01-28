module Monomer.Core.Themes.SampleThemes where

import Control.Lens ((&), (^.), (.~), (?~), non)

import Monomer.Core.ThemeTypes
import Monomer.Core.Themes.BaseTheme
import Monomer.Graphics

import qualified Monomer.Lens as L

darkTheme :: Theme
darkTheme = baseTheme darkMod where
  black = rgb 0 0 0
  white = rgb 255 255 255
  blueDisabled = rgb 20 20 200
  blueDark = rgb 0 0 235
  blueMid = rgb 20 20 255
  blueLight = rgb 40 40 255
  blueHighlight = rgb 135 206 250
  grayDisabled = rgb 50 50 50
  grayDark = rgb 80 80 80
  grayMid = rgb 110 110 110
  grayLight = rgb 140 140 140
  grayLighter = rgb 160 160 160
  grayBorder = rgb 150 150 150
  darkMod = BaseThemeColors {
    clearColor = black,
    btnBgBasic = grayDark,
    btnBgHover = grayMid,
    btnBgActive = grayLight,
    btnBgDisabled = grayDisabled,
    btnText = white,
    btnMainBgBasic = blueDark,
    btnMainBgHover = blueMid,
    btnMainBgActive = blueLight,
    btnMainBgDisabled = blueDisabled,
    btnMainText = white,
    dialogBg = grayLight,
    dialogBorder = grayDark,
    dialogText = white,
    dialogTitleText = white,
    disabledBg = grayDisabled,
    disabledText = grayMid,
    emptyOverlay = grayMid & L.a .~ 0.8,
    focusBorder = blueHighlight,
    iconFg = black,
    inputBorder = grayBorder,
    inputBgBasic = grayDark,
    inputBgHover = grayMid,
    inputBgFocus = grayDark,
    inputBgActive = grayLight,
    inputFgBasic = grayMid,
    inputFgHover = grayLight,
    inputFgFocus = blueHighlight,
    inputFgActive = blueHighlight,
    inputSndBasic = grayDark,
    inputSndHover = grayMid,
    inputSndFocus = grayLight,
    inputSndActive = grayLighter,
    inputHlBasic = grayMid,
    inputHlHover = grayLight,
    inputHlFocus = blueHighlight,
    inputHlActive = blueHighlight,
    inputSelBasic = grayMid,
    inputSelFocus = blueMid,
    inputText = white,
    labelText = white,
    lvMainBg = black,
    lvNormalBgBasic = black,
    lvNormalBgHover = grayDark,
    lvNormalText = white,
    lvSelectedBgBasic = grayMid,
    lvSelectedBgHover = grayLight,
    lvSelectedText = white,
    lvNormalFocusBorder = grayDark,
    lvSelectedFocusBorder = grayLight,
    scrollBarBasic = grayDark & L.a .~ 0.2,
    scrollThumbBasic = grayMid & L.a .~ 0.6,
    scrollBarHover = grayDark & L.a .~ 0.4,
    scrollThumbHover = grayMid & L.a .~ 0.8,
    tooltipBorder = grayBorder,
    tooltipBg = grayDark,
    tooltipText = white
  }
