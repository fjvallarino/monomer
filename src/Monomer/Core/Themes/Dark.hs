{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Core.Themes.Dark (
  darkTheme
) where

import Control.Lens ((&), (^.), (.~), (?~), non)
import Data.Default

import Monomer.Core.Combinators
import Monomer.Core.Style
import Monomer.Graphics.Color
import Monomer.Graphics.Types

import qualified Monomer.Lens as L

darkTheme :: Theme
darkTheme = Theme {
  _themeBasic = darkBasic,
  _themeHover = darkHover,
  _themeFocus = darkFocus,
  _themeFocusHover = darkFocusHover,
  _themeActive = darkActive,
  _themeDisabled = darkDisabled
}

borderFocus :: Border
borderFocus = border 1 lightSkyBlue

textPadding :: Padding
textPadding = padding 3 <> paddingB 1

smallFont :: TextStyle
smallFont = def
  & L.font ?~ Font "Regular"
  & L.fontSize ?~ FontSize 12
  & L.fontColor ?~ white

normalFont :: TextStyle
normalFont = def
  & L.font ?~ Font "Regular"
  & L.fontSize ?~ FontSize 16
  & L.fontColor ?~ white

titleFont :: TextStyle
titleFont = def
  & L.font ?~ Font "Bold"
  & L.fontSize ?~ FontSize 20
  & L.fontColor ?~ white

inputStyle :: StyleState
inputStyle = def
  & L.text ?~ normalFont
  & L.bgColor ?~ darkGray
  & L.hlColor ?~ blue
  & L.border ?~ border 1 gray
  & L.padding ?~ textPadding

numericInputStyle :: StyleState
numericInputStyle = inputStyle
  & L.text . non def . L.alignH ?~ ARight

listViewItemStyle :: StyleState
listViewItemStyle = def
  & L.text ?~ normalFont
  & L.text . non def . L.alignH ?~ ALeft
  & L.bgColor ?~ black
  & L.border ?~ border 1 black
  & L.padding ?~ paddingH 5

listViewItemSelectedStyle :: StyleState
listViewItemSelectedStyle = listViewItemStyle
  & L.bgColor ?~ darkGray
  & L.border ?~ border 1 darkGray

darkBasic :: ThemeState
darkBasic = def
  & L.fgColor .~ blue
  & L.hlColor .~ white
  & L.text .~ normalFont
  & L.emptyOverlayColor .~ (darkGray & L.a .~ 0.8)
  & L.btnStyle . L.bgColor ?~ darkGray
  & L.btnStyle . L.text ?~ normalFont
  & L.btnStyle . L.border ?~ border 1 darkGray
  & L.btnStyle . L.padding ?~ (paddingV 3 <> paddingH 5)
  & L.btnMainStyle . L.bgColor ?~ blue
  & L.btnMainStyle . L.text ?~ normalFont
  & L.btnMainStyle . L.border ?~ border 1 blue
  & L.btnMainStyle . L.padding ?~ (paddingV 3 <> paddingH 5)
  & L.checkboxWidth .~ 20
  & L.checkboxStyle . L.fgColor ?~ gray
  & L.dialWidth .~ 50
  & L.dialStyle . L.fgColor ?~ darkGray
  & L.dialStyle . L.hlColor ?~ gray
  & L.dialogFrameStyle . L.bgColor ?~ gray
  & L.dialogFrameStyle . L.border ?~ border 1 darkGray
  & L.dialogTitleStyle . L.text ?~ titleFont <> textLeft
  & L.dialogTitleStyle . L.padding ?~ padding 5
  & L.dialogCloseIconStyle . L.fgColor ?~ black
  & L.dialogCloseIconStyle . L.sizeReqW ?~ width 16
  & L.dialogCloseIconStyle . L.sizeReqH ?~ width 16
  & L.dialogCloseIconStyle . L.padding ?~ padding 5
  & L.dialogBodyStyle . L.text ?~ normalFont
  & L.dialogBodyStyle . L.padding ?~ padding 5
  & L.dialogBodyStyle . L.sizeReqW ?~ minWidth 400
  & L.dialogBodyStyle . L.sizeReqH ?~ minHeight 250
  & L.dialogButtonsStyle . L.padding ?~ padding 5
  & L.dropdownStyle .~ inputStyle
  & L.dropdownStyle . L.fgColor ?~ white
  & L.dropdownStyle . L.padding ?~ paddingH 5
  & L.dropdownStyle . L.text . non def . L.alignH ?~ ALeft
  & L.dropdownMaxHeight .~ 200
  & L.dropdownListStyle . L.bgColor ?~ black
  & L.dropdownItemStyle .~ listViewItemStyle
  & L.dropdownItemSelectedStyle .~ listViewItemSelectedStyle
  & L.inputNumericStyle .~ numericInputStyle
  & L.inputTextStyle .~ inputStyle
  & L.labelStyle . L.text ?~ (normalFont <> textLeft)
  & L.labelStyle . L.padding ?~ textPadding
  & L.listViewStyle . L.border ?~ border 1 gray
  & L.listViewItemStyle .~ listViewItemStyle
  & L.listViewItemSelectedStyle .~ listViewItemSelectedStyle
  & L.radioWidth .~ 20
  & L.radioStyle . L.fgColor ?~ gray
  & L.scrollBarColor .~ (gray & L.a .~ 0.2)
  & L.scrollThumbColor .~ (darkGray & L.a .~ 0.6)
  & L.scrollBarWidth .~ 10
  & L.scrollThumbWidth .~ 8
  & L.scrollThumbRadius .~ 4
  & L.scrollWheelRate .~ 10
  & L.tooltipStyle . L.text ?~ smallFont
  & L.tooltipStyle . L.bgColor ?~ darkGray
  & L.tooltipStyle . L.border ?~ border 1 gray
  & L.tooltipStyle . L.padding ?~ textPadding

darkHover :: ThemeState
darkHover = darkBasic
  & L.scrollBarColor .~ (gray & L.a .~ 0.4)
  & L.scrollThumbColor .~ (darkGray & L.a .~ 0.8)
  & L.btnStyle . L.bgColor ?~ lightGray
  & L.btnStyle . L.cursorIcon ?~ CursorHand
  & L.btnMainStyle . L.border ?~ border 1 dodgerBlue
  & L.btnMainStyle . L.bgColor ?~ dodgerBlue
  & L.btnMainStyle . L.cursorIcon ?~ CursorHand
  & L.checkboxStyle . L.fgColor ?~ lightGray
  & L.checkboxStyle . L.cursorIcon ?~ CursorHand
  & L.dialStyle . L.fgColor ?~ gray
  & L.dialStyle . L.hlColor ?~ lightGray
  & L.dialogCloseIconStyle . L.cursorIcon ?~ CursorHand
  & L.dropdownStyle . L.bgColor ?~ gray
  & L.dropdownStyle . L.cursorIcon ?~ CursorHand
  & L.dropdownItemStyle . L.bgColor ?~ gray
  & L.dropdownItemStyle . L.cursorIcon ?~ CursorHand
  & L.dropdownItemSelectedStyle . L.cursorIcon ?~ CursorHand
  & L.inputNumericStyle . L.cursorIcon ?~ CursorIBeam
  & L.inputTextStyle . L.cursorIcon ?~ CursorIBeam
  & L.listViewItemStyle . L.bgColor ?~ gray
  & L.listViewItemStyle . L.border ?~ border 1 gray
  & L.listViewItemStyle . L.cursorIcon ?~ CursorHand
  & L.listViewItemSelectedStyle . L.cursorIcon ?~ CursorHand
  & L.radioStyle . L.fgColor ?~ lightGray
  & L.radioStyle . L.cursorIcon ?~ CursorHand

darkFocus :: ThemeState
darkFocus = darkBasic
  & L.btnStyle . L.border ?~ borderFocus
  & L.btnMainStyle . L.border ?~ borderFocus
  & L.checkboxStyle . L.fgColor ?~ lightSkyBlue
  & L.dialStyle . L.fgColor ?~ gray
  & L.dialStyle . L.hlColor ?~ lightSkyBlue
  & L.dropdownStyle . L.border ?~ borderFocus
  & L.dropdownListStyle . L.border ?~ borderFocus
  & L.dropdownItemStyle . L.border ?~ border 1 gray
  & L.dropdownItemSelectedStyle . L.border ?~ border 1 lightGray
  & L.inputNumericStyle . L.border ?~ borderFocus
  & L.inputTextStyle . L.border ?~ borderFocus
  & L.listViewStyle . L.border ?~ borderFocus
  & L.listViewItemStyle . L.border ?~ border 1 gray
  & L.listViewItemSelectedStyle . L.border ?~ border 1 lightGray
  & L.radioStyle . L.fgColor ?~ lightSkyBlue

darkFocusHover :: ThemeState
darkFocusHover = darkFocus
  & L.btnStyle . L.bgColor ?~ lightGray
  & L.btnMainStyle . L.bgColor ?~ dodgerBlue

darkActive :: ThemeState
darkActive = darkHover
  & L.btnMainStyle . L.bgColor ?~ royalBlue
  & L.btnStyle . L.bgColor ?~ silver
  & L.checkboxStyle . L.fgColor ?~ lightSkyBlue
  & L.dialStyle . L.fgColor ?~ gray
  & L.dialStyle . L.hlColor ?~ lightSkyBlue
  & L.dropdownStyle . L.bgColor ?~ silver
  & L.radioStyle . L.fgColor ?~ lightSkyBlue

darkDisabled :: ThemeState
darkDisabled = darkBasic
  & L.btnMainStyle . L.text . non def . L.fontColor ?~ darkGray
  & L.btnMainStyle . L.bgColor ?~ silver
  & L.btnMainStyle . L.border ?~ border 1 lightGray
  & L.btnStyle . L.text . non def . L.fontColor ?~ darkGray
  & L.btnStyle . L.bgColor ?~ silver
  & L.btnStyle . L.border ?~ border 1 lightGray
  & L.dropdownStyle . L.text . non def . L.fontColor ?~ darkGray
  & L.dropdownStyle . L.bgColor ?~ silver
  & L.dropdownStyle . L.border ?~ border 1 lightGray
