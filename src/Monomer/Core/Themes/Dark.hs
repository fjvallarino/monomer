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
  _themeDisabled = darkDisabled
}

normalFont :: TextStyle
normalFont = def
  & L.font ?~ Font "Regular"
  & L.fontSize ?~ FontSize 24
  & L.fontColor ?~ white

darkBasic :: ThemeState
darkBasic = def
  & L.fgColor .~ blue
  & L.hlColor .~ white
  & L.text .~ normalFont
  & L.titleText . L.font ?~ Font "Bold"
  & L.titleText . L.fontSize ?~ FontSize 32
  & L.titleText . L.fontColor ?~ white
  & L.titleText . L.alignH ?~ ALeft
  & L.titleText . L.alignV ?~ AMiddle
  & L.btnStyle . L.bgColor ?~ gray
  & L.btnStyle . L.text ?~ normalFont
  & L.btnMainStyle . L.bgColor ?~ blue
  & L.btnMainStyle . L.text ?~ normalFont
  & L.btnMainStyle . L.padding ?~ padding 5

darkHover :: ThemeState
darkHover = darkBasic

darkFocus :: ThemeState
darkFocus = darkBasic

darkDisabled :: ThemeState
darkDisabled = darkBasic
