module Monomer.Core.ThemeTypes where

import Data.Default

import Monomer.Core.BasicTypes
import Monomer.Core.StyleTypes
import Monomer.Graphics.Color
import Monomer.Graphics.Types

data Theme = Theme {
  _themeBasic :: ThemeState,
  _themeHover :: ThemeState,
  _themeFocus :: ThemeState,
  _themeDisabled :: ThemeState
} deriving (Eq, Show)

instance Default Theme where
  def = Theme {
    _themeBasic = def,
    _themeHover = def,
    _themeFocus = def,
    _themeDisabled = def
  }

data ThemeState = ThemeState {
  _thsFgColor :: Color,
  _thsHlColor :: Color,
  _thsEmptyOverlayColor :: Color,
  _thsScrollBarColor :: Color,
  _thsScrollThumbColor :: Color,
  _thsScrollWidth :: Double,
  _thsCheckboxWidth :: Double,
  _thsRadioWidth :: Double,
  _thsText :: TextStyle,
  _thsBtnStyle :: StyleState,
  _thsBtnMainStyle :: StyleState,
  _thsCheckboxStyle :: StyleState,
  _thsLabelStyle :: StyleState,
  _thsListViewItemStyle :: StyleState,
  _thsListViewItemSelectedStyle :: StyleState,
  _thsRadioStyle :: StyleState,
  _thsDialogFrameStyle :: StyleState,
  _thsDialogTitleStyle :: StyleState,
  _thsDialogBodyStyle :: StyleState,
  _thsDialogButtonsStyle :: StyleState
} deriving (Eq, Show)

instance Default ThemeState where
  def = ThemeState {
    _thsFgColor = def,
    _thsHlColor = def,
    _thsEmptyOverlayColor = def,
    _thsScrollBarColor = def,
    _thsScrollThumbColor = def,
    _thsScrollWidth = def,
    _thsCheckboxWidth = def,
    _thsRadioWidth = def,
    _thsText = def,
    _thsBtnStyle = def,
    _thsBtnMainStyle = def,
    _thsCheckboxStyle = def,
    _thsLabelStyle = def,
    _thsListViewItemStyle = def,
    _thsListViewItemSelectedStyle = def,
    _thsRadioStyle = def,
    _thsDialogFrameStyle = def,
    _thsDialogTitleStyle = def,
    _thsDialogBodyStyle = def,
    _thsDialogButtonsStyle = def
  }
