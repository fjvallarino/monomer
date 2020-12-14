module Monomer.Core.ThemeTypes where

import Data.Default

import qualified Data.Map.Strict as M

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
  _thsText :: TextStyle,
  _thsEmptyOverlayColor :: Color,
  _thsBtnStyle :: StyleState,
  _thsBtnMainStyle :: StyleState,
  _thsCheckboxWidth :: Double,
  _thsCheckboxStyle :: StyleState,
  _thsDialogFrameStyle :: StyleState,
  _thsDialogTitleStyle :: StyleState,
  _thsDialogCloseIconStyle :: StyleState,
  _thsDialogBodyStyle :: StyleState,
  _thsDialogButtonsStyle :: StyleState,
  _thsDropdownMaxHeight :: Double,
  _thsDropdownStyle :: StyleState,
  _thsDropdownListStyle :: StyleState,
  _thsDropdownItemStyle :: StyleState,
  _thsDropdownItemSelectedStyle :: StyleState,
  _thsInputFloatingStyle :: StyleState,
  _thsInputIntegralStyle :: StyleState,
  _thsInputTextStyle :: StyleState,
  _thsLabelStyle :: StyleState,
  _thsListViewStyle :: StyleState,
  _thsListViewItemStyle :: StyleState,
  _thsListViewItemSelectedStyle :: StyleState,
  _thsRadioWidth :: Double,
  _thsRadioStyle :: StyleState,
  _thsScrollBarColor :: Color,
  _thsScrollThumbColor :: Color,
  _thsScrollWidth :: Double,
  _thsUserStyleMap :: M.Map String StyleState
} deriving (Eq, Show)

instance Default ThemeState where
  def = ThemeState {
    _thsFgColor = def,
    _thsHlColor = def,
    _thsText = def,
    _thsEmptyOverlayColor = def,
    _thsBtnStyle = def,
    _thsBtnMainStyle = def,
    _thsCheckboxWidth = def,
    _thsCheckboxStyle = def,
    _thsDialogFrameStyle = def,
    _thsDialogTitleStyle = def,
    _thsDialogCloseIconStyle = def,
    _thsDialogBodyStyle = def,
    _thsDialogButtonsStyle = def,
    _thsDropdownMaxHeight = def,
    _thsDropdownStyle = def,
    _thsDropdownListStyle = def,
    _thsDropdownItemStyle = def,
    _thsDropdownItemSelectedStyle = def,
    _thsInputFloatingStyle = def,
    _thsInputIntegralStyle = def,
    _thsInputTextStyle = def,
    _thsLabelStyle = def,
    _thsListViewStyle = def,
    _thsListViewItemStyle = def,
    _thsListViewItemSelectedStyle = def,
    _thsRadioWidth = def,
    _thsRadioStyle = def,
    _thsScrollBarColor = def,
    _thsScrollThumbColor = def,
    _thsScrollWidth = def,
    _thsUserStyleMap = M.empty
  }
