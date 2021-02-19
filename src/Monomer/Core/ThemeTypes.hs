{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Monomer.Core.ThemeTypes where

import Codec.Serialise
import Data.Default
import GHC.Generics

import qualified Data.Map.Strict as M

import Monomer.Core.BasicTypes
import Monomer.Core.StyleTypes
import Monomer.Graphics.Color
import Monomer.Graphics.Types

data Theme = Theme {
  _themeClearColor :: Color,
  _themeBasic :: ThemeState,
  _themeHover :: ThemeState,
  _themeFocus :: ThemeState,
  _themeFocusHover :: ThemeState,
  _themeActive :: ThemeState,
  _themeDisabled :: ThemeState
} deriving (Eq, Show, Generic, Serialise)

instance Default Theme where
  def = Theme {
    _themeClearColor = def,
    _themeBasic = def,
    _themeHover = def,
    _themeFocus = def,
    _themeFocusHover = def,
    _themeActive = def,
    _themeDisabled = def
  }

data ThemeState = ThemeState {
  _thsFgColor :: Color,
  _thsHlColor :: Color,
  _thsText :: TextStyle,
  _thsEmptyOverlay :: StyleState,
  _thsBtnStyle :: StyleState,
  _thsBtnMainStyle :: StyleState,
  _thsCheckboxWidth :: Double,
  _thsCheckboxStyle :: StyleState,
  _thsDialWidth :: Double,
  _thsDialStyle :: StyleState,
  _thsDialogFrameStyle :: StyleState,
  _thsDialogTitleStyle :: StyleState,
  _thsDialogCloseIconStyle :: StyleState,
  _thsDialogButtonsStyle :: StyleState,
  _thsDialogMsgBodyStyle :: StyleState,
  _thsDropdownMaxHeight :: Double,
  _thsDropdownStyle :: StyleState,
  _thsDropdownListStyle :: StyleState,
  _thsDropdownItemStyle :: StyleState,
  _thsDropdownItemSelectedStyle :: StyleState,
  _thsInputNumericStyle :: StyleState,
  _thsInputTextStyle :: StyleState,
  _thsLabelStyle :: StyleState,
  _thsListViewStyle :: StyleState,
  _thsListViewItemStyle :: StyleState,
  _thsListViewItemSelectedStyle :: StyleState,
  _thsRadioWidth :: Double,
  _thsRadioStyle :: StyleState,
  _thsScrollOverlay :: Bool,
  _thsScrollFollowFocus :: Bool,
  _thsScrollBarColor :: Color,
  _thsScrollThumbColor :: Color,
  _thsScrollBarWidth :: Double,
  _thsScrollThumbWidth :: Double,
  _thsScrollThumbRadius :: Double,
  _thsScrollWheelRate :: Double,
  _thsTooltipStyle :: StyleState,
  _thsUserStyleMap :: M.Map String StyleState
} deriving (Eq, Show, Generic, Serialise)

instance Default ThemeState where
  def = ThemeState {
    _thsFgColor = def,
    _thsHlColor = def,
    _thsText = def,
    _thsEmptyOverlay = def,
    _thsBtnStyle = def,
    _thsBtnMainStyle = def,
    _thsCheckboxWidth = def,
    _thsCheckboxStyle = def,
    _thsDialWidth = def,
    _thsDialStyle = def,
    _thsDialogFrameStyle = def,
    _thsDialogTitleStyle = def,
    _thsDialogCloseIconStyle = def,
    _thsDialogButtonsStyle = def,
    _thsDialogMsgBodyStyle = def,
    _thsDropdownMaxHeight = def,
    _thsDropdownStyle = def,
    _thsDropdownListStyle = def,
    _thsDropdownItemStyle = def,
    _thsDropdownItemSelectedStyle = def,
    _thsInputNumericStyle = def,
    _thsInputTextStyle = def,
    _thsLabelStyle = def,
    _thsListViewStyle = def,
    _thsListViewItemStyle = def,
    _thsListViewItemSelectedStyle = def,
    _thsRadioWidth = def,
    _thsRadioStyle = def,
    _thsScrollOverlay = False,
    _thsScrollFollowFocus = True,
    _thsScrollBarColor = def,
    _thsScrollThumbColor = def,
    _thsScrollBarWidth = 10,
    _thsScrollThumbWidth = 8,
    _thsScrollThumbRadius = 0,
    _thsScrollWheelRate = 10,
    _thsTooltipStyle = def,
    _thsUserStyleMap = M.empty
  }

instance Semigroup ThemeState where
  (<>) t1 t2 = ThemeState {
    _thsFgColor = _thsFgColor t2,
    _thsHlColor = _thsHlColor t2,
    _thsText = _thsText t1 <> _thsText t2,
    _thsEmptyOverlay = _thsEmptyOverlay t1 <> _thsEmptyOverlay t2,
    _thsBtnStyle = _thsBtnStyle t1 <> _thsBtnStyle t2,
    _thsBtnMainStyle = _thsBtnMainStyle t1 <> _thsBtnMainStyle t2,
    _thsCheckboxWidth = _thsCheckboxWidth t2,
    _thsCheckboxStyle = _thsCheckboxStyle t1 <> _thsCheckboxStyle t2,
    _thsDialWidth = _thsDialWidth t2,
    _thsDialStyle = _thsDialStyle t1 <> _thsDialStyle t2,
    _thsDialogFrameStyle = _thsDialogFrameStyle t1 <> _thsDialogFrameStyle t2,
    _thsDialogTitleStyle = _thsDialogTitleStyle t1 <> _thsDialogTitleStyle t2,
    _thsDialogCloseIconStyle = _thsDialogCloseIconStyle t1 <> _thsDialogCloseIconStyle t2,
    _thsDialogMsgBodyStyle = _thsDialogMsgBodyStyle t1 <> _thsDialogMsgBodyStyle t2,
    _thsDialogButtonsStyle = _thsDialogButtonsStyle t1 <> _thsDialogButtonsStyle t2,
    _thsDropdownMaxHeight = _thsDropdownMaxHeight t2,
    _thsDropdownStyle = _thsDropdownStyle t1 <> _thsDropdownStyle t2,
    _thsDropdownListStyle = _thsDropdownListStyle t1 <> _thsDropdownListStyle t2,
    _thsDropdownItemStyle = _thsDropdownItemStyle t1 <> _thsDropdownItemStyle t2,
    _thsDropdownItemSelectedStyle = _thsDropdownItemSelectedStyle t1 <> _thsDropdownItemSelectedStyle t2,
    _thsInputNumericStyle = _thsInputNumericStyle t1 <> _thsInputNumericStyle t2,
    _thsInputTextStyle = _thsInputTextStyle t1 <> _thsInputTextStyle t2,
    _thsLabelStyle = _thsLabelStyle t1 <> _thsLabelStyle t2,
    _thsListViewStyle = _thsListViewStyle t1 <> _thsListViewStyle t2,
    _thsListViewItemStyle = _thsListViewItemStyle t1 <> _thsListViewItemStyle t2,
    _thsListViewItemSelectedStyle = _thsListViewItemSelectedStyle t1 <> _thsListViewItemSelectedStyle t2,
    _thsRadioWidth = _thsRadioWidth t2,
    _thsRadioStyle = _thsRadioStyle t1 <> _thsRadioStyle t2,
    _thsScrollOverlay = _thsScrollOverlay t2,
    _thsScrollFollowFocus = _thsScrollFollowFocus t2,
    _thsScrollBarColor =  _thsScrollBarColor t2,
    _thsScrollThumbColor =  _thsScrollThumbColor t2,
    _thsScrollBarWidth =  _thsScrollBarWidth t2,
    _thsScrollThumbWidth =  _thsScrollThumbWidth t2,
    _thsScrollThumbRadius = _thsScrollThumbRadius t2,
    _thsScrollWheelRate = _thsScrollWheelRate t2,
    _thsTooltipStyle = _thsTooltipStyle t1 <> _thsTooltipStyle t2,
    _thsUserStyleMap = _thsUserStyleMap t1 <> _thsUserStyleMap t2
  }
