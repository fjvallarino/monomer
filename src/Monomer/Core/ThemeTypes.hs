{-|
Module      : Monomer.Core.ThemeTypes
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Theme configuration types.
-}
{-# LANGUAGE DeriveGeneric #-}

module Monomer.Core.ThemeTypes where

import Control.Applicative ((<|>))
import Data.Default
import GHC.Generics

import qualified Data.Map.Strict as M

import Monomer.Common
import Monomer.Core.StyleTypes
import Monomer.Graphics.ColorTable
import Monomer.Graphics.Types

-- | Theme configuration for each state, plus clear/base color.
data Theme = Theme {
  _themeClearColor :: Color,
  _themeBasic :: ThemeState,
  _themeHover :: ThemeState,
  _themeFocus :: ThemeState,
  _themeFocusHover :: ThemeState,
  _themeActive :: ThemeState,
  _themeDisabled :: ThemeState
} deriving (Eq, Show, Generic)

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

-- | Default theme settings for each widget.
data ThemeState = ThemeState {
  _thsFgColor :: Color,
  _thsHlColor :: Color,
  _thsTextStyle :: TextStyle,
  _thsEmptyOverlayStyle :: StyleState,
  _thsBtnStyle :: StyleState,
  _thsBtnMainStyle :: StyleState,
  _thsCheckboxStyle :: StyleState,
  _thsCheckboxWidth :: Double,
  _thsDateFieldStyle :: StyleState,
  _thsDialStyle :: StyleState,
  _thsDialWheelRate :: Rational,
  _thsDialWidth :: Double,
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
  _thsExternalLinkStyle :: StyleState,
  _thsLabelStyle :: StyleState,
  _thsNumericFieldStyle :: StyleState,
  _thsRadioStyle :: StyleState,
  _thsRadioWidth :: Double,
  _thsScrollOverlay :: Bool,
  _thsScrollFollowFocus :: Bool,
  _thsScrollBarColor :: Color,
  _thsScrollThumbColor :: Color,
  _thsScrollBarWidth :: Double,
  _thsScrollThumbWidth :: Double,
  _thsScrollThumbRadius :: Double,
  _thsScrollWheelRate :: Rational,
  _thsSeparatorLineWidth :: Double,
  _thsSeparatorLineStyle :: StyleState,
  _thsSelectListStyle :: StyleState,
  _thsSelectListItemStyle :: StyleState,
  _thsSelectListItemSelectedStyle :: StyleState,
  _thsSliderStyle :: StyleState,
  _thsSliderRadius :: Maybe Double,
  _thsSliderThumbFactor :: Double,
  _thsSliderWheelRate :: Rational,
  _thsSliderWidth :: Double,
  _thsTextAreaStyle :: StyleState,
  _thsTextFieldStyle :: StyleState,
  _thsTooltipStyle :: StyleState,
  _thsUserStyleMap :: M.Map String StyleState
} deriving (Eq, Show, Generic)

instance Default ThemeState where
  def = ThemeState {
    _thsFgColor = gray,
    _thsHlColor = lightGray,
    _thsTextStyle = def {
      _txsFontColor = Just black,
      _txsFontSize = Just (FontSize 16),
      _txsAlignH = Just ATLeft
    },
    _thsEmptyOverlayStyle = def,
    _thsBtnStyle = def,
    _thsBtnMainStyle = def,
    _thsCheckboxStyle = def,
    _thsCheckboxWidth = 20,
    _thsDateFieldStyle = def,
    _thsDialStyle = def,
    _thsDialWheelRate = 10,
    _thsDialWidth = 50,
    _thsDialogFrameStyle = def,
    _thsDialogTitleStyle = def,
    _thsDialogCloseIconStyle = def,
    _thsDialogButtonsStyle = def,
    _thsDialogMsgBodyStyle = def,
    _thsDropdownMaxHeight = 150,
    _thsDropdownStyle = def,
    _thsDropdownListStyle = def,
    _thsDropdownItemStyle = def,
    _thsDropdownItemSelectedStyle = def,
    _thsExternalLinkStyle = def,
    _thsLabelStyle = def,
    _thsNumericFieldStyle = def,
    _thsRadioStyle = def,
    _thsRadioWidth = 20,
    _thsScrollOverlay = False,
    _thsScrollFollowFocus = True,
    _thsScrollBarColor = def,
    _thsScrollThumbColor = darkGray,
    _thsScrollBarWidth = 10,
    _thsScrollThumbWidth = 8,
    _thsScrollThumbRadius = 0,
    _thsScrollWheelRate = 10,
    _thsSeparatorLineWidth = 1,
    _thsSeparatorLineStyle = def,
    _thsSelectListStyle = def,
    _thsSelectListItemStyle = def,
    _thsSelectListItemSelectedStyle = def,
    _thsSliderStyle = def,
    _thsSliderRadius = Nothing,
    _thsSliderThumbFactor = 1.25,
    _thsSliderWheelRate = 10,
    _thsSliderWidth = 10,
    _thsTextAreaStyle = def,
    _thsTextFieldStyle = def,
    _thsTooltipStyle = def,
    _thsUserStyleMap = M.empty
  }

instance Semigroup ThemeState where
  (<>) t1 t2 = ThemeState {
    _thsFgColor = _thsFgColor t2,
    _thsHlColor = _thsHlColor t2,
    _thsTextStyle = _thsTextStyle t1 <> _thsTextStyle t2,
    _thsEmptyOverlayStyle = _thsEmptyOverlayStyle t1 <> _thsEmptyOverlayStyle t2,
    _thsBtnStyle = _thsBtnStyle t1 <> _thsBtnStyle t2,
    _thsBtnMainStyle = _thsBtnMainStyle t1 <> _thsBtnMainStyle t2,
    _thsCheckboxStyle = _thsCheckboxStyle t1 <> _thsCheckboxStyle t2,
    _thsCheckboxWidth = _thsCheckboxWidth t2,
    _thsDateFieldStyle = _thsDateFieldStyle t1 <> _thsDateFieldStyle t2,
    _thsDialStyle = _thsDialStyle t1 <> _thsDialStyle t2,
    _thsDialWheelRate = _thsDialWheelRate t2,
    _thsDialWidth = _thsDialWidth t2,
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
    _thsExternalLinkStyle = _thsExternalLinkStyle t1 <> _thsExternalLinkStyle t2,
    _thsLabelStyle = _thsLabelStyle t1 <> _thsLabelStyle t2,
    _thsNumericFieldStyle = _thsNumericFieldStyle t1 <> _thsNumericFieldStyle t2,
    _thsRadioStyle = _thsRadioStyle t1 <> _thsRadioStyle t2,
    _thsRadioWidth = _thsRadioWidth t2,
    _thsScrollOverlay = _thsScrollOverlay t2,
    _thsScrollFollowFocus = _thsScrollFollowFocus t2,
    _thsScrollBarColor =  _thsScrollBarColor t2,
    _thsScrollThumbColor =  _thsScrollThumbColor t2,
    _thsScrollBarWidth =  _thsScrollBarWidth t2,
    _thsScrollThumbWidth =  _thsScrollThumbWidth t2,
    _thsScrollThumbRadius = _thsScrollThumbRadius t2,
    _thsScrollWheelRate = _thsScrollWheelRate t2,
    _thsSeparatorLineWidth = _thsSeparatorLineWidth t2,
    _thsSeparatorLineStyle = _thsSeparatorLineStyle t1 <> _thsSeparatorLineStyle t2,
    _thsSelectListStyle = _thsSelectListStyle t1 <> _thsSelectListStyle t2,
    _thsSelectListItemStyle = _thsSelectListItemStyle t1 <> _thsSelectListItemStyle t2,
    _thsSelectListItemSelectedStyle = _thsSelectListItemSelectedStyle t1 <> _thsSelectListItemSelectedStyle t2,
    _thsSliderStyle = _thsSliderStyle t1 <> _thsSliderStyle t2,
    _thsSliderThumbFactor = _thsSliderThumbFactor t2,
    _thsSliderWheelRate = _thsSliderWheelRate t2,
    _thsSliderWidth = _thsSliderWidth t2,
    _thsSliderRadius = _thsSliderRadius t2 <|> _thsSliderRadius t1,
    _thsTextAreaStyle = _thsTextAreaStyle t1 <> _thsTextAreaStyle t2,
    _thsTextFieldStyle = _thsTextFieldStyle t1 <> _thsTextFieldStyle t2,
    _thsTooltipStyle = _thsTooltipStyle t1 <> _thsTooltipStyle t2,
    _thsUserStyleMap = _thsUserStyleMap t1 <> _thsUserStyleMap t2
  }
