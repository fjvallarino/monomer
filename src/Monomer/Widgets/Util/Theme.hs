{-# LANGUAGE RankNTypes #-}

module Monomer.Widgets.Util.Theme where

import Control.Lens (Lens', (&), (^.), (^?), (.~), (?~), (<>~), at, non)
import Data.Default
import Data.Maybe

import Monomer.Core

import qualified Monomer.Lens as L

themeEmptyOverlay :: WidgetEnv s e -> Style
themeEmptyOverlay wenv = collectTheme wenv L.emptyOverlay

themeText :: WidgetEnv s e -> Style
themeText wenv = collectThemeField wenv L.text L.text

themeBtn :: WidgetEnv s e -> Style
themeBtn wenv = collectTheme wenv L.btnStyle

themeBtnMain :: WidgetEnv s e -> Style
themeBtnMain wenv = collectTheme wenv L.btnMainStyle

themeDialogFrame :: WidgetEnv s e -> Style
themeDialogFrame wenv = collectTheme wenv L.dialogFrameStyle

themeDialogTitle :: WidgetEnv s e -> Style
themeDialogTitle wenv = collectTheme wenv L.dialogTitleStyle

themeDialogCloseIcon :: WidgetEnv s e -> Style
themeDialogCloseIcon wenv = collectTheme wenv L.dialogCloseIconStyle

themeDialogMsgBody :: WidgetEnv s e -> Style
themeDialogMsgBody wenv = collectTheme wenv L.dialogMsgBodyStyle

themeDialogButtons :: WidgetEnv s e -> Style
themeDialogButtons wenv = collectTheme wenv L.dialogButtonsStyle

collectThemeField
  :: WidgetEnv s e -> Lens' StyleState (Maybe t) -> Lens' ThemeState t -> Style
collectThemeField wenv fieldS fieldT = collectThemeField_ wenv fieldS fieldT def

collectThemeField_
  :: WidgetEnv s e
  -> Lens' StyleState (Maybe t)
  -> Lens' ThemeState t
  -> Style
  -> Style
collectThemeField_ wenv fieldStyle fieldTheme target = style where
  basic = Just $ target ^. L.basic . non def
    & fieldStyle ?~ wenv ^. L.theme . L.basic . fieldTheme
  hover = Just $ target ^. L.hover . non def
    & fieldStyle ?~ wenv ^. L.theme . L.hover . fieldTheme
  focus = Just $ target ^. L.focus . non def
    & fieldStyle ?~ wenv ^. L.theme . L.focus . fieldTheme
  focusHover = Just $ target ^. L.focusHover . non def
    & fieldStyle ?~ wenv ^. L.theme . L.focusHover . fieldTheme
  active = Just $ target ^. L.active . non def
    & fieldStyle ?~ wenv ^. L.theme . L.active . fieldTheme
  disabled = Just $ target ^. L.disabled . non def
    & fieldStyle ?~ wenv ^. L.theme . L.disabled . fieldTheme
  style = Style basic hover focus focusHover active disabled

collectTheme :: WidgetEnv s e  -> Lens' ThemeState StyleState -> Style
collectTheme wenv fieldT = style where
  basic = Just $ wenv ^. L.theme . L.basic . fieldT
  hover = Just $ wenv ^. L.theme . L.hover . fieldT
  focus = Just $ wenv ^. L.theme . L.focus . fieldT
  focusHover = Just $ wenv ^. L.theme . L.focusHover . fieldT
  active = Just $ wenv ^. L.theme . L.active . fieldT
  disabled = Just $ wenv ^. L.theme . L.disabled . fieldT
  style = Style basic hover focus focusHover active disabled

collectUserTheme :: WidgetEnv s e  -> String -> Style
collectUserTheme wenv name = style where
  basic = wenv ^. L.theme . L.basic . L.userStyleMap . at name
  hover = wenv ^. L.theme . L.hover . L.userStyleMap . at name
  focus = wenv ^. L.theme . L.focus . L.userStyleMap . at name
  focusHover = wenv ^. L.theme . L.focusHover . L.userStyleMap . at name
  active = wenv ^. L.theme . L.active . L.userStyleMap . at name
  disabled = wenv ^. L.theme . L.disabled . L.userStyleMap . at name
  style = Style basic hover focus focusHover active disabled
