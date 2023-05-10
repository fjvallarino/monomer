{-|
Module      : Monomer.Widgets.Util.Theme
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions for loading theme values.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Util.Theme where

import Control.Lens (Lens', (&), (^.), (.~), (<>~), at, non)
import Data.Default

import Monomer.Core.StyleTypes
import Monomer.Core.ThemeTypes
import Monomer.Core.WidgetTypes

import qualified Monomer.Core.Lens as L

-- | Updates a the field of style with the field value from the active theme.
collectThemeField_
  :: WidgetEnv s e               -- ^ The widget environment (to get the theme).
  -> Lens' StyleState (Maybe t)  -- ^ The target field of the style.
  -> Lens' ThemeState (Maybe t)  -- ^ The source field of the theme.
  -> Style                       -- ^ The target style.
  -> Style                       -- ^ The updated style.
collectThemeField_ wenv fieldStyle fieldTheme target = style where
  basic = Just $ target ^. L.basic . non def
    & fieldStyle .~ wenv ^. L.theme . L.basic . fieldTheme
  hover = Just $ target ^. L.hover . non def
    & fieldStyle .~ wenv ^. L.theme . L.hover . fieldTheme
  focus = Just $ target ^. L.focus . non def
    & fieldStyle .~ wenv ^. L.theme . L.focus . fieldTheme
  focusHover = Just $ target ^. L.focusHover . non def
    & fieldStyle .~ wenv ^. L.theme . L.focusHover . fieldTheme
  active = Just $ target ^. L.active . non def
    & fieldStyle .~ wenv ^. L.theme . L.active . fieldTheme
  disabled = Just $ target ^. L.disabled . non def
    & fieldStyle .~ wenv ^. L.theme . L.disabled . fieldTheme
  style = Style basic hover focus focusHover active disabled

-- | Collects all the style states from a given field in the active theme.
collectTheme
  :: WidgetEnv s e               -- ^ The widget environment (to get the theme).
  -> Lens' ThemeState StyleState -- ^ The field into the theme
  -> Style                       -- ^ The collected style.
collectTheme wenv fieldT = style where
  basic = Just $ wenv ^. L.theme . L.basic . fieldT
  hover = Just $ wenv ^. L.theme . L.hover . fieldT
  focus = Just $ wenv ^. L.theme . L.focus . fieldT
  focusHover = Just $ wenv ^. L.theme . L.focusHover . fieldT
  active = Just $ wenv ^. L.theme . L.active . fieldT
  disabled = Just $ wenv ^. L.theme . L.disabled . fieldT
  style = Style basic hover focus focusHover active disabled

-- | Collects all the style states from a given entry in the map of user styles
--   in the active theme.
collectUserTheme
  :: WidgetEnv s e  -- ^ The widget environment (to get the theme).
  -> String         -- ^ The key into the user map.
  -> Style          -- ^ The collected style.
collectUserTheme wenv name = style where
  basic = wenv ^. L.theme . L.basic . L.userStyleMap . at name
  hover = wenv ^. L.theme . L.hover . L.userStyleMap . at name
  focus = wenv ^. L.theme . L.focus . L.userStyleMap . at name
  focusHover = wenv ^. L.theme . L.focusHover . L.userStyleMap . at name
  active = wenv ^. L.theme . L.active . L.userStyleMap . at name
  disabled = wenv ^. L.theme . L.disabled . L.userStyleMap . at name
  style = Style basic hover focus focusHover active disabled

{-|
Sets the given value, overwriting the previous one, for each of the states of a
theme.

Useful for customizing a base theme.

@
theme = darkTheme
  & setThemeValue L.sliderRadius (Just 10)
  & setThemeValue L.sliderWidth 10
@
-}
setThemeValue
  :: Lens' ThemeState a
  -> a
  -> Theme
  -> Theme
setThemeValue field value theme = newTheme where
  newTheme = theme
    & L.basic . field .~ value
    & L.hover . field .~ value
    & L.focus . field .~ value
    & L.focusHover . field .~ value
    & L.active . field .~ value
    & L.disabled . field .~ value

{-|
Sets the given style options, overwriting the previous style, for each of the
states of a theme.

Useful for customizing a base theme.

@
theme = darkTheme
  & setThemeStyle L.dropdownStyle [padding 6, paddingV 6]
@

Note: In most cases 'mergeThemeStyle' is a better choice, since it keeps all the
other style settings intact.
-}
setThemeStyle
  :: Lens' ThemeState StyleState
  -> [StyleState]
  -> Theme
  -> Theme
setThemeStyle field styles theme = newTheme where
  !value = mconcat styles
  newTheme = theme
    & L.basic . field .~ value
    & L.hover . field .~ value
    & L.focus . field .~ value
    & L.focusHover . field .~ value
    & L.active . field .~ value
    & L.disabled . field .~ value

{-|
Merges the given style options with the existing style, for each of the states
of a theme.

Useful for customizing a base theme.

@
theme = darkTheme
  & mergeThemeStyle L.dropdownStyle [padding 6, paddingV 6]
@
-}
mergeThemeStyle
  :: Lens' ThemeState StyleState
  -> [StyleState]
  -> Theme
  -> Theme
mergeThemeStyle field styles theme = newTheme where
  !value = mconcat styles
  newTheme = theme
    & L.basic . field <>~ value
    & L.hover . field <>~ value
    & L.focus . field <>~ value
    & L.focusHover . field <>~ value
    & L.active . field <>~ value
    & L.disabled . field <>~ value
