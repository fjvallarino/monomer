{-|
Module      : Monomer.Widgets.Containers.ThemeSwitch
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Switches to the provided theme for its child nodes.

Note: this widget ignores style settings. If you need to display borders or any
other kind of style config, set it on the child node or wrap the themeSwitch
widget in a `Monomer.Widgets.Containers.Box`.

Configs:

- themeClearBg: indicates the clear color of the theme should be applied before
rendering children. Defaults to False.
-}
{-# LANGUAGE FlexibleContexts #-}

module Monomer.Widgets.Containers.ThemeSwitch (
  themeClearBg,
  themeClearBg_,
  themeSwitch,
  themeSwitch_
) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Lens ((&), (^.), (.~), (%~), at)
import Data.Default
import Data.Maybe

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

import qualified Monomer.Lens as L

newtype ThemeSwitchCfg = ThemeSwitchCfg {
  _tmcClearBg :: Maybe Bool
} deriving (Eq, Show)


instance Default ThemeSwitchCfg where
  def = ThemeSwitchCfg {
    _tmcClearBg = Nothing
  }

instance Semigroup ThemeSwitchCfg where
  (<>) s1 s2 = ThemeSwitchCfg {
    _tmcClearBg = _tmcClearBg s2 <|> _tmcClearBg s1
  }

instance Monoid ThemeSwitchCfg where
  mempty = def

-- | Indicates the clear color should be applied before rendering children.
themeClearBg :: ThemeSwitchCfg
themeClearBg = themeClearBg_ True

-- | Sets whether the clear color should be applied before rendering children.
themeClearBg_ :: Bool -> ThemeSwitchCfg
themeClearBg_ clear = def {
  _tmcClearBg = Just clear
}

data ThemeSwitchState = ThemeSwitchState {
  _tssPrevTheme :: Maybe Theme,
  _tssChanged :: Bool
}

-- | Switches to a new theme starting from its child node.
themeSwitch :: Theme -> WidgetNode s e -> WidgetNode s e
themeSwitch theme managed = themeSwitch_ theme def managed

-- | Switches to a new theme starting from its child node. Accepts config.
themeSwitch_ :: Theme -> [ThemeSwitchCfg] -> WidgetNode s e -> WidgetNode s e
themeSwitch_ theme configs managed = makeNode widget managed where
  config = mconcat configs
  state = ThemeSwitchState Nothing False
  widget = makeThemeSwitch theme config state

makeNode :: Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode widget managedWidget = defaultWidgetNode "themeSwitch" widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makeThemeSwitch :: Theme -> ThemeSwitchCfg -> ThemeSwitchState -> Widget s e
makeThemeSwitch theme config state = widget where
  widget = createContainer state def {
    containerUpdateCWenv = updateCWenv,
    containerGetCurrentStyle = getCurrentStyle,
    containerInit = init,
    containerMerge = merge
  }

  updateCWenv wenv cidx cnode node = newWenv where
    oldTheme = _tssPrevTheme state
    -- When called during merge, the state has not yet been updated
    themeChanged = _tssChanged state || Just theme /= oldTheme
    parentChanged = wenv ^. L.themeChanged
    newWenv = wenv
      & L.theme .~ theme
      & L.themeChanged .~ (themeChanged || parentChanged)

  getCurrentStyle wenv node = style where
    clearBg = _tmcClearBg config == Just True
    clearColor = theme ^. L.clearColor
    style
      | clearBg = bgColor clearColor
      | otherwise = def

  init wenv node = resultNode newNode where
    newState = ThemeSwitchState (Just theme) False
    newNode = node
      & L.widget .~ makeThemeSwitch theme config newState

  merge wenv node oldNode oldState = resultNode newNode where
    oldTheme = _tssPrevTheme oldState
    newState = ThemeSwitchState (Just theme) (Just theme /= oldTheme)
    newNode = node
      & L.widget .~ makeThemeSwitch theme config newState
