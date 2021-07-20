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
widget in a `box`.

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

newtype ThemeCfg = ThemeCfg {
  _tmcClearBg :: Maybe Bool
} deriving (Eq, Show)


instance Default ThemeCfg where
  def = ThemeCfg {
    _tmcClearBg = Nothing
  }

instance Semigroup ThemeCfg where
  (<>) s1 s2 = ThemeCfg {
    _tmcClearBg = _tmcClearBg s2 <|> _tmcClearBg s1
  }

instance Monoid ThemeCfg where
  mempty = def

-- | Indicates the clear color should be applied before rendering children.
themeClearBg :: ThemeCfg
themeClearBg = themeClearBg_ True

-- | Sets whether the clear color should be applied before rendering children.
themeClearBg_ :: Bool -> ThemeCfg
themeClearBg_ clear = def {
  _tmcClearBg = Just clear
}

-- | Switchs to a new theme starting from its child node.
themeSwitch :: Theme -> WidgetNode s e -> WidgetNode s e
themeSwitch theme managed = themeSwitch_ theme def managed

-- | Switchs to a new theme starting from its child node. Accepts config.
themeSwitch_ :: Theme -> [ThemeCfg] -> WidgetNode s e -> WidgetNode s e
themeSwitch_ theme configs managed = makeNode widget managed where
  config = mconcat configs
  widget = makeTheme theme config

makeNode :: Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode widget managedWidget = defaultWidgetNode "themeSwitch" widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makeTheme :: Theme -> ThemeCfg -> Widget s e
makeTheme theme config = widget where
  widget = createContainer () def {
    containerUpdateCWenv = updateCWenv,
    containerGetActiveStyle = getActiveStyle
  }

  updateCWenv wenv cidx cnode node = newWenv where
    newWenv = wenv & L.theme .~ theme

  getActiveStyle wenv node = style where
    clearBg = _tmcClearBg config == Just True
    clearColor = theme ^. L.clearColor
    style
      | clearBg = bgColor clearColor
      | otherwise = def
