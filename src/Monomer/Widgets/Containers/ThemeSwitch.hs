{-|
Module      : Monomer.Widgets.Containers.ThemeSwitch
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Allows switching to a new theme for its child nodes. There are not configuration
options.
-}
{-# LANGUAGE FlexibleContexts #-}

module Monomer.Widgets.Containers.ThemeSwitch (
  themeSwitch
) where

import Control.Lens ((&), (^.), (.~), (%~), at)
import Data.Default
import Data.Maybe

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

import qualified Monomer.Lens as L

-- | Switchs to a new theme starting from its child node.
themeSwitch :: Theme -> WidgetNode s e -> WidgetNode s e
themeSwitch theme managed = makeNode widget managed where
  widget = makeTheme theme

makeNode :: Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode widget managedWidget = defaultWidgetNode "themeSwitch" widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makeTheme :: Theme -> Widget s e
makeTheme theme = widget where
  widget = createContainer () def {
    containerUpdateCWenv = updateCWenv
  }

  updateCWenv wenv cidx cnode node = newWenv where
    newWenv = wenv & L.theme .~ theme
