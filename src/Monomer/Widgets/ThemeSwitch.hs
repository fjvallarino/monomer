{-# LANGUAGE FlexibleContexts #-}

module Monomer.Widgets.ThemeSwitch (
  themeSwitch
) where

import Control.Lens ((&), (^.), (.~), (%~), at)
import Data.Default
import Data.Maybe

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

import qualified Monomer.Lens as L

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
