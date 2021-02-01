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
    containerUpdateCWenv = updateCWenv,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  updateCWenv wenv cidx cnode node = newWenv where
    newWenv = wenv & L.theme .~ theme

  getSizeReq :: ContainerGetSizeReqHandler s e a
  getSizeReq wenv currState node children = (newReqW, newReqH) where
    child = Seq.index children 0
    newReqW = child ^. L.info . L.sizeReqW
    newReqH = child ^. L.info . L.sizeReqH

  resize :: ContainerResizeHandler s e
  resize wenv viewport renderArea children node = resized where
    style = activeStyle wenv node
    contentArea = fromMaybe def (removeOuterBounds style renderArea)
    resized = (resultWidget node, Seq.singleton (contentArea, contentArea))
