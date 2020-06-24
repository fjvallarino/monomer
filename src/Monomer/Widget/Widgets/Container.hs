{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Container (
  ContainerConfig(..),
  container
) where

import Control.Monad (when)
import Data.Default
import Data.Maybe
import Data.Sequence (Seq(..), (|>))

import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Event.Types
import Monomer.Graphics.Drawing
import Monomer.Graphics.Types
import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.BaseContainer

data ContainerConfig e = ContainerConfig {
  _onClick :: Maybe e,
  _bgColor :: Maybe Color,
  _hoverColor :: Maybe Color
}

instance Default (ContainerConfig e) where
  def = ContainerConfig Nothing Nothing Nothing

container :: ContainerConfig e -> WidgetInstance s e -> WidgetInstance s e
container config managedWidget = makeInstance (makeContainer config) managedWidget

makeInstance :: Widget s e -> WidgetInstance s e -> WidgetInstance s e
makeInstance widget managedWidget = (defaultWidgetInstance "container" widget) {
  _instanceChildren = Seq.singleton managedWidget,
  _instanceFocusable = False
}

makeContainer :: ContainerConfig e -> Widget s e
makeContainer config = createContainer {
    _widgetHandleEvent = containerHandleEvent handleEvent,
    _widgetPreferredSize = containerPreferredSize preferredSize,
    _widgetResize = containerResize resize,
    _widgetRender = containerRender render
  }
  where
    handleEvent wctx ctx evt widgetInstance = case evt of
      Click point btn status -> result where
        isPressed = status == PressedBtn && btn == LeftBtn
        result = if isPressed && isJust (_onClick config)
                    then Just $ resultEvents [fromJust $ _onClick config] widgetInstance
                    else Nothing
      _ -> Nothing

    preferredSize renderer wctx childrenPairs = Node sizeReq childrenReqs where
      childrenReqs = fmap snd childrenPairs
      sizeReq = nodeValue $ Seq.index childrenReqs 0

    resize wctx viewport renderArea widgetInstance childrenPairs = (widgetInstance, assignedArea) where
      assignedArea = Seq.singleton (viewport, renderArea)

    render renderer wctx ctx widgetInstance = do
      let point = statusMousePos (_wcInputStatus wctx)
      let viewport = _instanceViewport widgetInstance

      when (inRect viewport point && isJust (_hoverColor config)) $
        drawBgRect renderer viewport (bgColor . fromJust . _hoverColor $ config)
