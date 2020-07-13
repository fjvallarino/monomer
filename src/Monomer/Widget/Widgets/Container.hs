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
import Monomer.Common.Tree
import Monomer.Event.Types
import Monomer.Graphics.Drawing
import Monomer.Graphics.Types
import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.BaseContainer

import qualified Monomer.Common.Style as St

data ContainerConfig s e = ContainerConfig {
  _ctOnClick :: [e],
  _ctOnClickReq :: [WidgetRequest s],
  _ctBgColor :: Maybe Color,
  _ctHoverColor :: Maybe Color
}

instance Default (ContainerConfig s e) where
  def = ContainerConfig [] [] Nothing Nothing

container :: ContainerConfig s e -> WidgetInstance s e -> WidgetInstance s e
container config managedWidget = makeInstance (makeContainer config) managedWidget

makeInstance :: Widget s e -> WidgetInstance s e -> WidgetInstance s e
makeInstance widget managedWidget = (defaultWidgetInstance "container" widget) {
  _instanceChildren = Seq.singleton managedWidget,
  _instanceFocusable = False
}

makeContainer :: ContainerConfig s e -> Widget s e
makeContainer config = createContainer {
    _widgetHandleEvent = containerHandleEvent handleEvent,
    _widgetPreferredSize = containerPreferredSize preferredSize,
    _widgetResize = containerResize resize,
    _widgetRender = containerRender render
  }
  where
    handleEvent wctx ctx evt widgetInstance = case evt of
      Click point btn -> result where
        events = _ctOnClick config
        requests = _ctOnClickReq config
        result = if btn == LeftBtn && not (null events && null requests)
                    then Just $ resultReqsEvents requests events widgetInstance
                    else Nothing
      _ -> Nothing

    preferredSize renderer wctx widgetInstance childrenPairs = Node sizeReq childrenReqs where
      childrenReqs = fmap snd childrenPairs
      sizeReq = nodeValue $ Seq.index childrenReqs 0

    resize wctx viewport renderArea widgetInstance childrenPairs = (widgetInstance, assignedArea) where
      assignedArea = Seq.singleton (viewport, renderArea)

    render renderer wctx ctx widgetInstance = do
      let point = statusMousePos (_wcInputStatus wctx)
      let viewport = _instanceViewport widgetInstance

      when (isJust (_ctBgColor config)) $
        drawRect renderer viewport (_ctBgColor config) Nothing

      when (pointInRect point viewport && isJust (_ctHoverColor config)) $
        drawRect renderer viewport (_ctHoverColor config) Nothing
