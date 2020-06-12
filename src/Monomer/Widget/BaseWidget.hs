{-# LANGUAGE ExistentialQuantification #-}

module Monomer.Widget.BaseWidget (
  createWidget,
  widgetMerge
) where

import Control.Monad
import Data.Default
import Data.Maybe
import Data.Typeable (Typeable, cast)

import Monomer.Common.Geometry
import Monomer.Common.Tree
import Monomer.Event.Types
import Monomer.Graphics.Renderer
import Monomer.Widget.PathContext
import Monomer.Widget.Types
import Monomer.Widget.Util

createWidget :: Widget s e
createWidget = Widget {
  _widgetGetState = ignoreGetState,
  _widgetMerge = ignoreMerge,
  _widgetNextFocusable = ignoreNextFocusable,
  _widgetFind = widgetFind,
  _widgetHandleEvent = ignoreHandleEvent,
  _widgetHandleCustom = ignoreHandleCustom,
  _widgetPreferredSize = widgetPreferredSize,
  _widgetResize = widgetResize,
  _widgetRender = ignoreRender
}

ignoreGetState :: s -> Maybe WidgetState
ignoreGetState _ = Nothing

ignoreMerge :: GlobalKeys s e -> PathContext -> s -> WidgetInstance s e -> WidgetInstance s e -> EventResult s e
ignoreMerge globalKeys ctx app new old = rWidget new

widgetMerge :: (s -> Maybe WidgetState -> Widget s e) -> GlobalKeys s e -> PathContext -> s -> WidgetInstance s e -> WidgetInstance s e -> EventResult s e
widgetMerge makeWidget globalKeys ctx app new old = rWidget updated where
  oldState = _widgetGetState (_instanceWidget old) app
  updated = new { _instanceWidget = makeWidget app oldState }

ignoreNextFocusable :: PathContext -> WidgetInstance s e -> Maybe Path
ignoreNextFocusable ctx widgetInstance = Nothing

widgetFind :: Point -> WidgetInstance s e -> Maybe Path
widgetFind point widgetInstance = Nothing

ignoreHandleEvent :: PathContext -> SystemEvent -> s -> WidgetInstance s e -> Maybe (EventResult s e)
ignoreHandleEvent ctx evt app widgetInstance = Nothing

ignoreHandleCustom :: forall i s e m . Typeable i => PathContext -> i -> s -> WidgetInstance s e -> Maybe (EventResult s e)
ignoreHandleCustom ctx evt app widgetInstance = Nothing

widgetPreferredSize :: Renderer m -> s -> WidgetInstance s e -> Tree SizeReq
widgetPreferredSize renderer app widgetInstance = singleNode SizeReq {
  _sizeRequested = Size 0 0,
  _sizePolicyWidth = FlexibleSize,
  _sizePolicyHeight = FlexibleSize
}

widgetResize :: s -> Rect -> Rect -> WidgetInstance s e -> Tree SizeReq -> WidgetInstance s e
widgetResize app viewport renderArea widgetInstance reqs = widgetInstance {
    _instanceViewport = viewport,
    _instanceRenderArea = renderArea
  }

ignoreRender :: (Monad m) => Renderer m -> Timestamp -> PathContext -> s -> WidgetInstance s e -> m ()
ignoreRender renderer ts ctx app widgetInstance = return ()
