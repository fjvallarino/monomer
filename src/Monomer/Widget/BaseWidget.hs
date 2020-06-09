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

createWidget :: (Monad m) => Widget s e m
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

ignoreMerge :: s -> WidgetInstance s e m -> WidgetInstance s e m -> WidgetInstance s e m
ignoreMerge app new old = new

widgetMerge :: (s -> Maybe WidgetState -> Widget s e m) -> s -> WidgetInstance s e m -> WidgetInstance s e m -> WidgetInstance s e m
widgetMerge makeWidget app new old = new { _instanceWidget = makeWidget app oldState } where
  oldState = _widgetGetState (_instanceWidget old) app

ignoreNextFocusable :: PathContext -> WidgetInstance s e m -> Maybe Path
ignoreNextFocusable ctx widgetInstance = Nothing

widgetFind :: Point -> WidgetInstance s e m -> Maybe Path
widgetFind point widgetInstance = Nothing

ignoreHandleEvent :: PathContext -> SystemEvent -> s -> WidgetInstance s e m -> Maybe (EventResult s e m)
ignoreHandleEvent ctx evt app widgetInstance = Nothing

ignoreHandleCustom :: forall i s e m . Typeable i => PathContext -> i -> s -> WidgetInstance s e m -> Maybe (EventResult s e m)
ignoreHandleCustom ctx evt app widgetInstance = Nothing

widgetPreferredSize :: (Monad m) => Renderer m -> s -> WidgetInstance s e m -> Tree SizeReq
widgetPreferredSize renderer app widgetInstance = singleNode SizeReq {
  _sizeRequested = Size 0 0,
  _sizePolicyWidth = FlexibleSize,
  _sizePolicyHeight = FlexibleSize
}

widgetResize :: (Monad m) => s -> Rect -> Rect -> WidgetInstance s e m -> Tree SizeReq -> WidgetInstance s e m
widgetResize app viewport renderArea widgetInstance reqs = widgetInstance {
    _instanceViewport = viewport,
    _instanceRenderArea = renderArea
  }

ignoreRender :: (Monad m) => Renderer m -> Timestamp -> PathContext -> s -> WidgetInstance s e m -> m ()
ignoreRender renderer ts ctx app widgetInstance = return ()
