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

type MakeWidget s e = WidgetContext s e -> Maybe WidgetState -> Widget s e

createWidget :: Widget s e
createWidget = Widget {
  _widgetInit = widgetInit,
  _widgetGetState = ignoreGetState,
  _widgetMerge = ignoreMerge,
  _widgetNextFocusable = ignoreNextFocusable,
  _widgetFind = widgetFind,
  _widgetHandleEvent = ignoreHandleEvent,
  _widgetHandleMessage = ignoreHandleMessage,
  _widgetPreferredSize = widgetPreferredSize,
  _widgetResize = widgetResize,
  _widgetRender = ignoreRender
}

widgetInit :: WidgetContext s e -> PathContext -> WidgetInstance s e -> WidgetResult s e
widgetInit _ _ widgetInstance = resultWidget widgetInstance

ignoreGetState :: WidgetContext s e -> Maybe WidgetState
ignoreGetState _ = Nothing

ignoreMerge :: WidgetContext s e -> PathContext -> WidgetInstance s e -> WidgetInstance s e -> WidgetResult s e
ignoreMerge wctx ctx new old = resultWidget new

widgetMerge :: MakeWidget s e -> WidgetContext s e -> PathContext -> WidgetInstance s e -> WidgetInstance s e -> WidgetResult s e
widgetMerge makeWidget wctx ctx new old = resultWidget updated where
  oldState = _widgetGetState (_instanceWidget old) wctx
  updated = new { _instanceWidget = makeWidget wctx oldState }

ignoreNextFocusable :: PathContext -> WidgetInstance s e -> Maybe Path
ignoreNextFocusable ctx widgetInstance = Nothing

widgetFind :: Path -> Point -> WidgetInstance s e -> Maybe Path
widgetFind path point widgetInstance = Nothing

ignoreHandleEvent :: WidgetContext s e -> PathContext -> SystemEvent -> WidgetInstance s e -> Maybe (WidgetResult s e)
ignoreHandleEvent wctx ctx evt widgetInstance = Nothing

ignoreHandleMessage :: forall i s e m . Typeable i => WidgetContext s e -> PathContext -> i -> WidgetInstance s e -> Maybe (WidgetResult s e)
ignoreHandleMessage wctx ctx evt widgetInstance = Nothing

widgetPreferredSize :: Renderer m -> WidgetContext s e -> WidgetInstance s e -> Tree SizeReq
widgetPreferredSize renderer wctx widgetInstance = singleNode SizeReq {
  _sizeRequested = Size 0 0,
  _sizePolicyWidth = FlexibleSize,
  _sizePolicyHeight = FlexibleSize
}

widgetResize :: WidgetContext s e -> Rect -> Rect -> WidgetInstance s e -> Tree SizeReq -> WidgetInstance s e
widgetResize wctx viewport renderArea widgetInstance reqs = widgetInstance {
    _instanceViewport = viewport,
    _instanceRenderArea = renderArea
  }

ignoreRender :: (Monad m) => Renderer m -> WidgetContext s e -> PathContext -> WidgetInstance s e -> m ()
ignoreRender renderer wctx ctx widgetInstance = return ()
