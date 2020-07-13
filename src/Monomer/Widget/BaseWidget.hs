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

type WidgetMergeHandler s e = WidgetContext s e -> PathContext -> Maybe WidgetState -> WidgetInstance s e -> WidgetResult s e

createWidget :: Widget s e
createWidget = Widget {
  _widgetInit = defaultInit,
  _widgetGetState = defaultGetState,
  _widgetMerge = widgetMerge defaultMerge,
  _widgetNextFocusable = defaultNextFocusable,
  _widgetFind = defaultFind,
  _widgetHandleEvent = defaultHandleEvent,
  _widgetHandleMessage = defaultHandleMessage,
  _widgetPreferredSize = defaultPreferredSize,
  _widgetResize = defaultResize,
  _widgetRender = defaultRender
}

defaultInit :: WidgetContext s e -> PathContext -> WidgetInstance s e -> WidgetResult s e
defaultInit _ _ widgetInstance = resultWidget widgetInstance

defaultGetState :: WidgetContext s e -> Maybe WidgetState
defaultGetState _ = Nothing

defaultMerge :: WidgetContext s e -> PathContext -> Maybe WidgetState -> WidgetInstance s e -> WidgetResult s e
defaultMerge wctx ctx oldState newInstance = resultWidget newInstance

widgetMerge :: WidgetMergeHandler s e -> WidgetContext s e -> PathContext -> WidgetInstance s e -> WidgetInstance s e -> WidgetResult s e
widgetMerge mergeHandler wctx ctx oldInstance newInstance = result where
  oldState = _widgetGetState (_instanceWidget oldInstance) wctx
  result = mergeHandler wctx ctx oldState newInstance

defaultNextFocusable :: PathContext -> WidgetInstance s e -> Maybe Path
defaultNextFocusable ctx widgetInstance = Nothing

defaultFind :: Path -> Point -> WidgetInstance s e -> Maybe Path
defaultFind path point widgetInstance = Nothing

defaultHandleEvent :: WidgetContext s e -> PathContext -> SystemEvent -> WidgetInstance s e -> Maybe (WidgetResult s e)
defaultHandleEvent wctx ctx evt widgetInstance = Nothing

defaultHandleMessage :: forall i s e m . Typeable i => WidgetContext s e -> PathContext -> i -> WidgetInstance s e -> Maybe (WidgetResult s e)
defaultHandleMessage wctx ctx evt widgetInstance = Nothing

defaultPreferredSize :: Renderer m -> WidgetContext s e -> WidgetInstance s e -> Tree SizeReq
defaultPreferredSize renderer wctx widgetInstance = singleNode SizeReq {
  _sizeRequested = Size 0 0,
  _sizePolicyWidth = FlexibleSize,
  _sizePolicyHeight = FlexibleSize
}

defaultResize :: WidgetContext s e -> Rect -> Rect -> WidgetInstance s e -> Tree SizeReq -> WidgetInstance s e
defaultResize wctx viewport renderArea widgetInstance reqs = widgetInstance {
    _instanceViewport = viewport,
    _instanceRenderArea = renderArea
  }

defaultRender :: (Monad m) => Renderer m -> WidgetContext s e -> PathContext -> WidgetInstance s e -> m ()
defaultRender renderer wctx ctx widgetInstance = return ()
