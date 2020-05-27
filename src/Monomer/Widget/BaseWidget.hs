{-# LANGUAGE ExistentialQuantification #-}

module Monomer.Widget.BaseWidget where

import Control.Monad
import Data.Default
import Data.Maybe
import Data.Typeable (Typeable, cast)

import Monomer.Common.Tree (Path)
import Monomer.Common.Types
import Monomer.Event.Types
import Monomer.Graphics.Renderer
import Monomer.Widget.PathContext
import Monomer.Widget.Types

import qualified Monomer.Common.Tree as Tr

createWidget :: (Monad m) => Widget s e m
createWidget = Widget {
  _widgetGetState = ignoreGetState,
  _widgetMerge = ignoreMerge,
  _widgetNextFocusable = ignoreNextFocusable,
  _widgetFind = instanceFind,
  _widgetHandleEvent = ignoreHandleEvent,
  _widgetHandleCustom = ignoreHandleCustom,
  _widgetPreferredSize = ignorePreferredSize,
  _widgetResize = defaultResize,
  _widgetRender = ignoreRender
}

ignoreGetState :: s -> Maybe WidgetState
ignoreGetState _ = Nothing

ignoreMerge :: s -> WidgetInstance s e m -> WidgetInstance s e m -> WidgetInstance s e m
ignoreMerge app new old = new

defaultMerge :: (s -> Maybe WidgetState -> Widget s e m) -> s -> WidgetInstance s e m -> WidgetInstance s e m -> WidgetInstance s e m
defaultMerge makeWidget app new old = new { _instanceWidget = makeWidget app oldState } where
  oldState = _widgetGetState (_instanceWidget old) app

ignoreNextFocusable :: PathContext -> WidgetInstance s e m -> Maybe Path
ignoreNextFocusable ctx widgetInstance = Nothing

instanceFind :: Point -> WidgetInstance s e m -> Maybe Path
instanceFind point widgetInstance = Nothing

ignoreHandleEvent :: PathContext -> SystemEvent -> s -> WidgetInstance s e m -> Maybe (EventResult s e m)
ignoreHandleEvent ctx evt app widgetInstance = Nothing

ignoreHandleCustom :: forall i s e m . Typeable i => PathContext -> i -> s -> WidgetInstance s e m -> Maybe (EventResult s e m)
ignoreHandleCustom ctx evt app widgetInstance = Nothing

ignorePreferredSize :: (Monad m) => Renderer m -> s -> WidgetInstance s e m -> m (Tr.Tree SizeReq)
ignorePreferredSize renderer app widgetInstance = return $ Tr.singleton SizeReq {
  _sizeRequested = Size 0 0,
  _sizePolicyWidth = FlexibleSize,
  _sizePolicyHeight = FlexibleSize
}

defaultResize :: (Monad m) => s -> Rect -> Rect -> WidgetInstance s e m -> Tr.Tree SizeReq -> WidgetInstance s e m
defaultResize app viewport renderArea widgetInstance reqs = widgetInstance {
    _instanceViewport = viewport,
    _instanceRenderArea = renderArea
  }

ignoreRender :: (Monad m) => Renderer m -> Timestamp -> PathContext -> s -> WidgetInstance s e m -> m ()
ignoreRender renderer ts ctx app widgetInstance = return ()
