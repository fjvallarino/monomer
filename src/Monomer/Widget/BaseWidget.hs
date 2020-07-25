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
import Monomer.Widget.Types
import Monomer.Widget.Util

type WidgetMergeHandler s e = WidgetEnv s e -> Maybe WidgetState -> WidgetInstance s e -> WidgetResult s e

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

defaultInit :: WidgetEnv s e -> WidgetInstance s e -> WidgetResult s e
defaultInit _ widgetInst = resultWidget widgetInst

defaultGetState :: WidgetEnv s e -> Maybe WidgetState
defaultGetState _ = Nothing

defaultMerge :: WidgetEnv s e -> Maybe WidgetState -> WidgetInstance s e -> WidgetResult s e
defaultMerge wenv oldState newInstance = resultWidget newInstance

widgetMerge :: WidgetMergeHandler s e -> WidgetEnv s e -> WidgetInstance s e -> WidgetInstance s e -> WidgetResult s e
widgetMerge mergeHandler wenv oldInstance newInstance = result where
  oldState = _widgetGetState (_instanceWidget oldInstance) wenv
  result = mergeHandler wenv oldState newInstance

defaultNextFocusable :: WidgetEnv s e -> Path -> WidgetInstance s e -> Maybe Path
defaultNextFocusable wenv startFrom widgetInst
  | isFocusCandidate startFrom widgetInst = Just (_instancePath widgetInst)
  | otherwise = Nothing

defaultFind :: WidgetEnv s e -> Path -> Point -> WidgetInstance s e -> Maybe Path
defaultFind wenv path point widgetInst = Just (_instancePath widgetInst)

defaultHandleEvent :: WidgetEnv s e -> Path -> SystemEvent -> WidgetInstance s e -> Maybe (WidgetResult s e)
defaultHandleEvent wenv target evt widgetInst = Nothing

defaultHandleMessage :: forall i s e m . Typeable i => WidgetEnv s e -> Path -> i -> WidgetInstance s e -> Maybe (WidgetResult s e)
defaultHandleMessage wenv target message widgetInst = Nothing

defaultPreferredSize :: WidgetEnv s e -> WidgetInstance s e -> Tree SizeReq
defaultPreferredSize wenv widgetInst = singleNode SizeReq {
  _sizeRequested = Size 0 0,
  _sizePolicyWidth = FlexibleSize,
  _sizePolicyHeight = FlexibleSize
}

defaultResize :: WidgetEnv s e -> Rect -> Rect -> WidgetInstance s e -> Tree SizeReq -> WidgetInstance s e
defaultResize wenv viewport renderArea widgetInst reqs = widgetInst {
    _instanceViewport = viewport,
    _instanceRenderArea = renderArea
  }

defaultRender :: (Monad m) => Renderer m -> WidgetEnv s e -> WidgetInstance s e -> m ()
defaultRender renderer wenv widgetInst = return ()
