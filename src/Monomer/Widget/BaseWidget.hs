{-# LANGUAGE ExistentialQuantification #-}

module Monomer.Widget.BaseWidget (
  baseWidgetMerge,
  createWidget
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

createWidget :: Widget s e
createWidget = Widget {
  widgetInit = defaultInit,
  widgetGetState = defaultGetState,
  widgetMerge = baseWidgetMerge defaultMerge,
  widgetNextFocusable = defaultNextFocusable,
  widgetFind = defaultFind,
  widgetHandleEvent = defaultHandleEvent,
  widgetHandleMessage = defaultHandleMessage,
  widgetUpdateSizeReq = defaultUpdateSizeReq,
  widgetResize = defaultResize,
  widgetRender = defaultRender
}

defaultInit :: WidgetEnv s e -> WidgetInstance s e -> WidgetResult s e
defaultInit _ widgetInst = resultWidget widgetInst

defaultGetState :: WidgetEnv s e -> Maybe WidgetState
defaultGetState _ = Nothing

type WidgetMergeHandler s e
  = WidgetEnv s e
  -> Maybe WidgetState
  -> WidgetInstance s e
  -> WidgetResult s e

defaultMerge :: WidgetMergeHandler s e
defaultMerge wenv oldState newInstance = resultWidget newInstance

baseWidgetMerge
  :: WidgetMergeHandler s e
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetInstance s e
  -> WidgetResult s e
baseWidgetMerge mergeHandler wenv oldInstance newInstance = result where
  oldState = widgetGetState (_wiWidget oldInstance) wenv
  result = mergeHandler wenv oldState newInstance

defaultNextFocusable
  :: WidgetEnv s e -> Path -> WidgetInstance s e -> Maybe Path
defaultNextFocusable wenv startFrom widgetInst
  | isFocusCandidate startFrom widgetInst = Just (_wiPath widgetInst)
  | otherwise = Nothing

defaultFind
  :: WidgetEnv s e -> Path -> Point -> WidgetInstance s e -> Maybe Path
defaultFind wenv path point widgetInst = Just (_wiPath widgetInst)

defaultHandleEvent
  :: WidgetEnv s e
  -> Path
  -> SystemEvent
  -> WidgetInstance s e
  -> Maybe (WidgetResult s e)
defaultHandleEvent wenv target evt widgetInst = Nothing

defaultHandleMessage
  :: forall i s e m . Typeable i
  => WidgetEnv s e
  -> Path
  -> i
  -> WidgetInstance s e
  -> Maybe (WidgetResult s e)
defaultHandleMessage wenv target message widgetInst = Nothing

defaultUpdateSizeReq :: WidgetEnv s e -> WidgetInstance s e -> WidgetInstance s e
defaultUpdateSizeReq wenv widgetInst = widgetInst

defaultResize
  :: WidgetEnv s e
  -> Rect
  -> Rect
  -> WidgetInstance s e
  -> WidgetInstance s e
defaultResize wenv viewport renderArea widgetInst = widgetInst {
    _wiViewport = viewport,
    _wiRenderArea = renderArea
  }

defaultRender
  :: (Monad m) => Renderer m -> WidgetEnv s e -> WidgetInstance s e -> m ()
defaultRender renderer wenv widgetInst = return ()
