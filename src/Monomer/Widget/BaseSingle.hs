{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.BaseSingle (
  Single(..),
  createSingle
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

type SingleInitHandler s e
  = WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetResult s e

type SingleGetStateHandler s e
  = WidgetEnv s e
  -> Maybe WidgetState

type SingleMergeHandler s e
  = WidgetEnv s e
  -> Maybe WidgetState
  -> WidgetInstance s e
  -> WidgetResult s e

type SingleFindNextFocusHandler s e
  = WidgetEnv s e
  -> Path
  -> WidgetInstance s e
  -> Maybe Path

type SingleFindByPointHandler s e
  = WidgetEnv s e
  -> Path
  -> Point
  -> WidgetInstance s e
  -> Maybe Path

type SingleEventHandler s e
  = WidgetEnv s e
  -> Path
  -> SystemEvent
  -> WidgetInstance s e
  -> Maybe (WidgetResult s e)

type SingleMessageHandler s e
  = forall i . Typeable i
  => WidgetEnv s e
  -> Path
  -> i
  -> WidgetInstance s e
  -> Maybe (WidgetResult s e)

type SingleUpdateSizeReqHandler s e
  = WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetInstance s e

type SingleResizeHandler s e
  = WidgetEnv s e
  -> Rect
  -> Rect
  -> WidgetInstance s e
  -> WidgetInstance s e

type SingleRenderHandler s e
  = forall m . Monad m
  => Renderer m
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> m ()

data Single s e = Single {
  singleInit :: SingleInitHandler s e,
  singleGetState :: SingleGetStateHandler s e,
  singleMerge :: SingleMergeHandler s e,
  singleFindNextFocus :: SingleFindNextFocusHandler s e,
  singleFindByPoint :: SingleFindByPointHandler s e,
  singleHandleEvent :: SingleEventHandler s e,
  singleHandleMessage :: SingleMessageHandler s e,
  singleUpdateSizeReq :: SingleUpdateSizeReqHandler s e,
  singleResize :: SingleResizeHandler s e,
  singleRender :: SingleRenderHandler s e
}

instance Default (Single s e) where
  def = Single {
    singleInit = defaultInit,
    singleGetState = defaultGetState,
    singleMerge = defaultMerge,
    singleFindNextFocus = defaultFindNextFocus,
    singleFindByPoint = defaultFindByPoint,
    singleHandleEvent = defaultHandleEvent,
    singleHandleMessage = defaultHandleMessage,
    singleUpdateSizeReq = defaultUpdateSizeReq,
    singleResize = defaultResize,
    singleRender = defaultRender
  }

createSingle :: Single s e -> Widget s e
createSingle Single{..} = Widget {
  widgetInit = singleInit,
  widgetGetState = singleGetState,
  widgetMerge = mergeWrapper singleMerge,
  widgetFindNextFocus = singleFindNextFocus,
  widgetFindByPoint = singleFindByPoint,
  widgetHandleEvent = singleHandleEvent,
  widgetHandleMessage = singleHandleMessage,
  widgetUpdateSizeReq = singleUpdateSizeReq,
  widgetResize = singleResize,
  widgetRender = singleRender
}

defaultInit :: SingleInitHandler s e
defaultInit _ widgetInst = resultWidget widgetInst

defaultGetState :: SingleGetStateHandler s e
defaultGetState _ = Nothing

defaultMerge :: SingleMergeHandler s e
defaultMerge wenv oldState newInstance = resultWidget newInstance

mergeWrapper
  :: SingleMergeHandler s e
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetInstance s e
  -> WidgetResult s e
mergeWrapper mergeHandler wenv oldInstance newInstance = result where
  oldState = widgetGetState (_wiWidget oldInstance) wenv
  result = mergeHandler wenv oldState newInstance

defaultFindNextFocus :: SingleFindNextFocusHandler s e
defaultFindNextFocus wenv startFrom widgetInst
  | isFocusCandidate startFrom widgetInst = Just (_wiPath widgetInst)
  | otherwise = Nothing

defaultFindByPoint :: SingleFindByPointHandler s e
defaultFindByPoint wenv path point widgetInst = Just (_wiPath widgetInst)

defaultHandleEvent :: SingleEventHandler s e
defaultHandleEvent wenv target evt widgetInst = Nothing

defaultHandleMessage :: SingleMessageHandler s e
defaultHandleMessage wenv target message widgetInst = Nothing

defaultUpdateSizeReq :: SingleUpdateSizeReqHandler s e
defaultUpdateSizeReq wenv widgetInst = widgetInst

defaultResize :: SingleResizeHandler s e
defaultResize wenv viewport renderArea widgetInst = widgetInst {
    _wiViewport = viewport,
    _wiRenderArea = renderArea
  }

defaultRender :: SingleRenderHandler s e
defaultRender renderer wenv widgetInst = return ()
