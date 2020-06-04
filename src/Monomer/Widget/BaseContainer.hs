{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widget.BaseContainer where

import Control.Monad
import Data.Default
import Data.Maybe
import Data.Typeable (Typeable)
import Data.Sequence (Seq, (<|), (><))

import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Tree
import Monomer.Event.Core
import Monomer.Event.Types
import Monomer.Graphics.Renderer
import Monomer.Widget.PathContext
import Monomer.Widget.Types
import Monomer.Widget.Util

type WidgetMergeHandler s e m = s -> Maybe WidgetState -> WidgetInstance s e m -> WidgetInstance s e m
type WidgetEventHandler s e m = PathContext -> SystemEvent -> s -> WidgetInstance s e m -> Maybe (EventResult s e m)
type WidgetPreferredSizeHandler s e m = Monad m => Renderer m -> s -> Seq (WidgetInstance s e m, Tree SizeReq) -> Tree SizeReq
type WidgetResizeHandler s e m = s -> Rect -> Rect -> WidgetInstance s e m -> Seq (WidgetInstance s e m, Tree SizeReq) -> (WidgetInstance s e m, Seq (Rect, Rect))

createContainer :: (Monad m) => Widget s e m
createContainer = Widget {
  _widgetGetState = ignoreGetState,
  _widgetMerge = containerMergeTrees ignoreOldInstance,
  _widgetNextFocusable = containerNextFocusable,
  _widgetFind = containerFind,
  _widgetHandleEvent = containerHandleEvent ignoreEvent,
  _widgetHandleCustom = containerHandleCustom,
  _widgetPreferredSize = containerPreferredSize defaultPreferredSize,
  _widgetResize = containerResize defaultResize,
  _widgetRender = containerRender
}

-- | State Handling helpers
ignoreGetState :: forall i s . Typeable i => s -> Maybe i
ignoreGetState _ = Nothing

-- | Merging
ignoreOldInstance :: WidgetMergeHandler s e m
ignoreOldInstance app state newInstance = newInstance

{-- This implementation is far from complete --}
containerMergeTrees :: (Monad m) => WidgetMergeHandler s e m -> s -> WidgetInstance s e m -> WidgetInstance s e m -> WidgetInstance s e m
containerMergeTrees mergeWidgetState app newInstance oldInstance = if matches then mergedInstance else newInstance where
  matches = instanceMatches newInstance oldInstance
  oldState = _widgetGetState (_instanceWidget oldInstance) app
  mergedInstance = (mergeWidgetState app oldState newInstance) {
    _instanceChildren = newChildren
  }
  {-- This should also handle changes in position and global keys --}
  candidateChildren = _instanceChildren newInstance
  oldChildren = _instanceChildren oldInstance
  newChildren = mergedChildren Seq.>< addedChildren
  mergedChildren = fmap mergeChild (Seq.zip candidateChildren oldChildren)
  addedChildren = Seq.drop (Seq.length oldChildren) candidateChildren
  mergeChild = \(newChild, oldChild) -> _widgetMerge (_instanceWidget newChild) app newChild oldChild

instanceMatches :: (Monad m) => WidgetInstance s e m -> WidgetInstance s e m -> Bool
instanceMatches newInstance oldInstance = typeMatches && keyMatches where
  typeMatches = _instanceType oldInstance == _instanceType newInstance
  keyMatches = _instanceKey oldInstance == _instanceKey newInstance

-- | Find next focusable item
containerNextFocusable :: PathContext -> WidgetInstance s e m -> Maybe Path
containerNextFocusable ctx widgetInstance = nextFocus where
  children = _instanceChildren widgetInstance
  stepper idx child = (addToCurrent ctx idx, child)
  filterChildren (ctx, child) = isTargetBeforeCurrent ctx && not (isTargetReached ctx)
  indexes = Seq.fromList [0..length children]
  pairs = Seq.zipWith stepper indexes children
  maybeFocused = fmap getFocused (Seq.filter filterChildren pairs)
  focusedPaths = fmap fromJust $ Seq.filter isJust maybeFocused
  nextFocus = Seq.lookup 0 focusedPaths
  getFocused (ctx, child) = if _instanceFocusable child
    then Just (currentPath ctx)
    else _widgetNextFocusable (_instanceWidget child) ctx child

-- | Find instance matching point
containerFind :: Point -> WidgetInstance s e m -> Maybe Path
containerFind point widgetInstance = fmap (combinePath point children) childIdx where
  children = _instanceChildren widgetInstance
  pointInWidget wi = inRect (_instanceViewport wi) point
  childIdx = Seq.findIndexL pointInWidget children

combinePath :: Point -> Seq (WidgetInstance s e m) -> Int -> Path
combinePath point children childIdx = childIdx <| childPath where
  child = Seq.index children childIdx
  childPath = fromMaybe Seq.empty $ _widgetFind (_instanceWidget child) point child

-- | Event Handling
ignoreEvent :: WidgetEventHandler s e m
ignoreEvent ctx evt app widgetInstance = Nothing

containerHandleEvent :: WidgetEventHandler s e m -> PathContext -> SystemEvent -> s -> WidgetInstance s e m -> Maybe (EventResult s e m)
containerHandleEvent pHandler ctx event app widgetInstance
  | isTargetReached ctx || not (isTargetValid ctx (_instanceChildren widgetInstance)) = pHandler ctx event app widgetInstance
  | otherwise = mergeParentChildEventResults widgetInstance pResult cResult childIdx where
      nextCtx = fromJust $ moveToTarget ctx
      childIdx = fromJust $ nextTargetStep ctx
      children = _instanceChildren widgetInstance
      child = Seq.index children childIdx
      pResult = pHandler ctx event app widgetInstance
      cResult = _widgetHandleEvent (_instanceWidget child) nextCtx event app child

mergeParentChildEventResults :: WidgetInstance s e m -> Maybe (EventResult s e m) -> Maybe (EventResult s e m) -> Int -> Maybe (EventResult s e m)
mergeParentChildEventResults _ Nothing Nothing _ = Nothing
mergeParentChildEventResults _ pResult Nothing _ = pResult
mergeParentChildEventResults original Nothing (Just cResult) idx = Just $ cResult {
    _eventResultNewWidget = replaceChild original (_eventResultNewWidget cResult) idx
  }
mergeParentChildEventResults original (Just pResult) (Just cResult) idx
  | ignoreChildren pResult = Just pResult
  | ignoreParent cResult = Just $ cResult {
      _eventResultNewWidget = replaceChild original (_eventResultNewWidget cResult) idx
    }
  | otherwise = Just $ EventResult requests userEvents newWidget where
      requests = _eventResultRequest pResult >< _eventResultRequest cResult
      userEvents = _eventResultUserEvents pResult >< _eventResultUserEvents cResult
      newWidget = replaceChild (_eventResultNewWidget pResult) (_eventResultNewWidget cResult) idx

-- | Custom Handling
containerHandleCustom :: forall i s e m . Typeable i => PathContext -> i -> s -> WidgetInstance s e m -> Maybe (EventResult s e m)
containerHandleCustom ctx arg app widgetInstance
  | isTargetReached ctx || not (isTargetValid ctx (_instanceChildren widgetInstance)) = Nothing
  | otherwise = customResult where
      nextCtx = fromJust $ moveToTarget ctx
      childIdx = fromJust $ nextTargetStep ctx
      children = _instanceChildren widgetInstance
      child = Seq.index children childIdx
      customResult = flip fmap (_widgetHandleCustom (_instanceWidget child) nextCtx arg app child) $
        \cr@(EventResult _ _ newChild) -> cr { _eventResultNewWidget = replaceChild widgetInstance newChild childIdx }

-- | Preferred size
defaultPreferredSize :: WidgetPreferredSizeHandler s e m
defaultPreferredSize renderer app childrenPairs = Node current childrenReqs where
  current = SizeReq {
    _sizeRequested = Size 0 0,
    _sizePolicyWidth = FlexibleSize,
    _sizePolicyHeight = FlexibleSize
  }
  childrenReqs = fmap snd childrenPairs

containerPreferredSize :: (Monad m) => WidgetPreferredSizeHandler s e m -> Renderer m -> s -> WidgetInstance s e m -> Tree SizeReq
containerPreferredSize psHandler renderer app widgetInstance = psHandler renderer app (Seq.zip children childrenReqs) where
  children = _instanceChildren widgetInstance
  childrenReqs = flip fmap children updateChild
  updateChild child = Node (updateSizeReq req child) reqs where
    Node req reqs = _widgetPreferredSize (_instanceWidget child) renderer app child

-- | Resize
defaultResize :: WidgetResizeHandler s e m
defaultResize app viewport renderArea widgetInstance childrenReqs = (widgetInstance, childrenSizes) where
  childrenSizes = Seq.replicate (Seq.length childrenReqs) (def, def)

containerResize :: (Monad m) => WidgetResizeHandler s e m -> s -> Rect -> Rect -> WidgetInstance s e m -> Tree SizeReq -> WidgetInstance s e m
containerResize rHandler app viewport renderArea widgetInstance reqs = newInstance where
  newInstance = tempInstance {
    _instanceViewport = viewport,
    _instanceRenderArea = renderArea,
    _instanceChildren = newChildren
  }
  children = _instanceChildren widgetInstance
  childrenReqs = nodeChildren reqs
  (tempInstance, assignedAreas) = rHandler app viewport renderArea widgetInstance (Seq.zip children childrenReqs)
  newChildren = flip fmap (Seq.zip3 children childrenReqs assignedAreas) $
    \(child, req, (viewport, renderArea)) -> _widgetResize (_instanceWidget child) app viewport renderArea child req

-- | Rendering
containerRender :: (Monad m) => Renderer m -> Timestamp -> PathContext -> s -> WidgetInstance s e m -> m ()
containerRender renderer ts ctx app widgetInstance = do
  let children = _instanceChildren widgetInstance
  let indexes = Seq.fromList [0..length children]
  let pairs = Seq.zip indexes children

  forM_ pairs $ \(idx, child) ->
    when (_instanceVisible child) $ _widgetRender (_instanceWidget child) renderer ts (addToCurrent ctx idx) app child

-- | Event Handling Helpers
ignoreChildren :: EventResult s e m -> Bool
ignoreChildren result = Seq.null $ Seq.filter isIgnoreChildrenEvents (_eventResultRequest result)

ignoreParent :: EventResult s e m -> Bool
ignoreParent result = Seq.null $ Seq.filter isIgnoreParentEvents (_eventResultRequest result)

replaceChild :: WidgetInstance s e m -> WidgetInstance s e m -> Int -> WidgetInstance s e m
replaceChild parent child idx = parent { _instanceChildren = newChildren } where
  newChildren = Seq.update idx child (_instanceChildren parent)
