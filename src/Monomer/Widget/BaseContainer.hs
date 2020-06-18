{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widget.BaseContainer (
  createContainer,
  containerHandleEvent,
  containerPreferredSize,
  containerResize,
  containerRender
) where

import Control.Monad
import Data.Default
import Data.Foldable (fold)
import Data.List (foldl')
import Data.Maybe
import Data.Typeable (Typeable)
import Data.Sequence (Seq(..), (<|), (><))

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Tree
import Monomer.Event.Core
import Monomer.Event.Types
import Monomer.Graphics.Renderer
import Monomer.Widget.PathContext
import Monomer.Widget.Types
import Monomer.Widget.Util

type ChildSizeReq s e = (WidgetInstance s e, Tree SizeReq)

type WidgetInitHandler s e = PathContext -> s -> WidgetInstance s e -> WidgetResult s e
type WidgetMergeHandler s e = s -> Maybe WidgetState -> WidgetInstance s e -> WidgetInstance s e
type WidgetEventHandler s e m = PathContext -> SystemEvent -> s -> WidgetInstance s e -> Maybe (WidgetResult s e)
type WidgetPreferredSizeHandler s e m = Monad m => Renderer m -> s -> Seq (WidgetInstance s e, Tree SizeReq) -> Tree SizeReq
type WidgetResizeHandler s e = s -> Rect -> Rect -> WidgetInstance s e -> Seq (ChildSizeReq s e) -> (WidgetInstance s e, Seq (Rect, Rect))

createContainer :: Widget s e
createContainer = Widget {
  _widgetInit = containerInit defaultInit,
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

-- | Init handler
defaultInit :: WidgetInitHandler s e
defaultInit _ _ widgetInstance = resultWidget widgetInstance

containerInit :: WidgetInitHandler s e -> PathContext -> s -> WidgetInstance s e -> WidgetResult s e
containerInit initHandler ctx app widgetInstance = WidgetResult (reqs <> newReqs) (events <> newEvents) newInstance where
  children = _instanceChildren widgetInstance
  indexes = Seq.fromList [0..length children]
  zipper idx child = _widgetInit (_instanceWidget child) (addToCurrent ctx idx) app child
  results = Seq.zipWith zipper indexes children
  newReqs = fold $ fmap _resultRequests results
  newEvents = fold $ fmap _resultEvents results
  newChildren = fmap _resultWidget results
  WidgetResult reqs events tempInstance = initHandler ctx app widgetInstance
  newInstance = tempInstance {
    _instanceChildren = newChildren
  }

-- | State Handling helpers
ignoreGetState :: forall i s . Typeable i => s -> Maybe i
ignoreGetState _ = Nothing

-- | Merging
ignoreOldInstance :: WidgetMergeHandler s e
ignoreOldInstance app state newInstance = newInstance

{-- This implementation is far from complete --}
containerMergeTrees :: WidgetMergeHandler s e -> GlobalKeys s e -> PathContext -> s -> WidgetInstance s e -> WidgetInstance s e -> WidgetResult s e
containerMergeTrees mergeWidgetState globalKeys ctx app newInstance oldInstance = result where
  oldState = _widgetGetState (_instanceWidget oldInstance) app
  oldChildren = _instanceChildren oldInstance
  newChildren = _instanceChildren newInstance
  indexes = Seq.fromList [0..length newChildren]
  newPairs = Seq.zipWith (\idx child -> (addToCurrent ctx idx, child)) indexes newChildren
  mergedResults = mergeChildren globalKeys app newPairs oldChildren
  mergedChildren = fmap _resultWidget mergedResults
  mergedReqs = concatSeq $ fmap _resultRequests mergedResults
  mergedEvents = concatSeq $ fmap _resultEvents mergedResults
  mergedInstance = (mergeWidgetState app oldState newInstance) {
    _instanceChildren = mergedChildren
  }
  result = WidgetResult mergedReqs mergedEvents mergedInstance

mergeChildren :: GlobalKeys s e -> s -> Seq (PathContext, WidgetInstance s e) -> Seq (WidgetInstance s e) -> Seq (WidgetResult s e)
mergeChildren _ _ Empty _ = Empty
mergeChildren keys app ((ctx, newChild) :<| newChildren) Empty = child <| mergeChildren keys app newChildren Empty where
  child = _widgetInit (_instanceWidget newChild) ctx app newChild
mergeChildren keys app ((ctx, newChild) :<| newChildren) oldFull@(oldChild :<| oldChildren) = result where
  newWidget = _instanceWidget newChild
  oldKeyed = maybe Nothing (\key -> M.lookup key keys) (_instanceKey newChild)
  mergedOld = _widgetMerge newWidget keys ctx app newChild oldChild
  mergedKey = _widgetMerge newWidget keys ctx app newChild (snd $ fromJust oldKeyed)
  initNew = _widgetInit newWidget ctx app newChild
  (child, oldRest) = if | instanceMatches newChild oldChild -> (mergedOld, oldChildren)
                        | isJust oldKeyed -> (mergedKey, oldFull)
                        | otherwise -> (initNew, oldFull)
  result = child <| mergeChildren keys app newChildren oldRest

concatSeq :: Seq (Seq a) -> Seq a
concatSeq seqs = foldl' (><) Seq.empty seqs

-- | Find next focusable item
containerNextFocusable :: PathContext -> WidgetInstance s e -> Maybe Path
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
containerFind :: Point -> WidgetInstance s e -> Maybe Path
containerFind point widgetInstance = fmap (combinePath point children) childIdx where
  children = _instanceChildren widgetInstance
  pointInWidget wi = inRect (_instanceViewport wi) point
  childIdx = Seq.findIndexL pointInWidget children

combinePath :: Point -> Seq (WidgetInstance s e) -> Int -> Path
combinePath point children childIdx = childIdx <| childPath where
  child = Seq.index children childIdx
  childPath = fromMaybe Seq.empty $ _widgetFind (_instanceWidget child) point child

-- | Event Handling
ignoreEvent :: WidgetEventHandler s e m
ignoreEvent ctx evt app widgetInstance = Nothing

containerHandleEvent :: WidgetEventHandler s e m -> PathContext -> SystemEvent -> s -> WidgetInstance s e -> Maybe (WidgetResult s e)
containerHandleEvent pHandler ctx event app widgetInstance
  | isTargetReached ctx || not (isTargetValid ctx (_instanceChildren widgetInstance)) = pHandler ctx event app widgetInstance
  | otherwise = mergeParentChildWidgetResults widgetInstance pResponse cResponse childIdx where
      nextCtx = fromJust $ moveToTarget ctx
      childIdx = fromJust $ nextTargetStep ctx
      children = _instanceChildren widgetInstance
      child = Seq.index children childIdx
      pResponse = pHandler ctx event app widgetInstance
      cResponse = _widgetHandleEvent (_instanceWidget child) nextCtx event app child

mergeParentChildWidgetResults :: WidgetInstance s e -> Maybe (WidgetResult s e) -> Maybe (WidgetResult s e) -> Int -> Maybe (WidgetResult s e)
mergeParentChildWidgetResults _ Nothing Nothing _ = Nothing
mergeParentChildWidgetResults _ pResponse Nothing _ = pResponse
mergeParentChildWidgetResults original Nothing (Just cResponse) idx = Just $ cResponse {
    _resultWidget = replaceChild original (_resultWidget cResponse) idx
  }
mergeParentChildWidgetResults original (Just pResponse) (Just cResponse) idx
  | ignoreChildren pResponse = Just pResponse
  | ignoreParent cResponse = Just $ cResponse {
      _resultWidget = replaceChild original (_resultWidget cResponse) idx
    }
  | otherwise = Just $ WidgetResult requests userEvents newWidget where
      requests = _resultRequests pResponse >< _resultRequests cResponse
      userEvents = _resultEvents pResponse >< _resultEvents cResponse
      newWidget = replaceChild (_resultWidget pResponse) (_resultWidget cResponse) idx

-- | Custom Handling
containerHandleCustom :: forall i s e m . Typeable i => PathContext -> i -> s -> WidgetInstance s e -> Maybe (WidgetResult s e)
containerHandleCustom ctx arg app widgetInstance
  | isTargetReached ctx || not (isTargetValid ctx (_instanceChildren widgetInstance)) = Nothing
  | otherwise = customResult where
      nextCtx = fromJust $ moveToTarget ctx
      childIdx = fromJust $ nextTargetStep ctx
      children = _instanceChildren widgetInstance
      child = Seq.index children childIdx
      customResult = flip fmap (_widgetHandleCustom (_instanceWidget child) nextCtx arg app child) $
        \cr@(WidgetResult _ _ newChild) -> cr { _resultWidget = replaceChild widgetInstance newChild childIdx }

-- | Preferred size
defaultPreferredSize :: WidgetPreferredSizeHandler s e m
defaultPreferredSize renderer app childrenPairs = Node current childrenReqs where
  current = SizeReq {
    _sizeRequested = Size 0 0,
    _sizePolicyWidth = FlexibleSize,
    _sizePolicyHeight = FlexibleSize
  }
  childrenReqs = fmap snd childrenPairs

containerPreferredSize :: (Monad m) => WidgetPreferredSizeHandler s e m -> Renderer m -> s -> WidgetInstance s e -> Tree SizeReq
containerPreferredSize psHandler renderer app widgetInstance = psHandler renderer app (Seq.zip children childrenReqs) where
  children = _instanceChildren widgetInstance
  childrenReqs = flip fmap children updateChild
  updateChild child = Node (updateSizeReq req child) reqs where
    Node req reqs = _widgetPreferredSize (_instanceWidget child) renderer app child

-- | Resize
defaultResize :: WidgetResizeHandler s e
defaultResize app viewport renderArea widgetInstance childrenReqs = (widgetInstance, childrenSizes) where
  childrenSizes = Seq.replicate (Seq.length childrenReqs) (def, def)

containerResize :: WidgetResizeHandler s e -> s -> Rect -> Rect -> WidgetInstance s e -> Tree SizeReq -> WidgetInstance s e
containerResize rHandler app viewport renderArea widgetInstance reqs = newInstance where
  newInstance = tempInstance {
    _instanceViewport = viewport,
    _instanceRenderArea = renderArea,
    _instanceChildren = newChildren
  }
  children = _instanceChildren widgetInstance
  defReqs = Seq.replicate (Seq.length children) (singleNode def)
  curReqs = nodeChildren reqs
  childrenReqs = if Seq.null curReqs then defReqs else curReqs
  (tempInstance, assignedAreas) = rHandler app viewport renderArea widgetInstance (Seq.zip children childrenReqs)
  newChildren = flip fmap (Seq.zip3 children childrenReqs assignedAreas) $
    \(child, req, (viewport, renderArea)) -> _widgetResize (_instanceWidget child) app viewport renderArea child req

-- | Rendering
containerRender :: (Monad m) => Renderer m -> Timestamp -> PathContext -> s -> WidgetInstance s e -> m ()
containerRender renderer ts ctx app widgetInstance = do
  let children = _instanceChildren widgetInstance
  let indexes = Seq.fromList [0..length children]
  let pairs = Seq.zip indexes children

  forM_ pairs $ \(idx, child) ->
    when (_instanceVisible child) $ _widgetRender (_instanceWidget child) renderer ts (addToCurrent ctx idx) app child

-- | Event Handling Helpers
ignoreChildren :: WidgetResult s e -> Bool
ignoreChildren result = Seq.null $ Seq.filter isIgnoreChildrenEvents (_resultRequests result)

ignoreParent :: WidgetResult s e -> Bool
ignoreParent result = Seq.null $ Seq.filter isIgnoreParentEvents (_resultRequests result)

replaceChild :: WidgetInstance s e -> WidgetInstance s e -> Int -> WidgetInstance s e
replaceChild parent child idx = parent { _instanceChildren = newChildren } where
  newChildren = Seq.update idx child (_instanceChildren parent)
