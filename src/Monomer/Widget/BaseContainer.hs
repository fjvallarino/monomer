{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widget.BaseContainer (
  createContainer,
  containerHandleEvent,
  containerPreferredSize,
  containerResize,
  containerRender,
  defaultRender
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
import Monomer.Graphics.Drawing
import Monomer.Graphics.Renderer
import Monomer.Widget.PathContext
import Monomer.Widget.Types
import Monomer.Widget.Util

type ChildSizeReq s e = (WidgetInstance s e, Tree SizeReq)

type WidgetInitHandler s e = WidgetContext s e -> PathContext -> WidgetInstance s e -> WidgetResult s e
type WidgetMergeHandler s e = WidgetContext s e -> Maybe WidgetState -> WidgetInstance s e -> WidgetInstance s e
type WidgetEventHandler s e m = WidgetContext s e -> PathContext -> SystemEvent -> WidgetInstance s e -> Maybe (WidgetResult s e)
type WidgetPreferredSizeHandler s e m = Monad m => Renderer m -> WidgetContext s e -> Seq (WidgetInstance s e, Tree SizeReq) -> Tree SizeReq
type WidgetResizeHandler s e = WidgetContext s e -> Rect -> Rect -> WidgetInstance s e -> Seq (ChildSizeReq s e) -> (WidgetInstance s e, Seq (Rect, Rect))
type WidgetRenderHandler s e m = (Monad m) => Renderer m -> WidgetContext s e -> PathContext -> WidgetInstance s e -> m ()

createContainer :: Widget s e
createContainer = Widget {
  _widgetInit = containerInit defaultInit,
  _widgetGetState = ignoreGetState,
  _widgetMerge = containerMergeTrees ignoreOldInstance,
  _widgetNextFocusable = containerNextFocusable,
  _widgetFind = containerFind,
  _widgetHandleEvent = containerHandleEvent ignoreEvent,
  _widgetHandleMessage = containerHandleMessage,
  _widgetPreferredSize = containerPreferredSize defaultPreferredSize,
  _widgetResize = containerResize defaultResize,
  _widgetRender = containerRender defaultRender
}

-- | Init handler
defaultInit :: WidgetInitHandler s e
defaultInit _ _ widgetInstance = resultWidget widgetInstance

containerInit :: WidgetInitHandler s e -> WidgetContext s e -> PathContext -> WidgetInstance s e -> WidgetResult s e
containerInit initHandler wctx ctx widgetInstance = WidgetResult (reqs <> newReqs) (events <> newEvents) newInstance where
  children = _instanceChildren widgetInstance
  indexes = Seq.fromList [0..length children]
  zipper idx child = _widgetInit (_instanceWidget child) wctx (addToCurrent ctx idx) child
  results = Seq.zipWith zipper indexes children
  newReqs = fold $ fmap _resultRequests results
  newEvents = fold $ fmap _resultEvents results
  newChildren = fmap _resultWidget results
  WidgetResult reqs events tempInstance = initHandler wctx ctx widgetInstance
  newInstance = tempInstance {
    _instanceChildren = newChildren
  }

-- | State Handling helpers
ignoreGetState :: forall i s e . Typeable i => WidgetContext s e -> Maybe i
ignoreGetState _ = Nothing

-- | Merging
ignoreOldInstance :: WidgetMergeHandler s e
ignoreOldInstance app state newInstance = newInstance

{-- This implementation is far from complete --}
containerMergeTrees :: WidgetMergeHandler s e -> WidgetContext s e -> PathContext -> WidgetInstance s e -> WidgetInstance s e -> WidgetResult s e
containerMergeTrees mergeWidgetState wctx ctx newInstance oldInstance = result where
  oldState = _widgetGetState (_instanceWidget oldInstance) wctx
  oldChildren = _instanceChildren oldInstance
  newChildren = _instanceChildren newInstance
  indexes = Seq.fromList [0..length newChildren]
  newPairs = Seq.zipWith (\idx child -> (addToCurrent ctx idx, child)) indexes newChildren
  mergedResults = mergeChildren wctx newPairs oldChildren
  mergedChildren = fmap _resultWidget mergedResults
  concatSeq seqs = foldl' (><) Seq.empty seqs
  mergedReqs = concatSeq $ fmap _resultRequests mergedResults
  mergedEvents = concatSeq $ fmap _resultEvents mergedResults
  mergedInstance = (mergeWidgetState wctx oldState newInstance) {
    _instanceChildren = mergedChildren
  }
  result = WidgetResult mergedReqs mergedEvents mergedInstance

mergeChildren :: WidgetContext s e -> Seq (PathContext, WidgetInstance s e) -> Seq (WidgetInstance s e) -> Seq (WidgetResult s e)
mergeChildren _ Empty _ = Empty
mergeChildren wctx ((ctx, newChild) :<| newChildren) Empty = child <| mergeChildren wctx newChildren Empty where
  child = _widgetInit (_instanceWidget newChild) wctx ctx newChild
mergeChildren wctx ((ctx, newChild) :<| newChildren) oldFull@(oldChild :<| oldChildren) = result where
  newWidget = _instanceWidget newChild
  oldKeyed = _instanceKey newChild >>= (\key -> M.lookup key (_wcGlobalKeys wctx))
  mergedOld = _widgetMerge newWidget wctx ctx newChild oldChild
  mergedKey = _widgetMerge newWidget wctx ctx newChild (snd $ fromJust oldKeyed)
  initNew = _widgetInit newWidget wctx ctx newChild
  (child, oldRest) = if | instanceMatches newChild oldChild -> (mergedOld, oldChildren)
                        | isJust oldKeyed -> (mergedKey, oldFull)
                        | otherwise -> (initNew, oldFull)
  result = child <| mergeChildren wctx newChildren oldRest

-- | Find next focusable item
containerNextFocusable :: PathContext -> WidgetInstance s e -> Maybe Path
containerNextFocusable ctx widgetInstance = nextFocus where
  children = _instanceChildren widgetInstance
  stepper idx child = (addToCurrent ctx idx, child)
  filterChildren (ctx, child) = isTargetBeforeCurrent ctx && not (isTargetReached ctx)
  indexes = Seq.fromList [0..length children]
  pairs = Seq.zipWith stepper indexes children
  maybeFocused = fmap getFocused (Seq.filter filterChildren pairs)
  focusedPaths = fromJust <$> Seq.filter isJust maybeFocused
  nextFocus = Seq.lookup 0 focusedPaths
  getFocused (ctx, child) = if _instanceFocusable child
    then Just (currentPath ctx)
    else _widgetNextFocusable (_instanceWidget child) ctx child

-- | Find instance matching point
containerFind :: Path -> Point -> WidgetInstance s e -> Maybe Path
containerFind path point widgetInstance = fmap (combinePath newPath point children) childIdx where
  children = _instanceChildren widgetInstance
  pointInWidget wi = inRect (_instanceViewport wi) point
  newPath = Seq.drop 1 path
  childIdx = case path of
    Empty -> Seq.findIndexL pointInWidget children
    p :<| ps -> if Seq.length children > p then Just p else Nothing

combinePath :: Path -> Point -> Seq (WidgetInstance s e) -> Int -> Path
combinePath path point children childIdx = childIdx <| childPath where
  child = Seq.index children childIdx
  childPath = fromMaybe Seq.empty $ _widgetFind (_instanceWidget child) path point child

-- | Event Handling
ignoreEvent :: WidgetEventHandler s e m
ignoreEvent wctx ctx evt widgetInstance = Nothing

containerHandleEvent :: WidgetEventHandler s e m -> WidgetContext s e -> PathContext -> SystemEvent -> WidgetInstance s e -> Maybe (WidgetResult s e)
containerHandleEvent pHandler wctx ctx event widgetInstance
  | isTargetReached ctx || not (isTargetValid ctx (_instanceChildren widgetInstance)) = pHandler wctx ctx event widgetInstance
  | otherwise = mergeParentChildWidgetResults widgetInstance pResponse cResponse childIdx where
      nextCtx = fromJust $ moveToTarget ctx
      childIdx = fromJust $ nextTargetStep ctx
      children = _instanceChildren widgetInstance
      child = Seq.index children childIdx
      pResponse = pHandler wctx ctx event widgetInstance
      cResponse = _widgetHandleEvent (_instanceWidget child) wctx nextCtx event child

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
containerHandleMessage :: forall i s e m . Typeable i => WidgetContext s e -> PathContext -> i -> WidgetInstance s e -> Maybe (WidgetResult s e)
containerHandleMessage wctx ctx arg widgetInstance
  | isTargetReached ctx || not (isTargetValid ctx (_instanceChildren widgetInstance)) = Nothing
  | otherwise = messageResult where
      nextCtx = fromJust $ moveToTarget ctx
      childIdx = fromJust $ nextTargetStep ctx
      children = _instanceChildren widgetInstance
      child = Seq.index children childIdx
      messageResult = flip fmap (_widgetHandleMessage (_instanceWidget child) wctx nextCtx arg child) $
        \cr -> cr { _resultWidget = replaceChild widgetInstance (_resultWidget cr) childIdx }

-- | Preferred size
defaultPreferredSize :: WidgetPreferredSizeHandler s e m
defaultPreferredSize renderer app childrenPairs = Node current childrenReqs where
  current = SizeReq {
    _sizeRequested = Size 0 0,
    _sizePolicyWidth = FlexibleSize,
    _sizePolicyHeight = FlexibleSize
  }
  childrenReqs = fmap snd childrenPairs

containerPreferredSize :: (Monad m) => WidgetPreferredSizeHandler s e m -> Renderer m -> WidgetContext s e -> WidgetInstance s e -> Tree SizeReq
containerPreferredSize psHandler renderer wctx widgetInstance = psHandler renderer wctx (Seq.zip children childrenReqs) where
  children = _instanceChildren widgetInstance
  childrenReqs = flip fmap children updateChild
  updateChild child = Node (updateSizeReq req child) reqs where
    Node req reqs = _widgetPreferredSize (_instanceWidget child) renderer wctx child

-- | Resize
defaultResize :: WidgetResizeHandler s e
defaultResize wctx viewport renderArea widgetInstance childrenReqs = (widgetInstance, childrenSizes) where
  childrenSizes = Seq.replicate (Seq.length childrenReqs) (def, def)

containerResize :: WidgetResizeHandler s e -> WidgetContext s e -> Rect -> Rect -> WidgetInstance s e -> Tree SizeReq -> WidgetInstance s e
containerResize rHandler wctx viewport renderArea widgetInstance reqs = newInstance where
  newInstance = tempInstance {
    _instanceViewport = viewport,
    _instanceRenderArea = renderArea,
    _instanceChildren = newChildren
  }
  children = _instanceChildren widgetInstance
  defReqs = Seq.replicate (Seq.length children) (singleNode def)
  curReqs = nodeChildren reqs
  childrenReqs = if Seq.null curReqs then defReqs else curReqs
  (tempInstance, assignedAreas) = rHandler wctx viewport renderArea widgetInstance (Seq.zip children childrenReqs)
  newChildren = flip fmap (Seq.zip3 children childrenReqs assignedAreas) $
    \(child, req, (viewport, renderArea)) -> _widgetResize (_instanceWidget child) wctx viewport renderArea child req

-- | Rendering
defaultRender :: (Monad m) => Renderer m -> WidgetContext s e -> PathContext -> WidgetInstance s e -> m ()
defaultRender  renderer wctx ctx WidgetInstance{..} =
  drawStyledBackground renderer _instanceRenderArea _instanceStyle

containerRender :: (Monad m) => WidgetRenderHandler s e m -> Renderer m -> WidgetContext s e -> PathContext -> WidgetInstance s e -> m ()
containerRender rHandler renderer wctx ctx widgetInstance = do
  let children = _instanceChildren widgetInstance
  let indexes = Seq.fromList [0..length children]
  let pairs = Seq.zip indexes children

  rHandler renderer wctx ctx widgetInstance

  forM_ pairs $ \(idx, child) ->
    when (_instanceVisible child) $ _widgetRender (_instanceWidget child) renderer wctx (addToCurrent ctx idx) child

-- | Event Handling Helpers
ignoreChildren :: WidgetResult s e -> Bool
ignoreChildren result = Seq.null $ Seq.filter isIgnoreChildrenEvents (_resultRequests result)

ignoreParent :: WidgetResult s e -> Bool
ignoreParent result = Seq.null $ Seq.filter isIgnoreParentEvents (_resultRequests result)

replaceChild :: WidgetInstance s e -> WidgetInstance s e -> Int -> WidgetInstance s e
replaceChild parent child idx = parent { _instanceChildren = newChildren } where
  newChildren = Seq.update idx child (_instanceChildren parent)
