{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widget.BaseContainer (
  createContainer,
  containerInit,
  containerMergeTrees,
  containerHandleEvent,
  containerHandleMessage,
  containerPreferredSize,
  containerResize,
  containerRender,
  defaultContainerRender
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

type ContainerInitHandler s e = WidgetContext s e -> PathContext -> WidgetInstance s e -> WidgetResult s e
type ContainerMergeHandler s e = WidgetContext s e -> PathContext -> Maybe WidgetState -> WidgetInstance s e -> WidgetResult s e
type ContainerEventHandler s e = WidgetContext s e -> PathContext -> SystemEvent -> WidgetInstance s e -> Maybe (WidgetResult s e)
type ContainerMessageHandler i s e = Typeable i => WidgetContext s e -> PathContext -> i -> WidgetInstance s e -> Maybe (WidgetResult s e)
type ContainerPreferredSizeHandler s e = WidgetContext s e -> WidgetInstance s e -> Seq (ChildSizeReq s e) -> Tree SizeReq
type ContainerResizeHandler s e = WidgetContext s e -> Rect -> Rect -> WidgetInstance s e -> Seq (ChildSizeReq s e) -> (WidgetInstance s e, Seq (Rect, Rect))
type ContainerRenderHandler s e m = (Monad m) => Renderer m -> WidgetContext s e -> PathContext -> WidgetInstance s e -> m ()

createContainer :: Widget s e
createContainer = Widget {
  _widgetInit = containerInit defaultInit,
  _widgetGetState = defaultGetState,
  _widgetMerge = containerMergeTrees defaultMerge,
  _widgetNextFocusable = containerNextFocusable,
  _widgetFind = containerFind,
  _widgetHandleEvent = containerHandleEvent defaultHandleEvent,
  _widgetHandleMessage = containerHandleMessage defaultHandleMessage,
  _widgetPreferredSize = containerPreferredSize defaultPreferredSize,
  _widgetResize = containerResize defaultResize,
  _widgetRender = containerRender defaultContainerRender
}

-- | Init handler
defaultInit :: ContainerInitHandler s e
defaultInit _ _ widgetInstance = resultWidget widgetInstance

containerInit :: ContainerInitHandler s e -> WidgetContext s e -> PathContext -> WidgetInstance s e -> WidgetResult s e
containerInit initHandler wctx ctx widgetInstance = WidgetResult (reqs <> newReqs) (events <> newEvents) newInstance where
  WidgetResult reqs events tempInstance = initHandler wctx ctx widgetInstance
  children = _instanceChildren tempInstance
  indexes = Seq.fromList [0..length children]
  zipper idx child = _widgetInit (_instanceWidget child) wctx (addToCurrent ctx idx) child
  results = Seq.zipWith zipper indexes children
  newReqs = fold $ fmap _resultRequests results
  newEvents = fold $ fmap _resultEvents results
  newChildren = fmap _resultWidget results
  newInstance = tempInstance {
    _instanceChildren = newChildren
  }

-- | State Handling helpers
defaultGetState :: forall i s e . Typeable i => WidgetContext s e -> Maybe i
defaultGetState _ = Nothing

-- | Merging
defaultMerge :: ContainerMergeHandler s e
defaultMerge wctx ctx state newInstance = resultWidget newInstance

containerMergeTrees :: ContainerMergeHandler s e -> WidgetContext s e -> PathContext -> WidgetInstance s e -> WidgetInstance s e -> WidgetResult s e
containerMergeTrees mergeHandler wctx ctx oldInstance newInstance = result where
  oldState = _widgetGetState (_instanceWidget oldInstance) wctx
  WidgetResult uReqs uEvents updatedInstance = mergeHandler wctx ctx oldState newInstance
  oldChildren = _instanceChildren oldInstance
  newChildren = _instanceChildren updatedInstance
  indexes = Seq.fromList [0..length newChildren]
  newPairs = Seq.zipWith (\idx child -> (addToCurrent ctx idx, child)) indexes newChildren
  mergedResults = mergeChildren wctx oldChildren newPairs
  mergedChildren = fmap _resultWidget mergedResults
  concatSeq seqs = foldl' (><) Seq.empty seqs
  mergedReqs = concatSeq $ fmap _resultRequests mergedResults
  mergedEvents = concatSeq $ fmap _resultEvents mergedResults
  mergedInstance = updatedInstance {
    _instanceChildren = mergedChildren
  }
  result = WidgetResult (uReqs <> mergedReqs) (uEvents <> mergedEvents) mergedInstance

mergeChildren :: WidgetContext s e -> Seq (WidgetInstance s e) -> Seq (PathContext, WidgetInstance s e) -> Seq (WidgetResult s e)
mergeChildren _ _ Empty = Empty
mergeChildren wctx Empty ((ctx, newChild) :<| newChildren) = child <| mergeChildren wctx Empty newChildren where
  child = _widgetInit (_instanceWidget newChild) wctx ctx newChild
mergeChildren wctx oldFull@(oldChild :<| oldChildren) ((ctx, newChild) :<| newChildren) = result where
  newWidget = _instanceWidget newChild
  oldKeyed = _instanceKey newChild >>= (\key -> M.lookup key (_wcGlobalKeys wctx))
  mergedOld = _widgetMerge newWidget wctx ctx oldChild newChild
  mergedKey = _widgetMerge newWidget wctx ctx (snd $ fromJust oldKeyed) newChild
  initNew = _widgetInit newWidget wctx ctx newChild
  (child, oldRest) = if | instanceMatches newChild oldChild -> (mergedOld, oldChildren)
                        | isJust oldKeyed -> (mergedKey, oldFull)
                        | otherwise -> (initNew, oldFull)
  result = child <| mergeChildren wctx oldRest newChildren

-- | Find next focusable item
containerNextFocusable :: WidgetContext s e -> PathContext -> WidgetInstance s e -> Maybe Path
containerNextFocusable wctx ctx widgetInstance = nextFocus where
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
    else _widgetNextFocusable (_instanceWidget child) wctx ctx child

-- | Find instance matching point
containerFind :: WidgetContext s e -> Path -> Point -> WidgetInstance s e -> Maybe Path
containerFind wctx path point widgetInstance = fmap (combinePath wctx newPath point children) childIdx where
  children = _instanceChildren widgetInstance
  pointInWidget wi = pointInRect point (_instanceViewport wi)
  newPath = Seq.drop 1 path
  childIdx = case path of
    Empty -> Seq.findIndexL pointInWidget children
    p :<| ps -> if Seq.length children > p then Just p else Nothing

combinePath :: WidgetContext s e -> Path -> Point -> Seq (WidgetInstance s e) -> Int -> Path
combinePath wctx path point children childIdx = childIdx <| childPath where
  child = Seq.index children childIdx
  childPath = fromMaybe Seq.empty $ _widgetFind (_instanceWidget child) wctx path point child

-- | Event Handling
defaultHandleEvent :: ContainerEventHandler s e
defaultHandleEvent wctx ctx evt widgetInstance = Nothing

containerHandleEvent :: ContainerEventHandler s e -> WidgetContext s e -> PathContext -> SystemEvent -> WidgetInstance s e -> Maybe (WidgetResult s e)
containerHandleEvent pHandler wctx ctx event widgetInstance
  | targetReached || not targetValid = pHandler wctx ctx event widgetInstance
  | otherwise = mergeParentChildWidgetResults widgetInstance pResponse cResponse childIdx
  where
    -- Having targetValid = False means the next path step is not in _instanceChildren, but may still be valid in the receiving widget
    -- For instance, Composite has its own tree of child widgets with (possibly) different types for Model and Events, and is a candidate for the next step
    targetValid = isTargetValid ctx children
    targetReached = isTargetReached ctx
    nextCtx = fromJust (moveToTarget ctx)
    childIdx = fromJust (nextTargetStep ctx)
    children = _instanceChildren widgetInstance
    child = Seq.index children childIdx
    pResponse = pHandler wctx ctx event widgetInstance
    cResponse = if isJust pResponse && ignoreChildren (fromJust pResponse)
                  then Nothing
                  else _widgetHandleEvent (_instanceWidget child) wctx nextCtx event child

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

-- | Message Handling
defaultHandleMessage :: ContainerMessageHandler i s e
defaultHandleMessage wctx ctx message widgetInstance = Nothing

containerHandleMessage :: forall i s e . Typeable i => ContainerMessageHandler i s e -> WidgetContext s e -> PathContext -> i -> WidgetInstance s e -> Maybe (WidgetResult s e)
containerHandleMessage mHandler wctx ctx arg widgetInstance
  | isTargetReached ctx || not (isTargetValid ctx (_instanceChildren widgetInstance)) = mHandler wctx ctx arg widgetInstance
  | otherwise = messageResult
  where
    nextCtx = fromJust $ moveToTarget ctx
    childIdx = fromJust $ nextTargetStep ctx
    children = _instanceChildren widgetInstance
    child = Seq.index children childIdx
    messageResult = updateChild <$> _widgetHandleMessage (_instanceWidget child) wctx nextCtx arg child
    updateChild cr = cr {
      _resultWidget = replaceChild widgetInstance (_resultWidget cr) childIdx
    }

-- | Preferred size
defaultPreferredSize :: ContainerPreferredSizeHandler s e
defaultPreferredSize wctx widgetInstance childrenPairs = Node current childrenReqs where
  current = SizeReq {
    _sizeRequested = Size 0 0,
    _sizePolicyWidth = FlexibleSize,
    _sizePolicyHeight = FlexibleSize
  }
  childrenReqs = fmap snd childrenPairs

containerPreferredSize :: ContainerPreferredSizeHandler s e -> WidgetContext s e -> WidgetInstance s e -> Tree SizeReq
containerPreferredSize psHandler wctx widgetInstance = psHandler wctx widgetInstance (Seq.zip children childrenReqs) where
  children = _instanceChildren widgetInstance
  childrenReqs = fmap updateChild children
  updateChild child = Node (updateSizeReq req child) reqs where
    Node req reqs = _widgetPreferredSize (_instanceWidget child) wctx child

-- | Resize
defaultResize :: ContainerResizeHandler s e
defaultResize wctx viewport renderArea widgetInstance childrenReqs = (widgetInstance, childrenSizes) where
  childrenSizes = Seq.replicate (Seq.length childrenReqs) (def, def)

containerResize :: ContainerResizeHandler s e -> WidgetContext s e -> Rect -> Rect -> WidgetInstance s e -> Tree SizeReq -> WidgetInstance s e
containerResize rHandler wctx viewport renderArea widgetInstance reqs = newInstance where
  children = _instanceChildren widgetInstance
  defReqs = Seq.replicate (Seq.length children) (singleNode def)
  curReqs = nodeChildren reqs
  childrenReqs = if Seq.null curReqs then defReqs else curReqs
  (tempInstance, assignedAreas) = rHandler wctx viewport renderArea widgetInstance (Seq.zip children childrenReqs)
  resizeChild (child, req, (viewport, renderArea)) = _widgetResize (_instanceWidget child) wctx viewport renderArea child req
  newChildren = resizeChild <$> Seq.zip3 children childrenReqs assignedAreas
  newInstance = tempInstance {
    _instanceViewport = viewport,
    _instanceRenderArea = renderArea,
    _instanceChildren = newChildren
  }

-- | Rendering
defaultContainerRender :: (Monad m) => Renderer m -> WidgetContext s e -> PathContext -> WidgetInstance s e -> m ()
defaultContainerRender  renderer wctx ctx WidgetInstance{..} =
  drawStyledBackground renderer _instanceRenderArea _instanceStyle

containerRender :: (Monad m) => ContainerRenderHandler s e m -> Renderer m -> WidgetContext s e -> PathContext -> WidgetInstance s e -> m ()
containerRender rHandler renderer wctx ctx widgetInstance = do
  let children = _instanceChildren widgetInstance
  let indexes = Seq.fromList [0..length children]
  let pairs = Seq.zip indexes children

  rHandler renderer wctx ctx widgetInstance

  forM_ pairs $ \(idx, child) ->
    when (_instanceVisible child) $ _widgetRender (_instanceWidget child) renderer wctx (addToCurrent ctx idx) child

-- | Event Handling Helpers
ignoreChildren :: WidgetResult s e -> Bool
ignoreChildren result = not . Seq.null $ Seq.filter isIgnoreChildrenEvents (_resultRequests result)

ignoreParent :: WidgetResult s e -> Bool
ignoreParent result = not . Seq.null $ Seq.filter isIgnoreParentEvents (_resultRequests result)

replaceChild :: WidgetInstance s e -> WidgetInstance s e -> Int -> WidgetInstance s e
replaceChild parent child idx = parent { _instanceChildren = newChildren } where
  newChildren = Seq.update idx child (_instanceChildren parent)
