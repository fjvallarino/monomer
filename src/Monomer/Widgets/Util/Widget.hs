{-|
Module      : Monomer.Widgets.Util.Widget
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions for widget lifecycle.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Util.Widget (
  defaultWidgetNode,
  isWidgetVisible,
  nodeVisibleChanged,
  nodeEnabledChanged,
  nodeFlagsChanged,
  childrenVisibleChanged,
  childrenEnabledChanged,
  childrenFlagsChanged,
  widgetDataGet,
  widgetDataSet,
  resultNode,
  resultEvts,
  resultReqs,
  resultReqsEvts,
  makeState,
  useState,
  useShared,
  infoMatches,
  nodeMatches,
  handleWidgetIdChange,
  findWidgetIdFromPath,
  delayedMessage,
  delayedMessage_
) where

import Control.Concurrent (threadDelay)
import Control.Lens ((&), (^#), (#~), (^.), (^?), (.~), (%~), _Just)
import Data.ByteString.Lazy (ByteString)
import Data.Default
import Data.Maybe
import Data.Map.Strict (Map)
import Data.Sequence (Seq(..), (<|))
import Data.Text (Text)
import Data.Typeable (Typeable, cast)

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Monomer.Common
import Monomer.Core.WidgetTypes

import qualified Monomer.Core.Lens as L

-- | Creates a basic widget node, with the given type, instance and no children.
defaultWidgetNode :: WidgetType -> Widget s e -> WidgetNode s e
defaultWidgetNode widgetType widget = WidgetNode {
  _wnWidget = widget,
  _wnInfo = def & L.widgetType .~ widgetType,
  _wnChildren = Seq.empty
}

-- | Checks if the node is within the visible viewport, and itself visible.
isWidgetVisible :: WidgetEnv s e -> WidgetNode s e -> Bool
isWidgetVisible wenv node = isVisible && isOverlapped where
  info = node ^. L.info
  isVisible = info ^. L.visible
  viewport = wenv ^. L.viewport
  isOverlapped = rectsOverlap viewport (info ^. L.viewport)

-- | Checks if the visibility flags changed between the old and new node.
nodeVisibleChanged :: WidgetNode s e -> WidgetNode s e -> Bool
nodeVisibleChanged oldNode newNode = oldVisible /= newVisible where
  oldVisible = oldNode ^. L.info . L.visible
  newVisible = newNode ^. L.info . L.visible

-- | Checks if the enabled flags changed between the old and new node.
nodeEnabledChanged :: WidgetNode s e -> WidgetNode s e -> Bool
nodeEnabledChanged oldNode newNode = oldEnabled /= newEnabled where
  oldEnabled = oldNode ^. L.info . L.enabled
  newEnabled = newNode ^. L.info . L.enabled

-- | Checks if the enabled/visible flags changed between the old and new node.
nodeFlagsChanged :: WidgetNode s e -> WidgetNode s e -> Bool
nodeFlagsChanged oldNode newNode = visibleChanged || enabledChanged where
  visibleChanged = nodeVisibleChanged oldNode newNode
  enabledChanged = nodeEnabledChanged oldNode newNode

-- | Checks if the visibility flags changed between the old and new children.
--   A change in count will result in a True result.
childrenVisibleChanged :: WidgetNode s e -> WidgetNode s e -> Bool
childrenVisibleChanged oldNode newNode = oldVisible /= newVisible where
  oldVisible = fmap (^. L.info . L.visible) (oldNode ^. L.children)
  newVisible = fmap (^. L.info . L.visible) (newNode ^. L.children)

-- | Checks if the enabled flags changed between the old and new children.
--   A change in count will result in a True result.
childrenEnabledChanged :: WidgetNode s e -> WidgetNode s e -> Bool
childrenEnabledChanged oldNode newNode = oldVisible /= newVisible where
  oldVisible = fmap (^. L.info . L.enabled) (oldNode ^. L.children)
  newVisible = fmap (^. L.info . L.enabled) (newNode ^. L.children)

-- | Checks if enabled/visible flags changed between the old and new children.
--   A change in count will result in a True result.
childrenFlagsChanged :: WidgetNode s e -> WidgetNode s e -> Bool
childrenFlagsChanged oldNode newNode = lenChanged || flagsChanged where
  oldChildren = oldNode ^. L.children
  newChildren = newNode ^. L.children
  flagsChanged = or (Seq.zipWith nodeFlagsChanged oldChildren newChildren)
  lenChanged = length oldChildren /= length newChildren

-- | Returns the current value associated to the WidgetData.
widgetDataGet :: s -> WidgetData s a -> a
widgetDataGet _ (WidgetValue value) = value
widgetDataGet model (WidgetLens lens) = model ^# lens

{-|
Generates a model update request with the provided value when the WidgetData is
WidgetLens. For WidgetValue and onChange event should be used.
-}
widgetDataSet :: WidgetData s a -> a -> [WidgetRequest s e]
widgetDataSet WidgetValue{} _ = []
widgetDataSet (WidgetLens lens) value = [UpdateModel updateFn] where
  updateFn model = model & lens #~ value

-- | Generates a WidgetResult with only the node field filled.
resultNode :: WidgetNode s e -> WidgetResult s e
resultNode node = WidgetResult node Seq.empty

-- | Generates a WidgetResult with the node field and events filled.
resultEvts :: Typeable e => WidgetNode s e -> [e] -> WidgetResult s e
resultEvts node events = result where
  result = WidgetResult node (Seq.fromList $ RaiseEvent <$> events)

-- | Generates a WidgetResult with the node field and reqs filled.
resultReqs :: WidgetNode s e -> [WidgetRequest s e] -> WidgetResult s e
resultReqs node requests = result where
  result = WidgetResult node (Seq.fromList requests)

{-|
Generates a WidgetResult with the node, events and reqs fields filled. These
related helpers exist because list has nicer literal syntax than Seq.

The events are appended __after__ the requests. If a specific order of events
and requests is needed, add the events to reqs using RaiseEvent.
-}
resultReqsEvts
  :: Typeable e
  => WidgetNode s e       -- ^ The new version of the node.
  -> [WidgetRequest s e]  -- ^ The widget requests.
  -> [e]                  -- ^ The user events.
  -> WidgetResult s e     -- ^ The result.
resultReqsEvts node requests events = result where
  result = WidgetResult node (Seq.fromList requests <> evtSeq)
  evtSeq = Seq.fromList $ RaiseEvent <$> events

{-|
Wraps a value in WidgetState, ignoring wenv and node. Useful when creating
Widget instances if the state is available beforehand.
-}
makeState
  :: WidgetModel i => i -> WidgetEnv s e -> WidgetNode s e -> Maybe WidgetState
makeState state wenv node = Just (WidgetState state)

-- | Casts the wrapped value in WidgetState to the expected type, if possible.
useState :: WidgetModel i => Maybe WidgetState -> Maybe i
useState Nothing = Nothing
useState (Just (WidgetState state)) = cast state

-- | Casts the wrapped value in WidgetShared to the expected type, if possible.
useShared :: Typeable i => Maybe WidgetShared -> Maybe i
useShared Nothing = Nothing
useShared (Just (WidgetShared shared)) = cast shared

-- | Checks if the type and key of two WidgetNodeInfo match.
infoMatches :: WidgetNodeInfo -> WidgetNodeInfo -> Bool
infoMatches oldInfo newInfo = typeMatches && keyMatches where
  typeMatches = oldInfo ^. L.widgetType == newInfo ^. L.widgetType
  keyMatches = oldInfo ^. L.key == newInfo ^. L.key

-- | Checks if the type and key of two WidgetNodes match.
nodeMatches :: WidgetNode s e -> WidgetNode s e -> Bool
nodeMatches oldNode newNode = infoMatches oldInfo newInfo where
  oldInfo = oldNode ^. L.info
  newInfo = newNode ^. L.info

{-|
Checks if the path the node in the provided result changed compared to the old
node. In case it did, it appends a SetWidgetPath request to keep track of the
new location.
-}
handleWidgetIdChange :: WidgetNode s e -> WidgetResult s e -> WidgetResult s e
handleWidgetIdChange oldNode result = newResult where
  oldPath = oldNode ^. L.info . L.path
  newPath = result ^. L.node . L.info . L.path
  widgetId = result ^. L.node . L.info . L.widgetId
  newResult
    | oldPath /= newPath = result
        & L.requests %~ (SetWidgetPath widgetId newPath <|)
    | otherwise = result

-- | Returns the WidgetId associated to the given path, if any.
findWidgetIdFromPath :: WidgetEnv s e -> Path -> Maybe WidgetId
findWidgetIdFromPath wenv path = mwni ^? _Just . L.widgetId where
  mwni = wenv ^. L.findByPath $ path

-- | Sends a message to the given node with a delay of n ms.
delayedMessage :: Typeable i => WidgetNode s e -> i -> Int -> WidgetRequest s e
delayedMessage node msg delay = delayedMessage_ widgetId path msg delay where
  widgetId = node ^. L.info . L.widgetId
  path = node ^. L.info . L.path

-- | Sends a message to the given WidgetId with a delay of n ms.
delayedMessage_
  :: Typeable i => WidgetId -> Path -> i -> Int -> WidgetRequest s e
delayedMessage_ widgetId path msg delay = RunTask widgetId path $ do
  threadDelay (delay * 1000)
  return msg
