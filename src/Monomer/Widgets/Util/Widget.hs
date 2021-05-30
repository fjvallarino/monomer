{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  matchFailedMsg,
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

import Monomer.Core
import Monomer.Event.Types
import Monomer.Event.Util
import Monomer.Graphics.Types

import qualified Monomer.Lens as L

defaultWidgetNode :: WidgetType -> Widget s e -> WidgetNode s e
defaultWidgetNode widgetType widget = WidgetNode {
  _wnWidget = widget,
  _wnInfo = def & L.widgetType .~ widgetType,
  _wnChildren = Seq.empty
}

isWidgetVisible :: WidgetEnv s e -> WidgetNode s e -> Bool
isWidgetVisible wenv node = isVisible && isOverlapped where
  info = node ^. L.info
  isVisible = info ^. L.visible
  viewport = wenv ^. L.viewport
  isOverlapped = rectsOverlap viewport (info ^. L.viewport)

nodeVisibleChanged :: WidgetNode s e -> WidgetNode s e -> Bool
nodeVisibleChanged oldNode newNode = oldVisible /= newVisible where
  oldVisible = oldNode ^. L.info . L.visible
  newVisible = newNode ^. L.info . L.visible

nodeEnabledChanged :: WidgetNode s e -> WidgetNode s e -> Bool
nodeEnabledChanged oldNode newNode = oldEnabled /= newEnabled where
  oldEnabled = oldNode ^. L.info . L.enabled
  newEnabled = newNode ^. L.info . L.enabled

nodeFlagsChanged :: WidgetNode s e -> WidgetNode s e -> Bool
nodeFlagsChanged oldNode newNode = visibleChanged || enabledChanged where
  visibleChanged = nodeVisibleChanged oldNode newNode
  enabledChanged = nodeEnabledChanged oldNode newNode

childrenVisibleChanged :: WidgetNode s e -> WidgetNode s e -> Bool
childrenVisibleChanged oldNode newNode = oldVisible /= newVisible where
  oldVisible = fmap (^. L.info . L.visible) (oldNode ^. L.children)
  newVisible = fmap (^. L.info . L.visible) (newNode ^. L.children)

childrenEnabledChanged :: WidgetNode s e -> WidgetNode s e -> Bool
childrenEnabledChanged oldNode newNode = oldVisible /= newVisible where
  oldVisible = fmap (^. L.info . L.enabled) (oldNode ^. L.children)
  newVisible = fmap (^. L.info . L.enabled) (newNode ^. L.children)

childrenFlagsChanged :: WidgetNode s e -> WidgetNode s e -> Bool
childrenFlagsChanged oldNode newNode = lenChanged || flagsChanged where
  oldChildren = oldNode ^. L.children
  newChildren = newNode ^. L.children
  flagsChanged = or (Seq.zipWith nodeFlagsChanged oldChildren newChildren)
  lenChanged = length oldChildren /= length newChildren

widgetDataGet :: s -> WidgetData s a -> a
widgetDataGet _ (WidgetValue value) = value
widgetDataGet model (WidgetLens lens) = model ^# lens

widgetDataSet :: WidgetData s a -> a -> [WidgetRequest s e]
widgetDataSet WidgetValue{} _ = []
widgetDataSet (WidgetLens lens) value = [UpdateModel updateFn] where
  updateFn model = model & lens #~ value

resultNode :: WidgetNode s e -> WidgetResult s e
resultNode node = WidgetResult node Seq.empty

resultEvts :: Typeable e => WidgetNode s e -> [e] -> WidgetResult s e
resultEvts node events = result where
  result = WidgetResult node (Seq.fromList $ RaiseEvent <$> events)

resultReqs :: WidgetNode s e -> [WidgetRequest s e] -> WidgetResult s e
resultReqs node requests = result where
  result = WidgetResult node (Seq.fromList requests)

resultReqsEvts
  :: Typeable e => WidgetNode s e -> [WidgetRequest s e] -> [e] -> WidgetResult s e
resultReqsEvts node requests events = result where
  result = WidgetResult node (Seq.fromList requests <> evtSeq)
  evtSeq = Seq.fromList $ RaiseEvent <$> events

makeState
  :: WidgetModel i => i -> WidgetEnv s e -> WidgetNode s e -> Maybe WidgetState
makeState state wenv node = Just (WidgetState state)

useState :: WidgetModel i => Maybe WidgetState -> Maybe i
useState Nothing = Nothing
useState (Just (WidgetState state)) = cast state

matchFailedMsg :: WidgetNodeInfo -> WidgetNodeInfo -> String
matchFailedMsg oldInfo newInfo = message where
  oldData = (oldInfo ^. L.widgetType, oldInfo ^. L.key)
  newData = (newInfo ^. L.widgetType, newInfo ^. L.key)
  message = "Nodes do not match: " ++ show oldData ++ " - " ++ show newData

infoMatches :: WidgetNodeInfo -> WidgetNodeInfo -> Bool
infoMatches oldInfo newInfo = typeMatches && keyMatches where
  typeMatches = oldInfo ^. L.widgetType == newInfo ^. L.widgetType
  keyMatches = oldInfo ^. L.key == newInfo ^. L.key

nodeMatches :: WidgetNode s e -> WidgetNode s e -> Bool
nodeMatches oldNode newNode = infoMatches oldInfo newInfo where
  oldInfo = oldNode ^. L.info
  newInfo = newNode ^. L.info

handleWidgetIdChange :: WidgetNode s e -> WidgetResult s e -> WidgetResult s e
handleWidgetIdChange oldNode result = newResult where
  oldPath = oldNode ^. L.info . L.path
  newPath = result ^. L.node . L.info . L.path
  widgetId = result ^. L.node . L.info . L.widgetId
  newResult
    | oldPath /= newPath = result
        & L.requests %~ (SetWidgetPath widgetId newPath <|)
    | otherwise = result

findWidgetIdFromPath :: WidgetEnv s e -> Path -> Maybe WidgetId
findWidgetIdFromPath wenv path = mwni ^? _Just . L.widgetId where
  mwni = wenv ^. L.findByPath $ path

delayedMessage :: Typeable i => WidgetNode s e -> i -> Int -> WidgetRequest s e
delayedMessage node msg delay = delayedMessage_ widgetId path msg delay where
  widgetId = node ^. L.info . L.widgetId
  path = node ^. L.info . L.path

delayedMessage_ :: Typeable i => WidgetId -> Path -> i -> Int -> WidgetRequest s e
delayedMessage_ widgetId path msg delay = RunTask widgetId path $ do
  threadDelay (delay * 1000)
  return msg
