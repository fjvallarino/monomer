{-# LANGUAGE FlexibleContexts #-}

module Monomer.Widgets.Util.Widget (
  defaultWidgetNode,
  isWidgetVisible,
  visibleChildrenChanged,
  widgetDataGet,
  widgetDataSet,
  resultWidget,
  resultEvts,
  resultReqs,
  resultReqsEvts,
  makeState,
  useState,
  loadState,
  matchFailedMsg,
  infoMatches,
  nodeMatches,
  handleWidgetIdChange
) where

import Codec.Serialise
import Control.Lens ((&), (^#), (#~), (^.), (.~), (%~))
import Data.ByteString.Lazy (ByteString)
import Data.Default
import Data.Maybe
import Data.Map.Strict (Map)
import Data.Sequence (Seq(..), (<|))
import Data.Typeable (cast, Typeable)

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event (checkKeyboard, isKeyC, isKeyV)
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

isWidgetVisible :: WidgetNode s e -> Rect -> Bool
isWidgetVisible node vp = isVisible && isOverlapped where
  info = node ^. L.info
  isVisible = info ^. L.visible
  isOverlapped = rectsOverlap vp (info ^. L.viewport)

visibleChildrenChanged :: WidgetNode s e -> WidgetNode s e -> Bool
visibleChildrenChanged oldNode newNode = oldVisible /= newVisible  where
  oldVisible = fmap (^. L.info . L.visible) (oldNode ^. L.children)
  newVisible = fmap (^. L.info . L.visible) (newNode ^. L.children)

widgetDataGet :: s -> WidgetData s a -> a
widgetDataGet _ (WidgetValue value) = value
widgetDataGet model (WidgetLens lens) = model ^# lens

widgetDataSet :: WidgetData s a -> a -> [WidgetRequest s]
widgetDataSet WidgetValue{} _ = []
widgetDataSet (WidgetLens lens) value = [UpdateModel updateFn] where
  updateFn model = model & lens #~ value

resultWidget :: WidgetNode s e -> WidgetResult s e
resultWidget node = WidgetResult node Seq.empty Seq.empty

resultEvts :: WidgetNode s e -> [e] -> WidgetResult s e
resultEvts node events = result where
  result = WidgetResult node Seq.empty (Seq.fromList events)

resultReqs :: WidgetNode s e -> [WidgetRequest s] -> WidgetResult s e
resultReqs node requests = result where
  result = WidgetResult node (Seq.fromList requests) Seq.empty

resultReqsEvts :: WidgetNode s e -> [WidgetRequest s] -> [e] -> WidgetResult s e
resultReqsEvts node requests events = result where
  result = WidgetResult node (Seq.fromList requests) (Seq.fromList events)

makeState :: (Typeable i, Serialise i) => i -> s -> Maybe WidgetState
makeState state model = Just (WidgetState state)

useState :: Typeable i => Maybe WidgetState -> Maybe i
useState Nothing = Nothing
useState (Just (WidgetState state)) = cast state

loadState :: (Typeable i, Serialise i) => Maybe WidgetState -> Maybe i
loadState state = state >>= wsVal >>= fromBS where
  wsVal (WidgetState val) = cast val
  fromBS bs = case deserialiseOrFail bs of
    Left _ -> Nothing
    Right val -> Just val

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
        & L.requests %~ (UpdateWidgetPath widgetId newPath <|)
    | otherwise = result
