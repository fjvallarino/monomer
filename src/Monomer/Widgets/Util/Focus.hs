module Monomer.Widgets.Util.Focus (
  isNodeFocused,
  isNodeParentOfFocused,
  parentPath,
  nextTargetStep,
  isFocusCandidate,
  isTargetReached,
  isTargetValid,
  isNodeParentOfPath,
  isNodeBeforePath,
  isNodeAfterPath,
  handleFocusChange
) where

import Control.Lens ((&), (^.), (.~), (%~))
import Data.Maybe
import Data.Sequence (Seq(..), (|>))

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event.Types
import Monomer.Widgets.Util.Hover
import Monomer.Widgets.Util.Widget

import qualified Monomer.Lens as L

isNodeFocused :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeFocused wenv node = wenv ^. L.focusedPath == node ^. L.info . L.path

isNodeParentOfFocused :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeParentOfFocused wenv node = seqStartsWith parentPath focusedPath where
  parentPath = node ^. L.info . L.path
  focusedPath = wenv ^. L.focusedPath

parentPath :: WidgetNode s e -> Path
parentPath node = Seq.take (Seq.length path - 1) path where
  path = node ^. L.info . L.path

nextTargetStep :: Path -> WidgetNode s e -> Maybe PathStep
nextTargetStep target node = nextStep where
  currentPath = node ^. L.info . L.path
  nextStep = Seq.lookup (Seq.length currentPath) target

isFocusCandidate :: FocusDirection -> Path -> WidgetNode s e -> Bool
isFocusCandidate FocusFwd = isFocusFwdCandidate
isFocusCandidate FocusBwd = isFocusBwdCandidate

isFocusFwdCandidate :: Path -> WidgetNode s e -> Bool
isFocusFwdCandidate startFrom node = isValid where
  info = node ^. L.info
  isAfter = isNodeAfterPath startFrom node
  isFocusable = info ^. L.focusable
  isEnabled = info ^. L.visible && info ^. L.enabled
  isValid = isAfter && isFocusable && isEnabled

isFocusBwdCandidate :: Path -> WidgetNode s e -> Bool
isFocusBwdCandidate startFrom node = isValid where
  info = node ^. L.info
  isBefore = isNodeBeforePath startFrom node
  isFocusable = info ^. L.focusable
  isEnabled = info ^. L.visible && info ^. L.enabled
  isValid = isBefore && isFocusable && isEnabled

isTargetReached :: Path -> WidgetNode s e -> Bool
isTargetReached target node = target == node ^. L.info . L.path

isTargetValid :: Path -> WidgetNode s e -> Bool
isTargetValid target node = valid where
  children = node ^. L.children
  valid = case nextTargetStep target node of
    Just step -> step < Seq.length children
    Nothing -> False

isNodeParentOfPath :: Path -> WidgetNode s e -> Bool
isNodeParentOfPath path node = result where
  widgetPath = node ^. L.info . L.path
  lenWidgetPath = Seq.length widgetPath
  pathPrefix = Seq.take lenWidgetPath path
  result = widgetPath == pathPrefix

isNodeAfterPath :: Path -> WidgetNode s e -> Bool
isNodeAfterPath path node = result where
  widgetPath = node ^. L.info . L.path
  lenPath = Seq.length path
  lenWidgetPath = Seq.length widgetPath
  widgetPathPrefix = Seq.take lenPath widgetPath
  result
    | lenWidgetPath > lenPath = path <= widgetPathPrefix
    | otherwise = path < widgetPath

isNodeBeforePath :: Path -> WidgetNode s e -> Bool
isNodeBeforePath path node = result where
  widgetPath = node ^. L.info . L.path
  result
    | path == emptyPath = True
    | otherwise = path > widgetPath

handleFocusChange
  :: (c -> [e])
  -> (c -> [WidgetRequest s])
  -> c
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)
handleFocusChange evtFn reqFn config node = result where
  evts = evtFn config
  reqs = reqFn config
  result
    | not (null evts && null reqs) = Just $ resultReqsEvts node reqs evts
    | otherwise = Nothing
