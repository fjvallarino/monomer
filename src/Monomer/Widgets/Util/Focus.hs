module Monomer.Widgets.Util.Focus (
  parentPath,
  nextTargetStep,
  isFocusCandidate,
  isTargetReached,
  isTargetValid,
  isWidgetParentOfPath,
  isWidgetBeforePath,
  isWidgetAfterPath
) where

import Control.Lens ((&), (^.), (.~))
import Data.Sequence (Seq, (|>))

import qualified Data.Sequence as Seq

import Monomer.Core

import qualified Monomer.Lens as L

parentPath :: WidgetNode s e -> Path
parentPath node = Seq.take (Seq.length path - 1) path where
  path = node ^. L.widgetInstance . L.path

nextTargetStep :: Path -> WidgetNode s e -> Maybe PathStep
nextTargetStep target node = nextStep where
  currentPath = node ^. L.widgetInstance . L.path
  nextStep = Seq.lookup (Seq.length currentPath) target

isFocusCandidate :: FocusDirection -> Path -> WidgetNode s e -> Bool
isFocusCandidate FocusFwd = isFocusFwdCandidate
isFocusCandidate FocusBwd = isFocusBwdCandidate

isFocusFwdCandidate :: Path -> WidgetNode s e -> Bool
isFocusFwdCandidate startFrom node = isValid where
  inst = node ^. L.widgetInstance
  isAfter = isWidgetAfterPath startFrom node
  isFocusable = _wiFocusable inst
  isEnabled = _wiVisible inst && _wiEnabled inst
  isValid = isAfter && isFocusable && isEnabled

isFocusBwdCandidate :: Path -> WidgetNode s e -> Bool
isFocusBwdCandidate startFrom node = isValid where
  inst = node ^. L.widgetInstance
  isBefore = isWidgetBeforePath startFrom node
  isFocusable = _wiFocusable inst
  isEnabled = _wiVisible inst && _wiEnabled inst
  isValid = isBefore && isFocusable && isEnabled

isTargetReached :: Path -> WidgetNode s e -> Bool
isTargetReached target node = target == node ^. L.widgetInstance . L.path

isTargetValid :: Path -> WidgetNode s e -> Bool
isTargetValid target node = valid where
  children = node ^. L.children
  valid = case nextTargetStep target node of
    Just step -> step < Seq.length children
    Nothing -> False

isWidgetParentOfPath :: Path -> WidgetNode s e -> Bool
isWidgetParentOfPath path node = result where
  widgetPath = node ^. L.widgetInstance . L.path
  lenWidgetPath = Seq.length widgetPath
  pathPrefix = Seq.take lenWidgetPath path
  result = widgetPath == pathPrefix

isWidgetAfterPath :: Path -> WidgetNode s e -> Bool
isWidgetAfterPath path node = result where
  widgetPath = node ^. L.widgetInstance . L.path
  lenPath = Seq.length path
  lenWidgetPath = Seq.length widgetPath
  widgetPathPrefix = Seq.take lenPath widgetPath
  result
    | lenWidgetPath > lenPath = path <= widgetPathPrefix
    | otherwise = path < widgetPath

isWidgetBeforePath :: Path -> WidgetNode s e -> Bool
isWidgetBeforePath path node = result where
  widgetPath = node ^. L.widgetInstance . L.path
  result
    | path == rootPath = True
    | otherwise = path > widgetPath
