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

import Data.Sequence (Seq, (|>))

import qualified Data.Sequence as Seq

import Monomer.Core

parentPath :: WidgetInstance s e -> Path
parentPath inst = Seq.take (Seq.length path - 1) path where
  path = _wiPath inst

nextTargetStep :: Path -> WidgetInstance s e -> Maybe PathStep
nextTargetStep target inst = nextStep where
  currentPath = _wiPath inst
  nextStep = Seq.lookup (Seq.length currentPath) target

isFocusCandidate :: FocusDirection -> Path -> WidgetInstance s e -> Bool
isFocusCandidate FocusFwd = isFocusFwdCandidate
isFocusCandidate FocusBwd = isFocusBwdCandidate

isFocusFwdCandidate :: Path -> WidgetInstance s e -> Bool
isFocusFwdCandidate startFrom inst = isValid where
  isAfter = isWidgetAfterPath startFrom inst
  isFocusable = _wiFocusable inst
  isEnabled = _wiVisible inst && _wiEnabled inst
  isValid = isAfter && isFocusable && isEnabled

isFocusBwdCandidate :: Path -> WidgetInstance s e -> Bool
isFocusBwdCandidate startFrom inst = isValid where
  isBefore = isWidgetBeforePath startFrom inst
  isFocusable = _wiFocusable inst
  isEnabled = _wiVisible inst && _wiEnabled inst
  isValid = isBefore && isFocusable && isEnabled

isTargetReached :: Path -> WidgetInstance s e -> Bool
isTargetReached target inst = target == _wiPath inst

isTargetValid :: Path -> WidgetInstance s e -> Bool
isTargetValid target inst = valid where
  children = _wiChildren inst
  valid = case nextTargetStep target inst of
    Just step -> step < Seq.length children
    Nothing -> False

isWidgetParentOfPath :: Path -> WidgetInstance s e -> Bool
isWidgetParentOfPath path inst = result where
  widgetPath = _wiPath inst
  lenWidgetPath = Seq.length widgetPath
  pathPrefix = Seq.take lenWidgetPath path
  result = widgetPath == pathPrefix

isWidgetAfterPath :: Path -> WidgetInstance s e -> Bool
isWidgetAfterPath path inst = result where
  widgetPath = _wiPath inst
  lenPath = Seq.length path
  lenWidgetPath = Seq.length widgetPath
  widgetPathPrefix = Seq.take lenPath widgetPath
  result
    | lenWidgetPath > lenPath = path <= widgetPathPrefix
    | otherwise = path < widgetPath

isWidgetBeforePath :: Path -> WidgetInstance s e -> Bool
isWidgetBeforePath path inst = result where
  widgetPath = _wiPath inst
  result
    | path == rootPath = True
    | otherwise = path > widgetPath
