module Monomer.Widgets.Util.Focus where

import Data.Sequence (Seq, (|>))

import qualified Data.Sequence as Seq

import Monomer.Core

parentPath :: WidgetInstance s e -> Path
parentPath widgetInst = Seq.take (Seq.length path - 1) path where
  path = _wiPath widgetInst

firstChildPath :: WidgetInstance s e -> Path
firstChildPath widgetInst = _wiPath widgetInst |> 0

nextTargetStep :: Path -> WidgetInstance s e -> Maybe PathStep
nextTargetStep target widgetInst = nextStep where
  currentPath = _wiPath widgetInst
  nextStep = Seq.lookup (Seq.length currentPath) target

isFocusCandidate :: FocusDirection -> Path -> WidgetInstance s e -> Bool
isFocusCandidate FocusFwd = isFocusFwdCandidate
isFocusCandidate FocusBwd = isFocusBwdCandidate

isFocusFwdCandidate :: Path -> WidgetInstance s e -> Bool
isFocusFwdCandidate startFrom widgetInst = isValid where
  isAfter = isWidgetAfterPath startFrom widgetInst
  isFocusable = _wiFocusable widgetInst
  isEnabled = _wiVisible widgetInst && _wiEnabled widgetInst
  isValid = isAfter && isFocusable && isEnabled

isFocusBwdCandidate :: Path -> WidgetInstance s e -> Bool
isFocusBwdCandidate startFrom widgetInst = isValid where
  isBefore = isWidgetBeforePath startFrom widgetInst
  isFocusable = _wiFocusable widgetInst
  isEnabled = _wiVisible widgetInst && _wiEnabled widgetInst
  isValid = isBefore && isFocusable && isEnabled

isTargetReached :: Path -> WidgetInstance s e -> Bool
isTargetReached target widgetInst = target == _wiPath widgetInst

isTargetValid :: Path -> WidgetInstance s e -> Bool
isTargetValid target widgetInst = valid where
  children = _wiChildren widgetInst
  valid = case nextTargetStep target widgetInst of
    Just step -> step < Seq.length children
    Nothing -> False

isWidgetParentOfPath :: Path -> WidgetInstance s e -> Bool
isWidgetParentOfPath path widgetInst = result where
  widgetPath = _wiPath widgetInst
  lenWidgetPath = Seq.length widgetPath
  pathPrefix = Seq.take lenWidgetPath path
  result = widgetPath == pathPrefix

isWidgetAfterPath :: Path -> WidgetInstance s e -> Bool
isWidgetAfterPath path widgetInst = result where
  widgetPath = _wiPath widgetInst
  lenPath = Seq.length path
  lenWidgetPath = Seq.length widgetPath
  widgetPathPrefix = Seq.take lenPath widgetPath
  result
    | lenWidgetPath > lenPath = path <= widgetPathPrefix
    | otherwise = path < widgetPath

isWidgetBeforePath :: Path -> WidgetInstance s e -> Bool
isWidgetBeforePath path widgetInst = result where
  widgetPath = _wiPath widgetInst
  result
    | path == rootPath = True
    | otherwise = path > widgetPath
