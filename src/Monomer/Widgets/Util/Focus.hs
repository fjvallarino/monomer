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
  isBefore = isTargetBeforeWidget startFrom widgetInst
  isFocusable = _wiFocusable widgetInst
  isEnabled = _wiVisible widgetInst && _wiEnabled widgetInst
  isValid = isBefore && isFocusable && isEnabled

isFocusBwdCandidate :: Path -> WidgetInstance s e -> Bool
isFocusBwdCandidate startFrom widgetInst = isValid where
  isAfter = isTargetAfterWidget startFrom widgetInst
  isFocusable = _wiFocusable widgetInst
  isEnabled = _wiVisible widgetInst && _wiEnabled widgetInst
  isValid = isAfter && isFocusable && isEnabled

isTargetReached :: Path -> WidgetInstance s e -> Bool
isTargetReached target widgetInst = target == _wiPath widgetInst

isTargetValid :: Path -> WidgetInstance s e -> Bool
isTargetValid target widgetInst = valid where
  children = _wiChildren widgetInst
  valid = case nextTargetStep target widgetInst of
    Just step -> step < Seq.length children
    Nothing -> False

isTargetBeforeWidget :: Path -> WidgetInstance s e -> Bool
isTargetBeforeWidget target widgetInst = result where
  currentPath = _wiPath widgetInst
  lenTarget = Seq.length target
  lenCurrent = Seq.length currentPath
  targetPrefix = Seq.take lenCurrent target
  result
    | lenTarget > lenCurrent = targetPrefix <= currentPath
    | otherwise = target < currentPath

isTargetAfterWidget :: Path -> WidgetInstance s e -> Bool
isTargetAfterWidget target widgetInst
  | target == rootPath = True
  | otherwise = target > currentPath
  where
    currentPath = _wiPath widgetInst
