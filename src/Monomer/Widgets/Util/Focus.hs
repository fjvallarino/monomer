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
  isAfter = isWidgetAfterPath startFrom node
  isFocusable = info ^. L.focusable
  isEnabled = info ^. L.visible && info ^. L.enabled
  isValid = isAfter && isFocusable && isEnabled

isFocusBwdCandidate :: Path -> WidgetNode s e -> Bool
isFocusBwdCandidate startFrom node = isValid where
  info = node ^. L.info
  isBefore = isWidgetBeforePath startFrom node
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

isWidgetParentOfPath :: Path -> WidgetNode s e -> Bool
isWidgetParentOfPath path node = result where
  widgetPath = node ^. L.info . L.path
  lenWidgetPath = Seq.length widgetPath
  pathPrefix = Seq.take lenWidgetPath path
  result = widgetPath == pathPrefix

isWidgetAfterPath :: Path -> WidgetNode s e -> Bool
isWidgetAfterPath path node = result where
  widgetPath = node ^. L.info . L.path
  lenPath = Seq.length path
  lenWidgetPath = Seq.length widgetPath
  widgetPathPrefix = Seq.take lenPath widgetPath
  result
    | lenWidgetPath > lenPath = path <= widgetPathPrefix
    | otherwise = path < widgetPath

isWidgetBeforePath :: Path -> WidgetNode s e -> Bool
isWidgetBeforePath path node = result where
  widgetPath = node ^. L.info . L.path
  result
    | path == rootPath = True
    | otherwise = path > widgetPath
