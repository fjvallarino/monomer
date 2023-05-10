{-|
Module      : Monomer.Widgets.Util.Focus
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions for focus handling.
-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Util.Focus (
  isNodeFocused,
  isNodeInfoFocused,
  isNodeParentOfFocused,
  parentPath,
  isPathParent,
  nextTargetStep,
  isFocusCandidate,
  isTargetReached,
  isTargetValid,
  isNodeParentOfPath,
  isNodeBeforePath,
  isNodeAfterPath,
  handleFocusChange
) where

import Control.Lens ((^.))

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Helper
import Monomer.Widgets.Util.Widget

import qualified Monomer.Core.Lens as L

-- | Checks if the given node is focused.
isNodeFocused :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeFocused wenv node = wenv ^. L.focusedPath == node ^. L.info . L.path

-- | Checks if the given nodeInfo is focused.
isNodeInfoFocused :: WidgetEnv s e -> WidgetNodeInfo -> Bool
isNodeInfoFocused wenv info = wenv ^. L.focusedPath == info ^. L.path

-- | Checks if the given node is a parent of the focused node.
isNodeParentOfFocused :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeParentOfFocused wenv node = seqStartsWith parentPath focusedPath where
  parentPath = node ^. L.info . L.path
  focusedPath = wenv ^. L.focusedPath

-- | Returns the parent path of a node.
parentPath :: WidgetNode s e -> Path
parentPath node = Seq.take (Seq.length path - 1) path where
  path = node ^. L.info . L.path

-- | Returns whether the first path is parent of the second path.
isPathParent :: Path -> Path -> Bool
isPathParent path childPath = path == basePath where
  basePath = Seq.take (Seq.length path) childPath

-- | Returns the index of the child matching the next step implied by target.
nextTargetStep :: WidgetNode s e -> Path -> Maybe PathStep
nextTargetStep node target = nextStep where
  currentPath = node ^. L.info . L.path
  nextStep = Seq.lookup (Seq.length currentPath) target

{-|
Checks if the node is a candidate for next focus in the given direction. The
node must be focusable, enabled and visible, plus having the correct position
considering the direction.
-}
isFocusCandidate :: WidgetNode s e -> Path -> FocusDirection -> Bool
isFocusCandidate node path FocusFwd = isFocusFwdCandidate node path
isFocusCandidate node path FocusBwd = isFocusBwdCandidate node path

-- | Checks if the node's path matches the target.
isTargetReached :: WidgetNode s e -> Path -> Bool
isTargetReached node target = target == node ^. L.info . L.path

-- | Checks if the node has a child matching the next target step.
isTargetValid :: WidgetNode s e -> Path -> Bool
isTargetValid node target = valid where
  children = node ^. L.children
  valid = case nextTargetStep node target of
    Just step -> step < Seq.length children
    Nothing -> False

-- | Checks if the node is parent of the provided path.
isNodeParentOfPath :: WidgetNode s e -> Path -> Bool
isNodeParentOfPath node path = result where
  widgetPath = node ^. L.info . L.path
  lenWidgetPath = Seq.length widgetPath
  pathPrefix = Seq.take lenWidgetPath path
  result = widgetPath == pathPrefix

-- | Checks if the node's path is after the target (deeper or to the right).
isNodeAfterPath :: WidgetNode s e -> Path -> Bool
isNodeAfterPath node path = result where
  widgetPath = node ^. L.info . L.path
  lenPath = Seq.length path
  lenWidgetPath = Seq.length widgetPath
  widgetPathPrefix = Seq.take lenPath widgetPath
  result
    | lenWidgetPath > lenPath = path <= widgetPathPrefix
    | otherwise = path < widgetPath

-- | Checks if the node's path is after the target (higher or to the left).
isNodeBeforePath :: WidgetNode s e -> Path -> Bool
isNodeBeforePath node path = result where
  widgetPath = node ^. L.info . L.path
  result
    | path == emptyPath = True
    | otherwise = path > widgetPath

-- | Generates a result with events and requests associated to a focus change.
handleFocusChange
  :: WidgetNode s e              -- ^ The node receiving the event.
  -> Path                        -- ^ The path of next/prev target, accordingly.
  -> [Path -> WidgetRequest s e] -- ^ Getter for reqs handler in a config type.
  -> Maybe (WidgetResult s e)    -- ^ The result.
handleFocusChange node path reqFns = result where
  reqs = ($ path) <$> reqFns
  result
    | not (null reqs) = Just $ resultReqs node reqs
    | otherwise = Nothing

-- Helpers
isFocusFwdCandidate :: WidgetNode s e -> Path -> Bool
isFocusFwdCandidate node startFrom = isValid where
  info = node ^. L.info
  isAfter = isNodeAfterPath node startFrom
  isFocusable = info ^. L.focusable
  isEnabled = info ^. L.visible && info ^. L.enabled
  isValid = isAfter && isFocusable && isEnabled

isFocusBwdCandidate :: WidgetNode s e -> Path -> Bool
isFocusBwdCandidate node startFrom = isValid where
  info = node ^. L.info
  isBefore = isNodeBeforePath node startFrom
  isFocusable = info ^. L.focusable
  isEnabled = info ^. L.visible && info ^. L.enabled
  isValid = isBefore && isFocusable && isEnabled
