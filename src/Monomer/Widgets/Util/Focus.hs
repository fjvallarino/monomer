{-|
Module      : Monomer.Widgets.Util.Focus
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions for focus handling.
-}
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
import Data.Typeable (Typeable)

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Helper
import Monomer.Widgets.Util.Widget

import qualified Monomer.Core.Lens as L

-- | Checks if the given node is focused
isNodeFocused :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeFocused wenv node = wenv ^. L.focusedPath == node ^. L.info . L.path

-- | Checks if the given node is a parent of the focused node
isNodeParentOfFocused :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeParentOfFocused wenv node = seqStartsWith parentPath focusedPath where
  parentPath = node ^. L.info . L.path
  focusedPath = wenv ^. L.focusedPath

-- | Returns the parent path of a node
parentPath :: WidgetNode s e -> Path
parentPath node = Seq.take (Seq.length path - 1) path where
  path = node ^. L.info . L.path

-- | Returns the index of the child matching the next step implied by target.
nextTargetStep :: Path -> WidgetNode s e -> Maybe PathStep
nextTargetStep target node = nextStep where
  currentPath = node ^. L.info . L.path
  nextStep = Seq.lookup (Seq.length currentPath) target

{-|
Checks if the node is a candidate for next focus in the given direction. The
node must be focusable, enabled and visible, plus having the correct position
considering the direction.
-}
isFocusCandidate :: FocusDirection -> Path -> WidgetNode s e -> Bool
isFocusCandidate FocusFwd = isFocusFwdCandidate
isFocusCandidate FocusBwd = isFocusBwdCandidate

-- | Checks if the node's path matches the target.
isTargetReached :: Path -> WidgetNode s e -> Bool
isTargetReached target node = target == node ^. L.info . L.path

-- | Checks if the node has a child matching the next target step.
isTargetValid :: Path -> WidgetNode s e -> Bool
isTargetValid target node = valid where
  children = node ^. L.children
  valid = case nextTargetStep target node of
    Just step -> step < Seq.length children
    Nothing -> False

-- | Checks if the node is parent of the provided path.
isNodeParentOfPath :: Path -> WidgetNode s e -> Bool
isNodeParentOfPath path node = result where
  widgetPath = node ^. L.info . L.path
  lenWidgetPath = Seq.length widgetPath
  pathPrefix = Seq.take lenWidgetPath path
  result = widgetPath == pathPrefix

-- | Checks if the node's path is after the target (deeper or to the right).
isNodeAfterPath :: Path -> WidgetNode s e -> Bool
isNodeAfterPath path node = result where
  widgetPath = node ^. L.info . L.path
  lenPath = Seq.length path
  lenWidgetPath = Seq.length widgetPath
  widgetPathPrefix = Seq.take lenPath widgetPath
  result
    | lenWidgetPath > lenPath = path <= widgetPathPrefix
    | otherwise = path < widgetPath

-- | Checks if the node's path is after the target (higher or to the left).
isNodeBeforePath :: Path -> WidgetNode s e -> Bool
isNodeBeforePath path node = result where
  widgetPath = node ^. L.info . L.path
  result
    | path == emptyPath = True
    | otherwise = path > widgetPath

-- | Generates a result with events and requests associated to a focus change.
handleFocusChange
  :: [Path -> WidgetRequest s e] -- ^ Getter for reqs handler in a config type.
  -> Path                        -- ^ The path of next/prev target, accordingly.
  -> WidgetNode s e              -- ^ The node receiving the event.
  -> Maybe (WidgetResult s e)    -- ^ The result.
handleFocusChange reqFns path node = result where
  reqs = ($ path) <$> reqFns
  result
    | not (null reqs) = Just $ resultReqs node reqs
    | otherwise = Nothing

-- Helpers
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
