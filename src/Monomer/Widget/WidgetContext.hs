{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.WidgetContext (
  module Monomer.Common.Tree,
  module Monomer.Widget.WidgetContext
) where

import Data.Sequence (Seq, (|>))

import qualified Data.Sequence as Seq

import Monomer.Common.Tree (Path, PathStep)

data WidgetContext = WidgetContext {
  _wcVisible :: Bool,
  _wcEnabled :: Bool,
  _wcFocusedPath :: Path,
  _wcTargetPath :: Path,
  _wcCurrentPath :: Path
} deriving (Show, Eq)

rootPath :: Path
rootPath = Seq.empty

childContext :: WidgetContext -> WidgetContext
childContext ctx = addToCurrent ctx 0

nextTargetStep :: WidgetContext -> Maybe PathStep
nextTargetStep WidgetContext{..} = Seq.lookup (Seq.length _wcCurrentPath) _wcTargetPath

moveToTarget :: WidgetContext -> Maybe WidgetContext
moveToTarget ctx = fmap (addToCurrent ctx) (nextTargetStep ctx)

addToCurrent :: WidgetContext -> PathStep -> WidgetContext
addToCurrent ctx step = ctx { _wcCurrentPath = _wcCurrentPath ctx |> step }

isFocused :: WidgetContext -> Bool
isFocused ctx = _wcFocusedPath ctx == _wcCurrentPath ctx

isTargetReached :: WidgetContext -> Bool
isTargetReached ctx = _wcTargetPath ctx == _wcCurrentPath ctx

isTargetValid :: WidgetContext -> Seq a -> Bool
isTargetValid ctx children = case nextTargetStep ctx of
  Just step -> step < Seq.length children
  Nothing -> False

isTargetBeforeCurrent :: WidgetContext -> Bool
isTargetBeforeCurrent ctx@WidgetContext{..} = targetPrefix <= _wcCurrentPath where
  lenTarget = Seq.length _wcTargetPath
  lenCurrent = Seq.length _wcCurrentPath
  targetPrefix = if lenTarget > lenCurrent
    then Seq.take lenCurrent _wcTargetPath
    else _wcTargetPath
