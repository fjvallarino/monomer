{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.PathContext (
  module Monomer.Widget.PathContext,
  module Monomer.Common.Tree
) where

import Data.Sequence (Seq, (|>))

import qualified Data.Sequence as Seq

import Monomer.Common.Tree (Path, PathStep)

data PathContext = PathContext {
  _pathFocused :: Path,
  _pathTarget :: Path,
  _pathCurrent :: Path
} deriving (Show, Eq)

rootPath :: Path
rootPath = Seq.empty

nextTargetStep :: PathContext -> Maybe PathStep
nextTargetStep PathContext{..} = Seq.lookup (Seq.length _pathCurrent) _pathTarget

moveToTarget :: PathContext -> Maybe PathContext
moveToTarget ctx = fmap (addToCurrent ctx) (nextTargetStep ctx)

currentPath :: PathContext -> Path
currentPath ctx = _pathCurrent ctx

addToCurrent :: PathContext -> PathStep -> PathContext
addToCurrent ctx step = ctx { _pathCurrent = _pathCurrent ctx |> step }

isFocused :: PathContext -> Bool
isFocused ctx = _pathFocused ctx == _pathCurrent ctx

isTargetReached :: PathContext -> Bool
isTargetReached ctx = _pathTarget ctx == _pathCurrent ctx

isTargetValid :: PathContext -> Seq a -> Bool
isTargetValid ctx children = case nextTargetStep ctx of
  Just step -> step < Seq.length children
  Nothing -> False

isTargetBeforeCurrent :: PathContext -> Bool
isTargetBeforeCurrent ctx@PathContext{..} = targetPrefix <= _pathCurrent where
  lenTarget = Seq.length _pathTarget
  lenCurrent = Seq.length _pathCurrent
  targetPrefix = if lenTarget > lenCurrent
    then Seq.take lenCurrent _pathTarget
    else _pathTarget
