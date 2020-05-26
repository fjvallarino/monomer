{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.PathContext where

import Data.Sequence (Seq, (<|), (|>), (><))

import qualified Data.Sequence as Seq

import Monomer.Common.Tree

data PathContext = PathContext {
  _pathFocused :: Path,
  _pathTarget :: Path,
  _pathCurrent :: Path
}

rootPath :: Path
rootPath = Seq.empty

nextTargetStep :: PathContext -> Maybe PathStep
nextTargetStep PathContext{..} = Seq.lookup (Seq.length _pathCurrent) _pathTarget

moveToTarget :: PathContext -> Maybe PathContext
moveToTarget ctx = fmap (addToCurrent ctx) (nextTargetStep ctx)

addToCurrent :: PathContext -> PathStep -> PathContext
addToCurrent ctx step = ctx { _pathCurrent = _pathCurrent ctx |> step }

isTargetReached :: PathContext -> Bool
isTargetReached ctx = _pathTarget ctx == _pathCurrent ctx

isTargetValid :: PathContext -> Seq.Seq a -> Bool
isTargetValid ctx children = case nextTargetStep ctx of
  Just step -> step < Seq.length children
  Nothing -> False

isTargetBeforeCurrent :: PathContext -> Bool
isTargetBeforeCurrent ctx@PathContext{..} = targetPrefix < _pathCurrent where
  lenTarget = Seq.length _pathTarget
  lenCurrent = Seq.length _pathCurrent
  targetPrefix = if lenTarget > lenCurrent
    then Seq.take lenCurrent _pathTarget
    else _pathTarget
