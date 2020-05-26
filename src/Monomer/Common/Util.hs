{-# LANGUAGE BangPatterns #-}

module Monomer.Common.Util where

import Data.Sequence (Seq(..), (<|), (|>), (><))

import qualified Data.List as L
import qualified Data.Sequence as Seq

import Monomer.Common.Types

inRect :: Rect -> Point -> Bool
inRect (Rect x y w h) (Point x2 y2) = (x2 >= x && x2 < x + w) && (y2 >= y && y2 < y + h)

midPoint :: Point -> Point -> Point
midPoint (Point x1 y1) (Point x2 y2) = Point x3 y3 where
  x3 = (x2 + x1) / 2
  y3 = (y2 + y1) / 2

rotateSeq :: Seq a -> Seq a
rotateSeq Empty = Seq.empty
rotateSeq (x :<| xs) = xs |> x

inverseRotateSeq :: Seq a -> Seq a
inverseRotateSeq Empty = Seq.empty
inverseRotateSeq (xs :|> x) = x <| xs

rotateUntil :: (Eq a) => a -> Seq a -> Seq a
rotateUntil val list = case Seq.elemIndexL val list of
  Nothing -> list
  Just idx -> part2 >< part1 where
    (part1, part2) = Seq.splitAt idx list

bindIf :: (Monad m) => Bool -> (a -> m a) -> a -> m a
bindIf False _ value = return value
bindIf _ action value = action value

compose :: (Traversable t) => t (a -> a) -> a -> a
compose functions init = foldr (.) id functions init
