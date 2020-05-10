{-# LANGUAGE BangPatterns #-}

module Monomer.Common.Util where

import Data.Default
import Data.List (elemIndex, splitAt)

import Monomer.Common.Types

inRect :: Rect -> Point -> Bool
inRect (Rect x y w h) (Point x2 y2) = (x2 >= x && x2 < x + w) && (y2 >= y && y2 < y + h)

rotateList :: [a] -> [a]
rotateList [] = []
rotateList (x:xs) = xs ++ [x]

inverseRotateList :: [a] -> [a]
inverseRotateList [] = []
inverseRotateList xs = last xs : init xs

rotateUntil :: (Eq a) => a -> [a] -> [a]
rotateUntil val list = case elemIndex val list of
  Nothing -> list
  Just idx -> part2 ++ part1 where
    (part1, part2) = splitAt idx list

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just val) _ = Just val
firstJust _ value = value

justDef :: (Default a) => Maybe a -> a
justDef Nothing = def
justDef (Just val) = val

midPoint :: Point -> Point -> Point
midPoint (Point x1 y1) (Point x2 y2) = Point x3 y3 where
  x3 = (x2 + x1) / 2
  y3 = (y2 + y1) / 2

moveRect :: Rect -> Double -> Double -> Rect
moveRect (Rect x y w h) dx dy = Rect (x + dx) (y + dy) w h

bindIf :: (Monad m) => Bool -> (a -> m a) -> a -> m a
bindIf False _ value = return value
bindIf _ action value = action value

compose :: [a -> a] -> a -> a
compose functions init = foldr (.) id functions init
