{-# LANGUAGE BangPatterns #-}

module Monomer.Common.Util where

import Monomer.Common.Types

inRect :: Rect -> Point -> Bool
inRect (Rect x y w h) (Point x2 y2) = (x2 >= x && x2 < x + w) && (y2 >= y && y2 < y + h)

midPoint :: Point -> Point -> Point
midPoint (Point x1 y1) (Point x2 y2) = Point x3 y3 where
  x3 = (x2 + x1) / 2
  y3 = (y2 + y1) / 2

bindIf :: (Monad m) => Bool -> (a -> m a) -> a -> m a
bindIf False _ value = return value
bindIf _ action value = action value

compose :: (Traversable t) => t (a -> a) -> a -> a
compose functions init = foldr (.) id functions init
