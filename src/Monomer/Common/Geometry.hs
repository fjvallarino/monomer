{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Common.Geometry where

import Control.Lens.TH (makeLenses)
import Data.Default

data Point = Point {
  _x :: !Double,
  _y :: !Double
} deriving (Show, Eq)

instance Default Point where
  def = Point 0 0

data Size = Size {
  _w :: !Double,
  _h :: !Double
} deriving (Show, Eq)

instance Default Size where
  def = Size 0 0

data Rect = Rect {
  _rx :: !Double,
  _ry :: !Double,
  _rw :: !Double,
  _rh :: !Double
} deriving (Show, Eq)

instance Default Rect where
  def = Rect 0 0 0 0

makeLenses ''Point
makeLenses ''Size
makeLenses ''Rect

inRect :: Rect -> Point -> Bool
inRect (Rect x y w h) (Point x2 y2) = (x2 >= x && x2 < x + w) && (y2 >= y && y2 < y + h)

midPoint :: Point -> Point -> Point
midPoint (Point x1 y1) (Point x2 y2) = Point x3 y3 where
  x3 = (x2 + x1) / 2
  y3 = (y2 + y1) / 2
