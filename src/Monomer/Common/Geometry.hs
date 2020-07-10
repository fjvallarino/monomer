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

pointInRect :: Point -> Rect -> Bool
pointInRect (Point px py) (Rect x y w h) = (px >= x && px < x + w) && (py >= y && py < y + h)

rectInRect :: Rect -> Rect -> Bool
rectInRect rect container = rectInRectH rect container && rectInRectV rect container

rectInRectH :: Rect -> Rect -> Bool
rectInRectH (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) = x1 >= x2 && x1 + w1 <= x2 + w2

rectInRectV :: Rect -> Rect -> Bool
rectInRectV (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) = y1 >= y2 && y1 + h1 <= y2 + h2

midPoint :: Point -> Point -> Point
midPoint (Point x1 y1) (Point x2 y2) = Point x3 y3 where
  x3 = (x2 + x1) / 2
  y3 = (y2 + y1) / 2
