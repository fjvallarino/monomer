{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Common.Geometry where

import Control.Lens.TH (makeLenses)
import Data.Default

data Point = Point {
  _pX :: !Double,
  _pY :: !Double
} deriving (Show, Eq)

instance Default Point where
  def = Point 0 0

data Size = Size {
  _sW :: !Double,
  _sH :: !Double
} deriving (Show, Eq)

instance Default Size where
  def = Size 0 0

data Rect = Rect {
  _rX :: !Double,
  _rY :: !Double,
  _rW :: !Double,
  _rH :: !Double
} deriving (Show, Eq)

instance Default Rect where
  def = Rect 0 0 0 0

makeLenses ''Point
makeLenses ''Size
makeLenses ''Rect

pointInRect :: Point -> Rect -> Bool
pointInRect (Point px py) (Rect x y w h) = pointInH && pointInV where
  pointInH = px >= x && px < x + w
  pointInV = py >= y && py < y + h

rectInRect :: Rect -> Rect -> Bool
rectInRect inner outer = rectInRectH inner outer && rectInRectV inner outer

rectInRectH :: Rect -> Rect -> Bool
rectInRectH (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
  x1 >= x2 && x1 + w1 <= x2 + w2

rectInRectV :: Rect -> Rect -> Bool
rectInRectV (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
  y1 >= y2 && y1 + h1 <= y2 + h2
