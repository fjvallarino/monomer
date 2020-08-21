{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Common.Geometry where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)
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

makeLensesWith abbreviatedFields ''Point
makeLensesWith abbreviatedFields ''Size
makeLensesWith abbreviatedFields ''Rect

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

subtractFromRect :: Rect -> Double -> Double -> Double -> Double -> Rect
subtractFromRect (Rect x y w h) l r t b = Rect nx ny nw nh where
  nx = x + l
  ny = y + t
  nw = max 0 $ w - l - r
  nh = max 0 $ h - t - b
