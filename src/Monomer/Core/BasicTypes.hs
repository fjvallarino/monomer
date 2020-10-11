module Monomer.Core.BasicTypes where

import Data.Default
import Data.Sequence (Seq)

import qualified Data.Sequence as Seq

type PathStep = Int
type Path = Seq PathStep
type Factor = Double

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

rootPath :: Path
rootPath = Seq.empty

pointInRect :: Point -> Rect -> Bool
pointInRect (Point px py) rect = coordInRectH px rect && coordInRectY py rect

addPoint :: Point -> Point -> Point
addPoint (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

coordInRectH :: Double -> Rect -> Bool
coordInRectH px (Rect x y w h) = px >= x && px < x + w

coordInRectY :: Double -> Rect -> Bool
coordInRectY py (Rect x y w h) = py >= y && py < y + h

addToSize :: Size -> Double -> Double -> Size
addToSize (Size w h) w2 h2 = Size nw nh where
  nw = max 0 $ w + w2
  nh = max 0 $ h + h2

subtractFromSize :: Size -> Double -> Double -> Size
subtractFromSize (Size w h) w2 h2 = Size nw nh where
  nw = max 0 $ w - w2
  nh = max 0 $ h - h2

rectInRect :: Rect -> Rect -> Bool
rectInRect inner outer = rectInRectH inner outer && rectInRectV inner outer

rectInRectH :: Rect -> Rect -> Bool
rectInRectH (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
  x1 >= x2 && x1 + w1 <= x2 + w2

rectInRectV :: Rect -> Rect -> Bool
rectInRectV (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
  y1 >= y2 && y1 + h1 <= y2 + h2

rectsOverlap :: Rect -> Rect -> Bool
rectsOverlap (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) = overlapX && overlapY where
  overlapX = x1 <= x2 + w2 && x1 + w1 >= x2
  overlapY = y1 <= y2 + h2 && y1 + h1 >= y2

addToRect :: Rect -> Double -> Double -> Double -> Double -> Rect
addToRect (Rect x y w h) l r t b = Rect nx ny nw nh where
  nx = x - l
  ny = y - t
  nw = max 0 $ w + l + r
  nh = max 0 $ h + t + b

subtractFromRect :: Rect -> Double -> Double -> Double -> Double -> Rect
subtractFromRect (Rect x y w h) l r t b = Rect nx ny nw nh where
  nx = x + l
  ny = y + t
  nw = max 0 $ w - l - r
  nh = max 0 $ h - t - b
