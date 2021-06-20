{-|
Module      : Monomer.Common.BasicTypes
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Basic types used across the library.
-}
{-# LANGUAGE DeriveGeneric #-}

module Monomer.Common.BasicTypes where

import Data.Default
import Data.Sequence (Seq)
import GHC.Generics

import qualified Data.Sequence as Seq

-- | An index in the list of children of a widget.
type PathStep = Int
-- | A sequence of steps, usually from the root.
type Path = Seq PathStep
-- | Resize factor.
type Factor = Double

-- | Point in the 2D space.
data Point = Point {
  _pX :: {-# UNPACK #-} !Double,
  _pY :: {-# UNPACK #-} !Double
} deriving (Eq, Show, Generic)

instance Default Point where
  def = Point 0 0

-- | Width and height, used for size requirements.
data Size = Size {
  _sW :: {-# UNPACK #-} !Double,
  _sH :: {-# UNPACK #-} !Double
} deriving (Eq, Show, Generic)

instance Default Size where
  def = Size 0 0

-- | Rectangle, usually representing an area of the screen.
data Rect = Rect {
  _rX :: {-# UNPACK #-} !Double,
  _rY :: {-# UNPACK #-} !Double,
  _rW :: {-# UNPACK #-} !Double,
  _rH :: {-# UNPACK #-} !Double
} deriving (Eq, Show, Generic)

instance Default Rect where
  def = Rect 0 0 0 0

-- | An empty path.
emptyPath :: Path
emptyPath = Seq.empty

-- | The path of the root element.
rootPath :: Path
rootPath = Seq.singleton 0

-- | Checks if a point is inside the given rect.
pointInRect :: Point -> Rect -> Bool
pointInRect (Point px py) rect = coordInRectH px rect && coordInRectY py rect

-- | Checks if a point is inside the given ellipse.
pointInEllipse :: Point -> Rect -> Bool
pointInEllipse (Point px py) rect = ellipseTest <= 1 where
  Rect rx ry rw rh = rect
  ew = rw / 2
  eh = rh / 2
  cx = rx + ew
  cy = ry + eh
  ellipseTest = ((px - cx) ^ 2) / ew ^ 2  + ((py - cy) ^ 2) / eh ^ 2

-- | Adds two points.
addPoint :: Point -> Point -> Point
addPoint (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

-- | Subtracts one point from another.
subPoint :: Point -> Point -> Point
subPoint (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

-- | Multiplies the coordinates of a point by the given factor.
mulPoint :: Double -> Point -> Point
mulPoint factor (Point x y) = Point (factor * x) (factor * y)

-- | Negates the coordinates of a point.
negPoint :: Point -> Point
negPoint (Point x y) = Point (-x) (-y)

-- | Checks if a coordinate is inside the horizontal range of a rect.
coordInRectH :: Double -> Rect -> Bool
coordInRectH px (Rect x y w h) = px >= x && px < x + w

-- | Checks if a coordinate is inside the vertical range of a rect.
coordInRectY :: Double -> Rect -> Bool
coordInRectY py (Rect x y w h) = py >= y && py < y + h

-- | Adds width and height to a Size.
addToSize :: Size -> Double -> Double -> Maybe Size
addToSize (Size w h) w2 h2 = newSize where
  nw = w + w2
  nh = h + h2
  newSize
    | nw >= 0 && nh >= 0 = Just $ Size nw nh
    | otherwise = Nothing

-- | Subtracts width and height from a Size.
subtractFromSize :: Size -> Double -> Double -> Maybe Size
subtractFromSize (Size w h) w2 h2 = newSize where
  nw = w - w2
  nh = h - h2
  newSize
    | nw >= 0 && nh >= 0 = Just $ Size nw nh
    | otherwise = Nothing

-- | Moves a rect by the provided offset.
moveRect :: Point -> Rect -> Rect
moveRect (Point x y) (Rect rx ry rw rh) = Rect (rx + x) (ry + y) rw rh

-- | Returns the middle point of a rect.
rectCenter :: Rect -> Point
rectCenter (Rect rx ry rw rh) = Point (rx + rw / 2) (ry + rh / 2)

-- | Checks if a rectangle is completely inside a rect.
rectInRect :: Rect -> Rect -> Bool
rectInRect inner outer = rectInRectH inner outer && rectInRectV inner outer

-- | Checks if a rectangle is completely inside a rectangle horizontal area.
rectInRectH :: Rect -> Rect -> Bool
rectInRectH (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
  x1 >= x2 && x1 + w1 <= x2 + w2

-- | Checks if a rectangle is completely inside a rectangle vertical area.
rectInRectV :: Rect -> Rect -> Bool
rectInRectV (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
  y1 >= y2 && y1 + h1 <= y2 + h2

-- | Checks if a rectangle overlaps another rectangle.
rectsOverlap :: Rect -> Rect -> Bool
rectsOverlap (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) = overlapX && overlapY where
  overlapX = x1 < x2 + w2 && x1 + w1 > x2
  overlapY = y1 < y2 + h2 && y1 + h1 > y2

-- | Returns a point bounded to the horizontal and vertical limits of a rect.
rectBoundedPoint :: Rect -> Point -> Point
rectBoundedPoint (Rect rx ry rw rh) (Point px py) = Point px2 py2 where
  px2 = max rx . min (rx + rw) $ px
  py2 = max ry . min (ry + rh) $ py

-- | Adds individual x, y, w and h coordinates to a rect.
addToRect :: Rect -> Double -> Double -> Double -> Double -> Maybe Rect
addToRect (Rect x y w h) l r t b = newRect where
  nx = x - l
  ny = y - t
  nw = w + l + r
  nh = h + t + b
  newRect
    | nw >= 0 && nh >= 0 = Just $ Rect nx ny nw nh
    | otherwise = Nothing

-- | Subtracts individual x, y, w and h coordinates from a rect.
subtractFromRect :: Rect -> Double -> Double -> Double -> Double -> Maybe Rect
subtractFromRect (Rect x y w h) l r t b = newRect where
  nx = x + l
  ny = y + t
  nw = w - l - r
  nh = h - t - b
  newRect
    | nw >= 0 && nh >= 0 = Just $ Rect nx ny nw nh
    | otherwise = Nothing

-- | Returns the intersection of two rects, if any.
intersectRects :: Rect -> Rect -> Maybe Rect
intersectRects (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) = newRect where
  nx1 = max x1 x2
  nx2 = min (x1 + w1) (x2 + w2)
  ny1 = max y1 y2
  ny2 = min (y1 + h1) (y2 + h2)
  nw = nx2 - nx1
  nh = ny2 - ny1
  newRect
    | nw >= 0 && nh >= 0 = Just $ Rect nx1 ny1 nw nh
    | otherwise = Nothing
