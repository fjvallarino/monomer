{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Monomer.Core.BasicTypes where

import Codec.Serialise
import Data.Default
import Data.Sequence (Seq)
import GHC.Generics

import qualified Data.Sequence as Seq

type PathStep = Int
type Path = Seq PathStep
type Factor = Double

data Point = Point {
  _pX :: {-# UNPACK #-} !Double,
  _pY :: {-# UNPACK #-} !Double
} deriving (Eq, Show, Generic, Serialise)

instance Default Point where
  def = Point 0 0

data Size = Size {
  _sW :: {-# UNPACK #-} !Double,
  _sH :: {-# UNPACK #-} !Double
} deriving (Eq, Show, Generic, Serialise)

instance Default Size where
  def = Size 0 0

data Rect = Rect {
  _rX :: {-# UNPACK #-} !Double,
  _rY :: {-# UNPACK #-} !Double,
  _rW :: {-# UNPACK #-} !Double,
  _rH :: {-# UNPACK #-} !Double
} deriving (Eq, Show, Generic, Serialise)

instance Default Rect where
  def = Rect 0 0 0 0

emptyPath :: Path
emptyPath = Seq.empty

rootPath :: Path
rootPath = Seq.singleton 0

pointInRect :: Point -> Rect -> Bool
pointInRect (Point px py) rect = coordInRectH px rect && coordInRectY py rect

pointInEllipse :: Point -> Rect -> Bool
pointInEllipse (Point px py) rect = ellipseTest <= 1 where
  Rect rx ry rw rh = rect
  ew = rw / 2
  eh = rh / 2
  cx = rx + ew
  cy = ry + eh
  ellipseTest = ((px - cx) ^ 2) / ew ^ 2  + ((py - cy) ^ 2) / eh ^ 2

addPoint :: Point -> Point -> Point
addPoint (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

subPoint :: Point -> Point -> Point
subPoint (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

negPoint :: Point -> Point
negPoint (Point x y) = Point (-x) (-y)

coordInRectH :: Double -> Rect -> Bool
coordInRectH px (Rect x y w h) = px >= x && px < x + w

coordInRectY :: Double -> Rect -> Bool
coordInRectY py (Rect x y w h) = py >= y && py < y + h

addToSize :: Size -> Double -> Double -> Maybe Size
addToSize (Size w h) w2 h2 = newSize where
  nw = w + w2
  nh = h + h2
  newSize
    | nw >= 0 && nh >= 0 = Just $ Size nw nh
    | otherwise = Nothing

subtractFromSize :: Size -> Double -> Double -> Maybe Size
subtractFromSize (Size w h) w2 h2 = newSize where
  nw = w - w2
  nh = h - h2
  newSize
    | nw >= 0 && nh >= 0 = Just $ Size nw nh
    | otherwise = Nothing

moveRect :: Point -> Rect -> Rect
moveRect (Point x y) (Rect rx ry rw rh) = Rect (rx + x) (ry + y) rw rh

rectCenter :: Rect -> Point
rectCenter (Rect rx ry rw rh) = Point (rx + rw / 2) (ry + rh / 2)

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
  overlapX = x1 < x2 + w2 && x1 + w1 > x2
  overlapY = y1 < y2 + h2 && y1 + h1 > y2

rectBoundedPoint :: Rect -> Point -> Point
rectBoundedPoint (Rect rx ry rw rh) (Point px py) = Point px2 py2 where
  px2 = max rx . min (rx + rw) $ px
  py2 = max ry . min (ry + rh) $ py

addToRect :: Rect -> Double -> Double -> Double -> Double -> Maybe Rect
addToRect (Rect x y w h) l r t b = newRect where
  nx = x - l
  ny = y - t
  nw = w + l + r
  nh = h + t + b
  newRect
    | nw >= 0 && nh >= 0 = Just $ Rect nx ny nw nh
    | otherwise = Nothing

subtractFromRect :: Rect -> Double -> Double -> Double -> Double -> Maybe Rect
subtractFromRect (Rect x y w h) l r t b = newRect where
  nx = x + l
  ny = y + t
  nw = w - l - r
  nh = h - t - b
  newRect
    | nw >= 0 && nh >= 0 = Just $ Rect nx ny nw nh
    | otherwise = Nothing

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
