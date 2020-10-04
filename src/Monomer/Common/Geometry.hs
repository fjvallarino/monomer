{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Common.Geometry where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default

type Coord = Double
type Factor = Double

data Point = Point {
  _pX :: !Coord,
  _pY :: !Coord
} deriving (Show, Eq)

instance Default Point where
  def = Point 0 0

data Size = Size {
  _sW :: !Coord,
  _sH :: !Coord
} deriving (Show, Eq)

instance Default Size where
  def = Size 0 0

data Rect = Rect {
  _rX :: !Coord,
  _rY :: !Coord,
  _rW :: !Coord,
  _rH :: !Coord
} deriving (Show, Eq)

instance Default Rect where
  def = Rect 0 0 0 0

makeLensesWith abbreviatedFields ''Point
makeLensesWith abbreviatedFields ''Size
makeLensesWith abbreviatedFields ''Rect

pointInRect :: Point -> Rect -> Bool
pointInRect (Point px py) rect = coordInRectH px rect && coordInRectY py rect

addPoint :: Point -> Point -> Point
addPoint (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

coordInRectH :: Coord -> Rect -> Bool
coordInRectH px (Rect x y w h) = px >= x && px < x + w

coordInRectY :: Coord -> Rect -> Bool
coordInRectY py (Rect x y w h) = py >= y && py < y + h

addToSize :: Size -> Coord -> Coord -> Size
addToSize (Size w h) w2 h2 = Size nw nh where
  nw = max 0 $ w + w2
  nh = max 0 $ h + h2

subtractFromSize :: Size -> Coord -> Coord -> Size
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

addToRect :: Rect -> Coord -> Coord -> Coord -> Coord -> Rect
addToRect (Rect x y w h) l r t b = Rect nx ny nw nh where
  nx = x - l
  ny = y - t
  nw = max 0 $ w + l + r
  nh = max 0 $ h + t + b

subtractFromRect :: Rect -> Coord -> Coord -> Coord -> Coord -> Rect
subtractFromRect (Rect x y w h) l r t b = Rect nx ny nw nh where
  nx = x + l
  ny = y + t
  nw = max 0 $ w - l - r
  nh = max 0 $ h - t - b
