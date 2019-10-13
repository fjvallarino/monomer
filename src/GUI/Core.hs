{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module GUI.Core where

import Control.Monad
import Data.Default
import Lens.Micro
import Lens.Micro.TH (makeLenses)

import qualified Data.Text as T

data Align = Align AlignH AlignV deriving (Show, Eq)
data AlignH = ALeft | ACenter | ARight deriving (Show, Eq)
data AlignV = ATop | AMiddle | ABottom deriving (Show, Eq)

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

data Color =
    RGB !Double !Double !Double
  deriving (Show, Eq)

instance Semigroup Color where
  (<>) _ c2 = c2

instance Default Color where
  def = RGB 0 0 0

white = RGB 255 255 255
black = RGB   0   0   0
red   = RGB 255   0   0
green = RGB   0 255   0
blue  = RGB   0   0 255

makeLenses ''Point
makeLenses ''Size
makeLenses ''Rect

type Font = T.Text
type FontSize = Double

data Renderer m  = (Monad m) => Renderer {
  beginPath :: m (),
  stroke :: m (),
  fill :: m (),
  fillColor :: Color -> m (),
  fillLinearGradient :: Point -> Point -> Color -> Color -> m (),
  strokeColor :: Color -> m (),
  strokeWidth :: Double -> m (),
  moveTo :: Point -> m (),
  line :: Point -> Point -> m (),
  lineTo :: Point -> m (),
  rect :: Rect -> m (),
  arc :: Point -> Double -> Double -> Double -> m (),
  quadTo :: Point -> Point -> m (),
  ellipse :: Rect -> m (),
  text :: Rect -> Font -> FontSize -> Align -> T.Text -> m (),
  textBounds :: Font -> FontSize -> T.Text -> m Size
}

inRect :: Rect -> Point -> Bool
inRect (Rect x y w h) (Point x2 y2) = (x2 >= x && x2 < x + w) && (y2 >= y && y2 < y + h)

rotateList :: [a] -> [a]
rotateList [] = []
rotateList (x:xs) = xs ++ [x]

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
