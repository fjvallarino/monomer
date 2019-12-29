{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module GUI.Common.Types where

import Control.Monad
import Data.Default
import Lens.Micro
import Lens.Micro.TH (makeLenses)

import qualified Data.Text as T

type Font = T.Text
type FontSize = Double

data Align = Align AlignH AlignV deriving (Show, Eq)

data AlignH = ALeft |
              ACenter |
              ARight deriving (Show, Eq)

data AlignV = ATop |
              AMiddle |
              ABottom deriving (Show, Eq)

data Direction = Horizontal | Vertical deriving (Show, Eq)

data SizePolicy = StrictSize |
                  FlexibleSize |
                  RemainderSize deriving (Show, Eq)

data ClipboardData = ClipboardEmpty | ClipboardText T.Text deriving (Eq, Show)

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
    RGB !Double !Double !Double |
    RGBA !Double !Double !Double !Double
  deriving (Show, Eq)

instance Semigroup Color where
  (<>) _ c2 = c2

instance Default Color where
  def = RGB 0 0 0

data Renderer m  = (Monad m) => Renderer {
  beginPath :: m (),
  -- Context management
  saveContext :: m (),
  restoreContext :: m (),
  -- Scissor operations
  scissor :: Rect -> m (),
  resetScissor :: m (),
  -- Strokes
  stroke :: m (),
  strokeColor :: Color -> m (),
  strokeWidth :: Double -> m (),
  -- Fill
  fill :: m (),
  fillColor :: Color -> m (),
  fillLinearGradient :: Point -> Point -> Color -> Color -> m (),
  -- Drawing
  moveTo :: Point -> m (),
  line :: Point -> Point -> m (),
  lineTo :: Point -> m (),
  rect :: Rect -> m (),
  arc :: Point -> Double -> Double -> Double -> m (),
  quadTo :: Point -> Point -> m (),
  ellipse :: Rect -> m (),
  -- Text
  text :: Rect -> Font -> FontSize -> Align -> T.Text -> m (),
  textBounds :: Font -> FontSize -> T.Text -> m Size
}

makeLenses ''Point
makeLenses ''Size
makeLenses ''Rect
