{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Common.Types where

import Data.Default
import Lens.Micro
import Lens.Micro.TH (makeLenses)

--import qualified Data.Text as T

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
