module Monomer.Graphics.Types where

import Data.Default
import Data.Text (Text)

data Color = Color {
  _r :: Int,
  _g :: Int,
  _b :: Int,
  _alpha :: Double
} deriving (Show, Eq)

instance Semigroup Color where
  (<>) _ c2 = c2

instance Default Color where
  def = Color 0 0 0 1.0

type Font = Text
type FontSize = Double

data Align
  = Align AlignH AlignV
  deriving (Show, Eq)

data AlignH
  = ALeft
  | ACenter
  | ARight
  deriving (Show, Eq)

data AlignV
  = ATop
  | AMiddle
  | ABottom
  deriving (Show, Eq)
