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
  def = Color 255 255 255 1.0

newtype Font
  = Font { unFont :: Text }
  deriving (Eq, Show)

newtype FontSize
  = FontSize { unFontSize :: Double }
  deriving (Eq, Show)

instance Default Font where
  def = Font "sans"

instance Default FontSize where
  def = FontSize 32

data Align
  = Align AlignH AlignV
  deriving (Show, Eq)

instance Default Align where
  def = Align ACenter AMiddle

data AlignH
  = ALeft
  | ACenter
  | ARight
  deriving (Show, Eq)

instance Default AlignH where
  def = ACenter

data AlignV
  = ATop
  | AMiddle
  | ABottom
  deriving (Show, Eq)

instance Default AlignV where
  def = AMiddle
