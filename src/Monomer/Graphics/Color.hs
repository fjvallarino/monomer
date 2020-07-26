module Monomer.Graphics.Color where

import Monomer.Graphics.Types

white      = rgb 255 255 255
black      = rgb   0   0   0
red        = rgb 255   0   0
lightGreen = rgb 128 255 128
green      = rgb   0 255   0
darkGreen  = rgb   0 128   0
lightBlue  = rgb 128 128 255
blue       = rgb   0   0 255
darkBlue   = rgb   0   0 128
lightGray  = rgb 191 191 191
gray       = rgb 127 127 127
darkGray   = rgb  63  63  63

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

clampChannel :: Int -> Int
clampChannel channel = clamp 0 255 channel

clampAlpha :: Double -> Double
clampAlpha alpha = clamp 0 1 alpha

rgb :: Int -> Int -> Int -> Color
rgb r g b = Color (clampChannel r) (clampChannel g) (clampChannel b) 1.0

rgba :: Int -> Int -> Int -> Double -> Color
rgba r g b a = Color {
  _r = clampChannel r,
  _g = clampChannel g,
  _b = clampChannel b,
  _alpha = clampAlpha a
}
