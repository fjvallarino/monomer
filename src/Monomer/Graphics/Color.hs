module Monomer.Graphics.Color where

import Data.Char (digitToInt)

import Monomer.Graphics.Types

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

clampChannel :: Int -> Int
clampChannel channel = clamp 0 255 channel

clampAlpha :: Double -> Double
clampAlpha alpha = clamp 0 1 alpha

rgb :: Int -> Int -> Int -> Color
rgb r g b = Color (clampChannel r) (clampChannel g) (clampChannel b) 1.0

rgbHex :: String -> Color
rgbHex hex
  | length hex == 6 = rgb r g b
  | otherwise = rgb 0 0 0
  where
    [r1, r2, g1, g2, b1, b2] = hex
    r = digitToInt r1 * 16 + digitToInt r2
    g = digitToInt g1 * 16 + digitToInt g2
    b = digitToInt b1 * 16 + digitToInt b2

rgba :: Int -> Int -> Int -> Double -> Color
rgba r g b a = Color {
  _colorR = clampChannel r,
  _colorG = clampChannel g,
  _colorB = clampChannel b,
  _colorA = clampAlpha a
}

transparent :: Color
transparent = rgba 0 0 0 0
