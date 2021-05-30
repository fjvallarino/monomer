{-|
Module      : Monomer.Graphics.Color
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions for color related operations.
-}
module Monomer.Graphics.Color (
  clampChannel,
  clampAlpha,
  rgb,
  rgbHex,
  rgba,
  transparent
) where

import Data.Char (digitToInt)

import Monomer.Graphics.Types

-- | Restricts a color channel to its valid range.
clampChannel :: Int -> Int
clampChannel channel = clamp 0 255 channel

-- | Restricts an alpha channel to its valid range.
clampAlpha :: Double -> Double
clampAlpha alpha = clamp 0 1 alpha

-- | Creates a Color from three red, green and blue colors.
rgb :: Int -> Int -> Int -> Color
rgb r g b = Color (clampChannel r) (clampChannel g) (clampChannel b) 1.0

-- | Creates a Color from a hex string. It may include a # prefix or not.
rgbHex :: String -> Color
rgbHex hex
  | length hex == 7 = rgbHex (tail hex)
  | length hex == 6 = rgb r g b
  | otherwise = rgb 0 0 0
  where
    [r1, r2, g1, g2, b1, b2] = hex
    r = digitToInt r1 * 16 + digitToInt r2
    g = digitToInt g1 * 16 + digitToInt g2
    b = digitToInt b1 * 16 + digitToInt b2

-- | Creates a Color from three red, green and blue colors plus alpha channel.
rgba :: Int -> Int -> Int -> Double -> Color
rgba r g b a = Color {
  _colorR = clampChannel r,
  _colorG = clampChannel g,
  _colorB = clampChannel b,
  _colorA = clampAlpha a
}

-- Creates a non visible color.
transparent :: Color
transparent = rgba 0 0 0 0

-- Helpers
clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx
