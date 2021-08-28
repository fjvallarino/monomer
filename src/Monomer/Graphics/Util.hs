{-|
Module      : Monomer.Graphics.Util
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions for graphics related operations.
-}
{-# LANGUAGE Strict #-}

module Monomer.Graphics.Util (
  clampChannel,
  clampAlpha,
  rgb,
  rgba,
  rgbHex,
  rgbaHex,
  hsl,
  hsla,
  transparent,
  alignInRect,
  alignHInRect,
  alignVInRect
) where

import Data.Char (digitToInt)

import Monomer.Common.BasicTypes
import Monomer.Graphics.Types
import Monomer.Helper

-- | Restricts a color channel to its valid range.
clampChannel :: Int -> Int
clampChannel channel = clamp 0 255 channel

-- | Restricts an alpha channel to its valid range.
clampAlpha :: Double -> Double
clampAlpha alpha = clamp 0 1 alpha

{-|
Creates a Color from red, green and blue components. Valid range for each
component is [0, 255].
-}
rgb :: Int -> Int -> Int -> Color
rgb r g b = Color (clampChannel r) (clampChannel g) (clampChannel b) 1.0

{-|
Creates a Color from red, green and blue components plus alpha channel. Valid
range for each component is [0, 255], while alpha is [0, 1].
-}
rgba :: Int -> Int -> Int -> Double -> Color
rgba r g b a = Color {
  _colorR = clampChannel r,
  _colorG = clampChannel g,
  _colorB = clampChannel b,
  _colorA = clampAlpha a
}

-- | Creates a Color from a hex string. It may include a # prefix or not.
rgbHex :: String -> Color
rgbHex hex
  | length hex == 7 = rgbHexSix (tail hex)
  | length hex == 6 = rgbHexSix hex
  | otherwise = rgb 0 0 0

-- | Creates a color from a six characters hex string. Fails if len is invalid.
rgbHexSix :: [Char] -> Color
rgbHexSix hex = rgb r g b where
  [r1, r2, g1, g2, b1, b2] = hex
  r = digitToInt r1 * 16 + digitToInt r2
  g = digitToInt g1 * 16 + digitToInt g2
  b = digitToInt b1 * 16 + digitToInt b2

{-|
Creates a Color from a hex string plus an alpha component. It may include a #
prefix or not.
-}
rgbaHex :: String -> Double -> Color
rgbaHex hex alpha = (rgbHex hex) {
    _colorA = clampAlpha alpha
  }

{-|
Creates a Color instance from HSL components. The valid ranges are:

- Hue: [0, 360]
- Saturation: [0, 100]
- Lightness: [0, 100]

Alpha is set to 1.0.
-}
hsl :: Int -> Int -> Int -> Color
hsl h s l = Color r g b 1.0 where
  vh = clamp 0 360 (fromIntegral h)
  vs = clamp 0 100 (fromIntegral s / 100)
  vl = clamp 0 100 (fromIntegral l / 100)
  a = vs * min vl (1 - vl)
  f n = vl - a * max mink (-1) where
    k = fromIntegral $ round (n + vh / 30) `mod` 12
    mink = minimum [k - 3, 9 - k, 1]
  i n = clampChannel . round $ 255 * f n
  (r, g, b) = (i 0, i 8, i 4)

{-|
Creates a Color instance from HSL components. The valid ranges are:

- Hue: [0, 360]
- Saturation: [0, 100]
- Lightness: [0, 100]
- Alpha: [0, 1]
-}
hsla :: Int -> Int -> Int -> Double -> Color
hsla h s l a = (hsl h s l) {
    _colorA = clampAlpha a
  }

-- | Creates a non visible color.
transparent :: Color
transparent = rgba 0 0 0 0

{-|
Aligns the child rect inside the parent given the alignment constraints.

Note: The child rect can overflow the parent.
-}
alignInRect :: Rect -> Rect -> AlignH -> AlignV -> Rect
alignInRect parent child ah av = newRect where
  tempRect = alignVInRect parent child av
  newRect = alignHInRect parent tempRect ah

-- | Aligns the child rect horizontally inside the parent.
alignHInRect :: Rect -> Rect -> AlignH -> Rect
alignHInRect parent child ah = newRect where
  Rect px _ pw _ = parent
  Rect _ cy cw ch = child
  newX = case ah of
    ALeft -> px
    ACenter -> px + (pw - cw) / 2
    ARight -> px + pw - cw
  newRect = Rect newX cy cw ch

-- | Aligns the child rect vertically inside the parent.
alignVInRect :: Rect -> Rect -> AlignV -> Rect
alignVInRect parent child av = newRect where
  Rect _ py _ ph = parent
  Rect cx _ cw ch = child
  newY = case av of
    ATop -> py
    AMiddle -> py + (ph - ch) / 2
    ABottom -> py + ph - ch
  newRect = Rect cx newY cw ch
