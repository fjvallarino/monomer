module Monomer.Graphics.Color where

import Monomer.Graphics.Types

transparent   = rgba 0 0 0 0

white         = rgb 255 255 255
black         = rgb   0   0   0
-- Red
lightYellow   = rgb 255 255 224
yellow        = rgb 255  255  0
orange        = rgb 255  69   0
red           = rgb 255   0   0
brown         = rgb 128   0   0
-- Green
lightGreen    = rgb 128 255 128
green         = rgb   0 255   0
darkGreen     = rgb   0 128   0
-- Blue
lightSkyBlue  = rgb 135 206 250
lightBlue     = rgb 173 216 230
blue          = rgb   0   0 255
darkBlue      = rgb   0   0 139
-- Gray
lightGray     = rgb 191 191 191
gray          = rgb 127 127 127
darkGray      = rgb  63  63  63
darkerGray    = rgb  32  32  32
-- Pink
lightPink     = rgb 255 192 203
pink          = rgb 255 105 180
darkPink      = rgb 199  21 133

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
  _colorR = clampChannel r,
  _colorG = clampChannel g,
  _colorB = clampChannel b,
  _colorA = clampAlpha a
}
