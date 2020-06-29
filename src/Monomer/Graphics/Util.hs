module Monomer.Graphics.Util where

import Monomer.Graphics.Color
import Monomer.Graphics.Renderer
import Monomer.Graphics.Types

defaultColor :: Color
defaultColor = white

defaultFont :: Font
defaultFont = "sans"

defaultFontSize :: FontSize
defaultFontSize = 32

defaultAlignH :: AlignH
defaultAlignH = ACenter

defaultAlignV :: AlignV
defaultAlignV = AMiddle

degToRad :: Double -> Double
degToRad rad = rad * 3.1416 / 180.0
