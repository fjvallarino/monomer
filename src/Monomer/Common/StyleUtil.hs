module Monomer.Common.StyleUtil where

import Monomer.Common.Style
import Monomer.Graphics.Types

width :: Double -> Style
width width = mempty { _styleWidth = Just width }

height :: Double -> Style
height height = mempty { _styleHeight = Just height }

border :: Double -> Color -> Style
border width color = mempty {
  _styleBorder = Just mempty {
    _borderLeft = Just (BorderSide width color),
    _borderRight = Just (BorderSide width color),
    _borderTop = Just (BorderSide width color),
    _borderBottom = Just (BorderSide width color)
  }
}

borderTop :: Double -> Color -> Style
borderTop width color = mempty {
  _styleBorder = Just mempty {
    _borderTop = Just (BorderSide width color)
  }
}

borderBottom :: Double -> Color -> Style
borderBottom width color = mempty {
  _styleBorder = Just mempty {
    _borderBottom = Just (BorderSide width color)
  }
}

borderLeft :: Double -> Color -> Style
borderLeft width color = mempty {
  _styleBorder = Just mempty {
    _borderLeft = Just (BorderSide width color)
  }
}

borderRight :: Double -> Color -> Style
borderRight width color = mempty {
  _styleBorder = Just mempty {
    _borderRight = Just (BorderSide width color)
  }
}

color :: Color -> Style
color color = mempty { _styleColor = Just color }

hover :: Color -> Style
hover color = mempty { _styleColor = Just color }

sRadius :: Double -> Style
sRadius rad = mempty { _styleRadius = Just (Radius jrad jrad jrad jrad) } where
  jrad = Just rad

textColor :: Color -> Style
textColor color = mempty {
  _styleText = Just $ mempty {
    _textStyleColor = Just color
  }
}

textSize :: Double -> Style
textSize size = mempty {
  _styleText = Just $ mempty {
    _textStyleFontSize = Just size
  }
}

textAlignH :: AlignH -> Style
textAlignH alignH = mempty {
  _styleText = Just $ mempty {
    _textStyleAlignH = Just alignH
  }
}

textAlignV :: AlignV -> Style
textAlignV alignV = mempty {
  _styleText = Just $ mempty {
    _textStyleAlignV = Just alignV
  }
}
