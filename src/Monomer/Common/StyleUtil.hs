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

bgColor :: Color -> Style
bgColor color = mempty { _styleBgColor = Just color }

fgColor :: Color -> Style
fgColor color = mempty { _styleFgColor = Just color }

bgHoverColor :: Color -> Style
bgHoverColor color = mempty { _styleBgHoverColor = Just color }

fgHoverColor :: Color -> Style
fgHoverColor color = mempty { _styleFgHoverColor = Just color }

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
