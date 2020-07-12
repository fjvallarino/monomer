module Monomer.Common.StyleUtil where

import Monomer.Common.Style
import Monomer.Graphics.Types

width :: Double -> Style
width width = mempty { _styleWidth = Just width }

height :: Double -> Style
height height = mempty { _styleHeight = Just height }

color :: Color -> Style
color color = mempty { _styleColor = Just color }

hover :: Color -> Style
hover color = mempty { _styleHover = Just color }

margin :: Double -> Style
margin mar = mempty {
  _styleMargin = Just mempty {
    _marginLeft = Just mar,
    _marginRight = Just mar,
    _marginTop = Just mar,
    _marginBottom = Just mar
  }
}

marginLeft :: Double -> Style
marginLeft mar = mempty {
  _styleMargin = Just mempty {
    _marginLeft = Just mar
  }
}

marginRight :: Double -> Style
marginRight mar = mempty {
  _styleMargin = Just mempty {
    _marginRight = Just mar
  }
}

marginTop :: Double -> Style
marginTop mar = mempty {
  _styleMargin = Just mempty {
    _marginTop = Just mar
  }
}

marginBottom :: Double -> Style
marginBottom mar = mempty {
  _styleMargin = Just mempty {
    _marginBottom = Just mar
  }
}

padding :: Double -> Style
padding padd = mempty {
  _stylePadding = Just mempty {
    _paddingLeft = Just padd,
    _paddingRight = Just padd,
    _paddingTop = Just padd,
    _paddingBottom = Just padd
  }
}

paddingLeft :: Double -> Style
paddingLeft padd = mempty {
  _stylePadding = Just mempty {
    _paddingLeft = Just padd
  }
}

paddingRight :: Double -> Style
paddingRight padd = mempty {
  _stylePadding = Just mempty {
    _paddingRight = Just padd
  }
}

paddingTop :: Double -> Style
paddingTop padd = mempty {
  _stylePadding = Just mempty {
    _paddingTop = Just padd
  }
}

paddingBottom :: Double -> Style
paddingBottom padd = mempty {
  _stylePadding = Just mempty {
    _paddingBottom = Just padd
  }
}

border :: Double -> Color -> Style
border width color = mempty {
  _styleBorder = Just mempty {
    _borderLeft = Just (BorderSide width color),
    _borderRight = Just (BorderSide width color),
    _borderTop = Just (BorderSide width color),
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

radius :: Double -> Style
radius rad = mempty { _styleRadius = Just (Radius jrad jrad jrad jrad) } where
  jrad = Just rad

radiusTopLeft :: Double -> Style
radiusTopLeft rad = mempty {
  _styleRadius = Just mempty {
    _radiusTopLeft = Just rad
  }
}

radiusTopRight :: Double -> Style
radiusTopRight rad = mempty {
  _styleRadius = Just mempty {
    _radiusTopRight = Just rad
  }
}

radiusBottomLeft :: Double -> Style
radiusBottomLeft rad = mempty {
  _styleRadius = Just mempty {
    _radiusBottomLeft = Just rad
  }
}

radiusBottomRight :: Double -> Style
radiusBottomRight rad = mempty {
  _styleRadius = Just mempty {
    _radiusBottomRight = Just rad
  }
}

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

textAlignLeft :: Style
textAlignLeft = textAlignH ALeft

textAlignCenter :: Style
textAlignCenter = textAlignH ACenter

textAlignRight :: Style
textAlignRight = textAlignH ARight

textAlignTop :: Style
textAlignTop = textAlignV ATop

textAlignMiddle :: Style
textAlignMiddle = textAlignV AMiddle

textAlignBottom :: Style
textAlignBottom = textAlignV ABottom
