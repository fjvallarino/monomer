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
    _mgnLeft = Just mar,
    _mgnRight = Just mar,
    _mgnTop = Just mar,
    _mgnBottom = Just mar
  }
}

marginLeft :: Double -> Style
marginLeft mar = mempty {
  _styleMargin = Just mempty {
    _mgnLeft = Just mar
  }
}

marginRight :: Double -> Style
marginRight mar = mempty {
  _styleMargin = Just mempty {
    _mgnRight = Just mar
  }
}

marginTop :: Double -> Style
marginTop mar = mempty {
  _styleMargin = Just mempty {
    _mgnTop = Just mar
  }
}

marginBottom :: Double -> Style
marginBottom mar = mempty {
  _styleMargin = Just mempty {
    _mgnBottom = Just mar
  }
}

padding :: Double -> Style
padding padd = mempty {
  _stylePadding = Just mempty {
    _padLeft = Just padd,
    _padRight = Just padd,
    _padTop = Just padd,
    _padBottom = Just padd
  }
}

paddingLeft :: Double -> Style
paddingLeft padd = mempty {
  _stylePadding = Just mempty {
    _padLeft = Just padd
  }
}

paddingRight :: Double -> Style
paddingRight padd = mempty {
  _stylePadding = Just mempty {
    _padRight = Just padd
  }
}

paddingTop :: Double -> Style
paddingTop padd = mempty {
  _stylePadding = Just mempty {
    _padTop = Just padd
  }
}

paddingBottom :: Double -> Style
paddingBottom padd = mempty {
  _stylePadding = Just mempty {
    _padBottom = Just padd
  }
}

border :: Double -> Color -> Style
border width color = mempty {
  _styleBorder = Just mempty {
    _brdLeft = Just (BorderSide width color),
    _brdRight = Just (BorderSide width color),
    _brdTop = Just (BorderSide width color),
    _brdBottom = Just (BorderSide width color)
  }
}

borderLeft :: Double -> Color -> Style
borderLeft width color = mempty {
  _styleBorder = Just mempty {
    _brdLeft = Just (BorderSide width color)
  }
}

borderRight :: Double -> Color -> Style
borderRight width color = mempty {
  _styleBorder = Just mempty {
    _brdRight = Just (BorderSide width color)
  }
}

borderTop :: Double -> Color -> Style
borderTop width color = mempty {
  _styleBorder = Just mempty {
    _brdTop = Just (BorderSide width color)
  }
}

borderBottom :: Double -> Color -> Style
borderBottom width color = mempty {
  _styleBorder = Just mempty {
    _brdBottom = Just (BorderSide width color)
  }
}

radius :: Double -> Style
radius rad = mempty { _styleRadius = Just (Radius jrad jrad jrad jrad) } where
  jrad = Just rad

radiusTopLeft :: Double -> Style
radiusTopLeft rad = mempty {
  _styleRadius = Just mempty {
    _radTopLeft = Just rad
  }
}

radiusTopRight :: Double -> Style
radiusTopRight rad = mempty {
  _styleRadius = Just mempty {
    _radTopRight = Just rad
  }
}

radiusBottomLeft :: Double -> Style
radiusBottomLeft rad = mempty {
  _styleRadius = Just mempty {
    _radBottomLeft = Just rad
  }
}

radiusBottomRight :: Double -> Style
radiusBottomRight rad = mempty {
  _styleRadius = Just mempty {
    _radBottomRight = Just rad
  }
}

textColor :: Color -> Style
textColor color = mempty {
  _styleText = Just $ mempty {
    _txsColor = Just color
  }
}

textSize :: Double -> Style
textSize size = mempty {
  _styleText = Just $ mempty {
    _txsFontSize = Just size
  }
}

textAlignH :: AlignH -> Style
textAlignH alignH = mempty {
  _styleText = Just $ mempty {
    _txsAlignH = Just alignH
  }
}

textAlignV :: AlignV -> Style
textAlignV alignV = mempty {
  _styleText = Just $ mempty {
    _txsAlignV = Just alignV
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
