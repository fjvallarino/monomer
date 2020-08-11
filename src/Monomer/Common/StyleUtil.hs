module Monomer.Common.StyleUtil where

import Monomer.Common.Style
import Monomer.Graphics.Types

import Monomer.Common.LensStyle

width :: Double -> Style
width width = mempty { _styleWidth = Just width }

height :: Double -> Style
height height = mempty { _styleHeight = Just height }

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

basicColorM :: Maybe Color -> Style
basicColorM mcolor = mempty {
  _styleBasic = Just mempty {
    _sstColor = mcolor
  }
}

basicColor :: Color -> Style
basicColor color = basicColorM (Just color)

hoverColor :: Color -> Style
hoverColor color = mempty {
  _styleHover = Just mempty {
    _sstColor = Just color
  }
}

focusColor :: Color -> Style
focusColor color = mempty {
  _styleFocus = Just mempty {
    _sstColor = Just color
  }
}

border :: Double -> Color -> Border
border width color = mempty {
  _brdLeft = Just (BorderSide width color),
  _brdRight = Just (BorderSide width color),
  _brdTop = Just (BorderSide width color),
  _brdBottom = Just (BorderSide width color)
}

borderLeft :: Double -> Color -> Border
borderLeft width color = mempty {
  _brdLeft = Just (BorderSide width color)
}

borderRight :: Double -> Color -> Border
borderRight width color = mempty {
  _brdRight = Just (BorderSide width color)
}

borderTop :: Double -> Color -> Border
borderTop width color = mempty {
  _brdTop = Just (BorderSide width color)
}

borderBottom :: Double -> Color -> Border
borderBottom width color = mempty {
  _brdBottom = Just (BorderSide width color)
}

radius :: Double -> Radius
radius rad = Radius jrad jrad jrad jrad where
  jrad = Just rad

radiusTopLeft :: Double -> Radius
radiusTopLeft rad = mempty {
  _radTopLeft = Just rad
}

radiusTopRight :: Double -> Radius
radiusTopRight rad = mempty {
  _radTopRight = Just rad
}

radiusBottomLeft :: Double -> Radius
radiusBottomLeft rad = mempty {
  _radBottomLeft = Just rad
}

radiusBottomRight :: Double -> Radius
radiusBottomRight rad = mempty {
  _radBottomRight = Just rad
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
    _txsFontSize = Just (FontSize size)
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
