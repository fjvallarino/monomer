module Monomer.Common.StyleUtil where

import Control.Lens ((&), (.~), (?~), non)
import Data.Default

import Monomer.Common.Style
import Monomer.Graphics.Types

import qualified Monomer.Common.LensStyle as L

width :: Double -> Style
width width = mempty { _styleWidth = Just width }

height :: Double -> Style
height height = mempty { _styleHeight = Just height }

margin :: Double -> Style
margin mar = def & L.margin ?~ Margin jm jm jm jm where
  jm = Just mar

marginLeft :: Double -> Style
marginLeft mar = def & L.margin . non def . L.left ?~ mar

marginRight :: Double -> Style
marginRight mar = def & L.margin . non def . L.right ?~ mar

marginTop :: Double -> Style
marginTop mar = def & L.margin . non def . L.top ?~ mar

marginBottom :: Double -> Style
marginBottom mar = def & L.margin . non def . L.bottom ?~ mar

padding :: Double -> Style
padding padd = def & L.padding ?~ Padding jp jp jp jp where
  jp = Just padd

paddingLeft :: Double -> Style
paddingLeft padd = def & L.padding . non def . L.left ?~ padd

paddingRight :: Double -> Style
paddingRight padd = def & L.padding . non def . L.right ?~ padd

paddingTop :: Double -> Style
paddingTop padd = def & L.padding . non def . L.top ?~ padd

paddingBottom :: Double -> Style
paddingBottom padd = def & L.padding . non def . L.bottom ?~ padd

basic :: StyleState -> Style
basic sst = def & L.basic ?~ sst

hover :: StyleState -> Style
hover sst = def & L.hover ?~ sst

focus :: StyleState -> Style
focus sst = def & L.focus ?~ sst

color :: Color -> StyleState
color col = def & L.color ?~ col

border :: Double -> Color -> StyleState
border w col = def & L.border ?~ Border bs bs bs bs where
  bs =  Just (BorderSide w col)

borderLeft :: Double -> Color -> StyleState
borderLeft w col = def & L.border . non def . L.left ?~ BorderSide w col

borderRight :: Double -> Color -> StyleState
borderRight w col = def & L.border . non def . L.right ?~ BorderSide w col

borderTop :: Double -> Color -> StyleState
borderTop w col = def & L.border . non def . L.top ?~ BorderSide w col

borderBottom :: Double -> Color -> StyleState
borderBottom w col = def & L.border . non def . L.bottom ?~ BorderSide w col

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
