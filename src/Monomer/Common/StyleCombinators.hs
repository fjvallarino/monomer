module Monomer.Common.StyleCombinators (
  width,
  height,
  margin,
  marginL,
  marginR,
  marginT,
  marginB,
  padding,
  paddingL,
  paddingR,
  paddingT,
  paddingB,
  border,
  borderL,
  borderR,
  borderT,
  borderB,
  radius,
  radiusTL,
  radiusTR,
  radiusBL,
  radiusBR,
  bgColor,
  fgColor,
  textFont,
  textSize,
  textColor,
  textAlignH,
  textAlignV,
  textLeft,
  textCenter,
  textRight,
  textTop,
  textMiddle,
  textBottom
) where

import Control.Lens ((&), (?~), non)
import Data.Default

import Monomer.Common.Style
import Monomer.Graphics.Types

import qualified Monomer.Common.LensStyle as L

width :: Double -> StyleState
width w = def & L.width ?~ w

height :: Double -> StyleState
height h = def & L.height ?~ h

margin :: Double -> StyleState
margin mar = def & L.margin ?~ Margin jm jm jm jm where
  jm = Just mar

marginL :: Double -> StyleState
marginL mar = def & L.margin . non def . L.left ?~ mar

marginR :: Double -> StyleState
marginR mar = def & L.margin . non def . L.right ?~ mar

marginT :: Double -> StyleState
marginT mar = def & L.margin . non def . L.top ?~ mar

marginB :: Double -> StyleState
marginB mar = def & L.margin . non def . L.bottom ?~ mar

padding :: Double -> StyleState
padding padd = def & L.padding ?~ Padding jp jp jp jp where
  jp = Just padd

paddingL :: Double -> StyleState
paddingL padd = def & L.padding . non def . L.left ?~ padd

paddingR :: Double -> StyleState
paddingR padd = def & L.padding . non def . L.right ?~ padd

paddingT :: Double -> StyleState
paddingT padd = def & L.padding . non def . L.top ?~ padd

paddingB :: Double -> StyleState
paddingB padd = def & L.padding . non def . L.bottom ?~ padd

border :: Double -> Color -> StyleState
border w col = def & L.border ?~ Border bs bs bs bs where
  bs =  Just (BorderSide w col)

borderL :: Double -> Color -> StyleState
borderL w col = def & L.border . non def . L.left ?~ BorderSide w col

borderR :: Double -> Color -> StyleState
borderR w col = def & L.border . non def . L.right ?~ BorderSide w col

borderT :: Double -> Color -> StyleState
borderT w col = def & L.border . non def . L.top ?~ BorderSide w col

borderB :: Double -> Color -> StyleState
borderB w col = def & L.border . non def . L.bottom ?~ BorderSide w col

radius :: Double -> StyleState
radius rad = def & L.radius ?~ Radius jrad jrad jrad jrad where
  jrad = Just rad

radiusTL :: Double -> StyleState
radiusTL rad = def & L.radius . non def . L.topLeft ?~ rad

radiusTR :: Double -> StyleState
radiusTR rad = def & L.radius . non def . L.topRight ?~ rad

radiusBL :: Double -> StyleState
radiusBL rad = def & L.radius . non def . L.bottomLeft ?~ rad

radiusBR :: Double -> StyleState
radiusBR rad = def & L.radius . non def . L.bottomRight ?~ rad

bgColor :: Color -> StyleState
bgColor col = def & L.bgColor ?~ col

fgColor :: Color -> StyleState
fgColor col = def & L.fgColor ?~ col

hlColor :: Color -> StyleState
hlColor col = def & L.hlColor ?~ col

textFont :: Font -> StyleState
textFont font = def & L.text . non def . L.font ?~ font

textSize :: Double -> StyleState
textSize size = def & L.text . non def . L.fontSize ?~ FontSize size

textColor :: Color -> StyleState
textColor col = def & L.text . non def . L.fontColor ?~ col

textAlignH :: AlignH -> StyleState
textAlignH align = def & L.text . non def . L.alignH ?~ align

textAlignV :: AlignV -> StyleState
textAlignV align = def & L.text . non def . L.alignV ?~ align

textLeft :: StyleState
textLeft = textAlignH ALeft

textCenter :: StyleState
textCenter = textAlignH ACenter

textRight :: StyleState
textRight = textAlignH ARight

textTop :: StyleState
textTop = textAlignV ATop

textMiddle :: StyleState
textMiddle = textAlignV AMiddle

textBottom :: StyleState
textBottom = textAlignV ABottom
