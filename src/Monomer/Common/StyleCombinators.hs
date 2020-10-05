module Monomer.Common.StyleCombinators (
  Width(..),
  Height(..),
  FlexWidth(..),
  FlexHeight(..),
  BoundedWidth(..),
  BoundedHeight(..),
  MinWidth(..),
  MinHeight(..),
  MaxWidth(..),
  MaxHeight(..),
--  width,
--  height,
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
  iradius,
  iradiusTL,
  iradiusTR,
  iradiusBL,
  iradiusBR,
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

class Width t where
  width :: Double -> t

class Height t where
  height :: Double -> t

class FlexWidth t where
  flexWidth :: Double -> t

class FlexHeight t where
  flexHeight :: Double -> t

class BoundedWidth t where
  boundedWidth :: Double -> Double -> t

class BoundedHeight t where
  boundedHeight :: Double -> Double -> t

class MinWidth t where
  minWidth :: Double -> t

class MinHeight t where
  minHeight :: Double -> t

class MaxWidth t where
  maxWidth :: Double -> t

class MaxHeight t where
  maxHeight :: Double -> t

instance Width StyleState where
  width w = def & L.sizeReqW ?~ FixedSize w

instance Height StyleState where
  height h = def & L.sizeReqH ?~ FixedSize h

instance FlexWidth StyleState where
  flexWidth w = def & L.sizeReqW ?~ FlexSize w 1

instance FlexHeight StyleState where
  flexHeight h = def & L.sizeReqH ?~ FlexSize h 1

instance BoundedWidth StyleState where
  boundedWidth w1 w2 = def & L.sizeReqW ?~ BoundedSize w1 w2 1

instance BoundedHeight StyleState where
  boundedHeight h1 h2 = def & L.sizeReqH ?~ BoundedSize h1 h2 1

instance MinWidth StyleState where
  minWidth w = def & L.sizeReqW ?~ BoundedSize w (2 * w) 1

instance MinHeight StyleState where
  minHeight h = def & L.sizeReqH ?~ BoundedSize h (2 * h) 1

instance MaxWidth StyleState where
  maxWidth w = def & L.sizeReqW ?~ BoundedSize 0 w 1

instance MaxHeight StyleState where
  maxHeight h = def & L.sizeReqH ?~ BoundedSize 0 h 1

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

radiusCorner :: Double -> RadiusCorner
radiusCorner rad = RadiusCorner RadiusBoth rad

radius :: Double -> StyleState
radius rad = def & L.radius ?~ Radius jrad jrad jrad jrad where
  jrad = Just $ radiusCorner rad

radiusTL :: Double -> StyleState
radiusTL rad = def & L.radius . non def . L.topLeft ?~ radiusCorner rad

radiusTR :: Double -> StyleState
radiusTR rad = def & L.radius . non def . L.topRight ?~ radiusCorner rad

radiusBL :: Double -> StyleState
radiusBL rad = def & L.radius . non def . L.bottomLeft ?~ radiusCorner rad

radiusBR :: Double -> StyleState
radiusBR rad = def & L.radius . non def . L.bottomRight ?~ radiusCorner rad

iradiusCorner :: Double -> RadiusCorner
iradiusCorner rad = RadiusCorner RadiusInner rad

iradius :: Double -> StyleState
iradius rad = def & L.radius ?~ Radius jrad jrad jrad jrad where
  jrad = Just $ iradiusCorner rad

iradiusTL :: Double -> StyleState
iradiusTL rad = def & L.radius . non def . L.topLeft ?~ iradiusCorner rad

iradiusTR :: Double -> StyleState
iradiusTR rad = def & L.radius . non def . L.topRight ?~ iradiusCorner rad

iradiusBL :: Double -> StyleState
iradiusBL rad = def & L.radius . non def . L.bottomLeft ?~ iradiusCorner rad

iradiusBR :: Double -> StyleState
iradiusBR rad = def & L.radius . non def . L.bottomRight ?~ iradiusCorner rad

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
