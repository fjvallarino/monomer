module Monomer.Core.Style (
  module Monomer.Core.StyleTypes
) where

import Control.Lens ((&), (?~), non)
import Data.Default

import Monomer.Core.Combinators
import Monomer.Core.StyleTypes
import Monomer.Graphics.Types

import qualified Monomer.Core.Lens.Style as L

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

instance BgColor StyleState where
  bgColor col = def & L.bgColor ?~ col

instance FgColor StyleState where
  fgColor col = def & L.fgColor ?~ col

instance HlColor StyleState where
  hlColor col = def & L.hlColor ?~ col

instance TextFont StyleState where
  textFont font = def & L.text . non def . L.font ?~ font

instance TextSize StyleState where
  textSize size = def & L.text . non def . L.fontSize ?~ FontSize size

instance TextColor StyleState where
  textColor col = def & L.text . non def . L.fontColor ?~ col

instance TextLeft StyleState where
  textLeft = textAlignH ALeft

instance TextCenter StyleState where
  textCenter = textAlignH ACenter

instance TextRight StyleState where
  textRight = textAlignH ARight

instance TextTop StyleState where
  textTop = textAlignV ATop

instance TextMiddle StyleState where
  textMiddle = textAlignV AMiddle

instance TextBottom StyleState where
  textBottom = textAlignV ABottom

instance Margin_ StyleState where
  margin mar = def & L.margin ?~ Margin jm jm jm jm where
    jm = Just mar

instance MarginL StyleState where
  marginL mar = def & L.margin . non def . L.left ?~ mar

instance MarginR StyleState where
  marginR mar = def & L.margin . non def . L.right ?~ mar

instance MarginT StyleState where
  marginT mar = def & L.margin . non def . L.top ?~ mar

instance MarginB StyleState where
  marginB mar = def & L.margin . non def . L.bottom ?~ mar

instance Padding_ StyleState where
  padding padd = def & L.padding ?~ Padding jp jp jp jp where
    jp = Just padd

instance PaddingL StyleState where
  paddingL padd = def & L.padding . non def . L.left ?~ padd

instance PaddingR StyleState where
  paddingR padd = def & L.padding . non def . L.right ?~ padd

instance PaddingT StyleState where
  paddingT padd = def & L.padding . non def . L.top ?~ padd

instance PaddingB StyleState where
  paddingB padd = def & L.padding . non def . L.bottom ?~ padd

instance Border_ StyleState where
  border w col = def & L.border ?~ Border bs bs bs bs where
    bs =  Just (BorderSide w col)

instance BorderL StyleState where
  borderL w col = def & L.border . non def . L.left ?~ BorderSide w col

instance BorderR StyleState where
  borderR w col = def & L.border . non def . L.right ?~ BorderSide w col

instance BorderT StyleState where
  borderT w col = def & L.border . non def . L.top ?~ BorderSide w col

instance BorderB StyleState where
  borderB w col = def & L.border . non def . L.bottom ?~ BorderSide w col

instance Radius_ StyleState where
  radius rad = def & L.radius ?~ Radius jrad jrad jrad jrad where
    jrad = Just $ radiusCorner rad

instance RadiusTL StyleState where
  radiusTL rad = def & L.radius . non def . L.topLeft ?~ radiusCorner rad

instance RadiusTR StyleState where
  radiusTR rad = def & L.radius . non def . L.topRight ?~ radiusCorner rad

instance RadiusBL StyleState where
  radiusBL rad = def & L.radius . non def . L.bottomLeft ?~ radiusCorner rad

instance RadiusBR StyleState where
  radiusBR rad = def & L.radius . non def . L.bottomRight ?~ radiusCorner rad

instance InnerRadius_ StyleState where
  iradius rad = def & L.radius ?~ Radius jrad jrad jrad jrad where
    jrad = Just $ iradiusCorner rad

instance InnerRadiusTL StyleState where
  iradiusTL rad = def & L.radius . non def . L.topLeft ?~ iradiusCorner rad

instance InnerRadiusTR StyleState where
  iradiusTR rad = def & L.radius . non def . L.topRight ?~ iradiusCorner rad

instance InnerRadiusBL StyleState where
  iradiusBL rad = def & L.radius . non def . L.bottomLeft ?~ iradiusCorner rad

instance InnerRadiusBR StyleState where
  iradiusBR rad = def & L.radius . non def . L.bottomRight ?~ iradiusCorner rad

-- Internal

radiusCorner :: Double -> RadiusCorner
radiusCorner rad = RadiusCorner RadiusBoth rad

iradiusCorner :: Double -> RadiusCorner
iradiusCorner rad = RadiusCorner RadiusInner rad

textAlignH :: AlignH -> StyleState
textAlignH align = def & L.text . non def . L.alignH ?~ align

textAlignV :: AlignV -> StyleState
textAlignV align = def & L.text . non def . L.alignV ?~ align
