module Monomer.Core.Style (
  module Monomer.Core.StyleTypes
) where

import Control.Lens ((&), (?~), non)
import Data.Default

import Monomer.Core.Combinators
import Monomer.Core.StyleTypes
import Monomer.Graphics.Types

import qualified Monomer.Core.Lens as L

instance Width StyleState where
  width w = def & L.sizeReqW ?~ FixedSize w

instance Height StyleState where
  height h = def & L.sizeReqH ?~ FixedSize h

instance FlexWidth StyleState where
  flexWidth w = def & L.sizeReqW ?~ FlexSize w 1

instance FlexHeight StyleState where
  flexHeight h = def & L.sizeReqH ?~ FlexSize h 1

instance RangeWidth StyleState where
  rangeWidth w1 w2 = def & L.sizeReqW ?~ RangeSize w1 w2 1

instance RangeHeight StyleState where
  rangeHeight h1 h2 = def & L.sizeReqH ?~ RangeSize h1 h2 1

instance MinWidth StyleState where
  minWidth w = def & L.sizeReqW ?~ RangeSize w (2 * w) 1

instance MinHeight StyleState where
  minHeight h = def & L.sizeReqH ?~ RangeSize h (2 * h) 1

instance MaxWidth StyleState where
  maxWidth w = def & L.sizeReqW ?~ RangeSize 0 w 1

instance MaxHeight StyleState where
  maxHeight h = def & L.sizeReqH ?~ RangeSize 0 h 1

instance BgColor StyleState where
  bgColor col = def & L.bgColor ?~ col

instance FgColor StyleState where
  fgColor col = def & L.fgColor ?~ col

instance HlColor StyleState where
  hlColor col = def & L.hlColor ?~ col

instance TextFont TextStyle where
  textFont font = def & L.font ?~ font

instance TextFont StyleState where
  textFont font = def & L.text ?~ textFont font

instance TextSize TextStyle where
  textSize size = def & L.fontSize ?~ FontSize size

instance TextSize StyleState where
  textSize size = def & L.text ?~ textSize size

instance TextColor TextStyle where
  textColor col = def & L.fontColor ?~ col

instance TextColor StyleState where
  textColor col = def & L.text ?~ textColor col

instance TextLeft TextStyle where
  textLeft = textAlignH ALeft

instance TextLeft StyleState where
  textLeft = styleTextAlignH ALeft

instance TextCenter TextStyle where
  textCenter = textAlignH ACenter

instance TextCenter StyleState where
  textCenter = styleTextAlignH ACenter

instance TextRight TextStyle where
  textRight = textAlignH ARight

instance TextRight StyleState where
  textRight = styleTextAlignH ARight

instance TextTop TextStyle where
  textTop = textAlignV ATop

instance TextTop StyleState where
  textTop = styleTextAlignV ATop

instance TextMiddle TextStyle where
  textMiddle = textAlignV AMiddle

instance TextMiddle StyleState where
  textMiddle = styleTextAlignV AMiddle

instance TextBottom TextStyle where
  textBottom = textAlignV ABottom

instance TextBottom StyleState where
  textBottom = styleTextAlignV ABottom

instance Margin_ Margin where
  margin mar = Margin jm jm jm jm where
    jm = Just mar

instance Margin_ StyleState where
  margin mar = def & L.margin ?~ margin mar

instance MarginL Margin where
  marginL mar = def & L.left ?~ mar

instance MarginL StyleState where
  marginL mar = def & L.margin . non def . L.left ?~ mar

instance MarginR Margin where
  marginR mar = def & L.right ?~ mar

instance MarginR StyleState where
  marginR mar = def & L.margin . non def . L.right ?~ mar

instance MarginT Margin where
  marginT mar = def & L.top ?~ mar

instance MarginT StyleState where
  marginT mar = def & L.margin . non def . L.top ?~ mar

instance MarginB Margin where
  marginB mar = def & L.bottom ?~ mar

instance MarginB StyleState where
  marginB mar = def & L.margin . non def . L.bottom ?~ mar

instance Padding_ Padding where
  padding padd = Padding jp jp jp jp where
    jp = Just padd

instance Padding_ StyleState where
  padding padd = def & L.padding ?~ padding padd

instance PaddingL Padding where
  paddingL padd = def & L.left ?~ padd

instance PaddingL StyleState where
  paddingL padd = def & L.padding . non def . L.left ?~ padd

instance PaddingR Margin where
  paddingR padd = def & L.right ?~ padd

instance PaddingR StyleState where
  paddingR padd = def & L.padding . non def . L.right ?~ padd

instance PaddingT Margin where
  paddingT padd = def & L.top ?~ padd

instance PaddingT StyleState where
  paddingT padd = def & L.padding . non def . L.top ?~ padd

instance PaddingB Margin where
  paddingB padd = def & L.bottom ?~ padd

instance PaddingB StyleState where
  paddingB padd = def & L.padding . non def . L.bottom ?~ padd

instance Border_ Border where
  border w col = Border bs bs bs bs where
    bs =  Just (BorderSide w col)

instance Border_ StyleState where
  border w col = def & L.border ?~ border w col

instance BorderL Border where
  borderL w col = def & L.left ?~ BorderSide w col

instance BorderL StyleState where
  borderL w col = def & L.border . non def . L.left ?~ BorderSide w col

instance BorderR Border where
  borderR w col = def & L.right ?~ BorderSide w col

instance BorderR StyleState where
  borderR w col = def & L.border . non def . L.right ?~ BorderSide w col

instance BorderT Border where
  borderT w col = def & L.top ?~ BorderSide w col

instance BorderT StyleState where
  borderT w col = def & L.border . non def . L.top ?~ BorderSide w col

instance BorderB Border where
  borderB w col = def & L.bottom ?~ BorderSide w col

instance BorderB StyleState where
  borderB w col = def & L.border . non def . L.bottom ?~ BorderSide w col

instance Radius_ Radius where
  radius rad = Radius jrad jrad jrad jrad where
    jrad = Just $ radiusCorner rad

instance Radius_ StyleState where
  radius rad = def & L.radius ?~ radius rad

instance RadiusTL Radius where
  radiusTL rad = def & L.topLeft ?~ radiusCorner rad

instance RadiusTL StyleState where
  radiusTL rad = def & L.radius . non def . L.topLeft ?~ radiusCorner rad

instance RadiusTR Radius where
  radiusTR rad = def & L.topRight ?~ radiusCorner rad

instance RadiusTR StyleState where
  radiusTR rad = def & L.radius . non def . L.topRight ?~ radiusCorner rad

instance RadiusBL Radius where
  radiusBL rad = def & L.bottomLeft ?~ radiusCorner rad

instance RadiusBL StyleState where
  radiusBL rad = def & L.radius . non def . L.bottomLeft ?~ radiusCorner rad

instance RadiusBR Radius where
  radiusBR rad = def & L.bottomRight ?~ radiusCorner rad

instance RadiusBR StyleState where
  radiusBR rad = def & L.radius . non def . L.bottomRight ?~ radiusCorner rad

instance InnerRadius_ Radius where
  iradius rad = Radius jrad jrad jrad jrad where
    jrad = Just $ iradiusCorner rad

instance InnerRadius_ StyleState where
  iradius rad = def & L.radius ?~ iradius rad

instance InnerRadiusTL Radius where
  iradiusTL rad = def & L.topLeft ?~ iradiusCorner rad

instance InnerRadiusTL StyleState where
  iradiusTL rad = def & L.radius . non def . L.topLeft ?~ iradiusCorner rad

instance InnerRadiusTR Radius where
  iradiusTR rad = def & L.topRight ?~ iradiusCorner rad

instance InnerRadiusTR StyleState where
  iradiusTR rad = def & L.radius . non def . L.topRight ?~ iradiusCorner rad

instance InnerRadiusBL Radius where
  iradiusBL rad = def & L.bottomLeft ?~ iradiusCorner rad

instance InnerRadiusBL StyleState where
  iradiusBL rad = def & L.radius . non def . L.bottomLeft ?~ iradiusCorner rad

instance InnerRadiusBR Radius where
  iradiusBR rad = def & L.bottomRight ?~ iradiusCorner rad

instance InnerRadiusBR StyleState where
  iradiusBR rad = def & L.radius . non def . L.bottomRight ?~ iradiusCorner rad

-- Internal

radiusCorner :: Double -> RadiusCorner
radiusCorner rad = RadiusCorner RadiusBoth rad

iradiusCorner :: Double -> RadiusCorner
iradiusCorner rad = RadiusCorner RadiusInner rad

textAlignH :: AlignH -> TextStyle
textAlignH align = def & L.alignH ?~ align

textAlignV :: AlignV -> TextStyle
textAlignV align = def & L.alignV ?~ align

styleTextAlignH :: AlignH -> StyleState
styleTextAlignH align = def & L.text . non def . L.alignH ?~ align

styleTextAlignV :: AlignV -> StyleState
styleTextAlignV align = def & L.text . non def . L.alignV ?~ align
