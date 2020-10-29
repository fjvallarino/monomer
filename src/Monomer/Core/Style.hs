module Monomer.Core.Style (
  module Monomer.Core.StyleTypes,
  module Monomer.Core.ThemeTypes,
  paddingH,
  paddingV
) where

import Control.Lens ((&), (?~), non)
import Data.Default

import Monomer.Core.Combinators
import Monomer.Core.StyleTypes
import Monomer.Core.ThemeTypes
import Monomer.Graphics.Types

import qualified Monomer.Core.Lens as L

paddingH :: (Semigroup a, PaddingL a, PaddingR a) => Double -> a
paddingH p = paddingL p <> paddingR p

paddingV :: (Semigroup a, PaddingT a, PaddingB a) => Double -> a
paddingV p = paddingT p <> paddingB p

-- Size
instance Width SizeReq where
  width w = FixedSize w

instance Height SizeReq where
  height h = FixedSize h

instance FlexWidth SizeReq where
  flexWidth w = FlexSize w 1

instance FlexHeight SizeReq where
  flexHeight h = FlexSize h 1

instance RangeWidth SizeReq where
  rangeWidth w1 w2 = RangeSize w1 w2 1

instance RangeHeight SizeReq where
  rangeHeight h1 h2 = RangeSize h1 h2 1

instance MinWidth SizeReq where
  minWidth w = RangeSize w (2 * w) 1

instance MinHeight SizeReq where
  minHeight h = RangeSize h (2 * h) 1

instance MaxWidth SizeReq where
  maxWidth w = RangeSize 0 w 1

instance MaxHeight SizeReq where
  maxHeight h = RangeSize 0 h 1

-- Text
instance TextFont TextStyle where
  textFont font = def & L.font ?~ font

instance TextSize TextStyle where
  textSize size = def & L.fontSize ?~ FontSize size

instance TextColor TextStyle where
  textColor col = def & L.fontColor ?~ col

instance TextLeft TextStyle where
  textLeft = textAlignH ALeft

instance TextCenter TextStyle where
  textCenter = textAlignH ACenter

instance TextRight TextStyle where
  textRight = textAlignH ARight

instance TextTop TextStyle where
  textTop = textAlignV ATop

instance TextMiddle TextStyle where
  textMiddle = textAlignV AMiddle

instance TextBottom TextStyle where
  textBottom = textAlignV ABottom

-- Padding

instance Padding_ Padding where
  padding padd = Padding jp jp jp jp where
    jp = Just padd

instance PaddingL Padding where
  paddingL padd = def & L.left ?~ padd

instance PaddingR Padding where
  paddingR padd = def & L.right ?~ padd

instance PaddingT Padding where
  paddingT padd = def & L.top ?~ padd

instance PaddingB Padding where
  paddingB padd = def & L.bottom ?~ padd

-- Border

instance Border_ Border where
  border w col = Border bs bs bs bs where
    bs =  Just (BorderSide w col)

instance BorderL Border where
  borderL w col = def & L.left ?~ BorderSide w col

instance BorderR Border where
  borderR w col = def & L.right ?~ BorderSide w col

instance BorderT Border where
  borderT w col = def & L.top ?~ BorderSide w col

instance BorderB Border where
  borderB w col = def & L.bottom ?~ BorderSide w col

-- Radius

instance Radius_ Radius where
  radius rad = Radius jrad jrad jrad jrad where
    jrad = Just $ radiusCorner rad

instance RadiusTL Radius where
  radiusTL rad = def & L.topLeft ?~ radiusCorner rad

instance RadiusTR Radius where
  radiusTR rad = def & L.topRight ?~ radiusCorner rad

instance RadiusBL Radius where
  radiusBL rad = def & L.bottomLeft ?~ radiusCorner rad

instance RadiusBR Radius where
  radiusBR rad = def & L.bottomRight ?~ radiusCorner rad

-- Inner radius

instance InnerRadius_ Radius where
  iradius rad = Radius jrad jrad jrad jrad where
    jrad = Just $ iradiusCorner rad

instance InnerRadiusTL Radius where
  iradiusTL rad = def & L.topLeft ?~ iradiusCorner rad

instance InnerRadiusTR Radius where
  iradiusTR rad = def & L.topRight ?~ iradiusCorner rad

instance InnerRadiusBL Radius where
  iradiusBL rad = def & L.bottomLeft ?~ iradiusCorner rad

instance InnerRadiusBR Radius where
  iradiusBR rad = def & L.bottomRight ?~ iradiusCorner rad

--
-- StyleState instances
--

-- Size
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

-- Color

instance BgColor StyleState where
  bgColor col = def & L.bgColor ?~ col

instance FgColor StyleState where
  fgColor col = def & L.fgColor ?~ col

instance HlColor StyleState where
  hlColor col = def & L.hlColor ?~ col

-- Text
instance TextFont StyleState where
  textFont font = def & L.text ?~ textFont font

instance TextSize StyleState where
  textSize size = def & L.text ?~ textSize size

instance TextColor StyleState where
  textColor col = def & L.text ?~ textColor col

instance TextLeft StyleState where
  textLeft = styleTextAlignH ALeft

instance TextCenter StyleState where
  textCenter = styleTextAlignH ACenter

instance TextRight StyleState where
  textRight = styleTextAlignH ARight

instance TextTop StyleState where
  textTop = styleTextAlignV ATop

instance TextMiddle StyleState where
  textMiddle = styleTextAlignV AMiddle

instance TextBottom StyleState where
  textBottom = styleTextAlignV ABottom

-- Padding
instance Padding_ StyleState where
  padding padd = def & L.padding ?~ padding padd

instance PaddingL StyleState where
  paddingL padd = def & L.padding . non def . L.left ?~ padd

instance PaddingR StyleState where
  paddingR padd = def & L.padding . non def . L.right ?~ padd

instance PaddingT StyleState where
  paddingT padd = def & L.padding . non def . L.top ?~ padd

instance PaddingB StyleState where
  paddingB padd = def & L.padding . non def . L.bottom ?~ padd

-- Border
instance Border_ StyleState where
  border w col = def & L.border ?~ border w col

instance BorderL StyleState where
  borderL w col = def & L.border . non def . L.left ?~ BorderSide w col

instance BorderR StyleState where
  borderR w col = def & L.border . non def . L.right ?~ BorderSide w col

instance BorderT StyleState where
  borderT w col = def & L.border . non def . L.top ?~ BorderSide w col

instance BorderB StyleState where
  borderB w col = def & L.border . non def . L.bottom ?~ BorderSide w col

-- Radius
instance Radius_ StyleState where
  radius rad = def & L.radius ?~ radius rad

instance RadiusTL StyleState where
  radiusTL rad = def & L.radius . non def . L.topLeft ?~ radiusCorner rad

instance RadiusTR StyleState where
  radiusTR rad = def & L.radius . non def . L.topRight ?~ radiusCorner rad

instance RadiusBL StyleState where
  radiusBL rad = def & L.radius . non def . L.bottomLeft ?~ radiusCorner rad

instance RadiusBR StyleState where
  radiusBR rad = def & L.radius . non def . L.bottomRight ?~ radiusCorner rad

-- Inner radius
instance InnerRadius_ StyleState where
  iradius rad = def & L.radius ?~ iradius rad

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

textAlignH :: AlignH -> TextStyle
textAlignH align = def & L.alignH ?~ align

textAlignV :: AlignV -> TextStyle
textAlignV align = def & L.alignV ?~ align

styleTextAlignH :: AlignH -> StyleState
styleTextAlignH align = def & L.text . non def . L.alignH ?~ align

styleTextAlignV :: AlignV -> StyleState
styleTextAlignV align = def & L.text . non def . L.alignV ?~ align
