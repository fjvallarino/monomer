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

paddingH :: (Semigroup a, CmbPaddingL a, CmbPaddingR a) => Double -> a
paddingH p = paddingL p <> paddingR p

paddingV :: (Semigroup a, CmbPaddingT a, CmbPaddingB a) => Double -> a
paddingV p = paddingT p <> paddingB p

-- Size
instance CmbWidth SizeReq where
  width w = FixedSize w

instance CmbHeight SizeReq where
  height h = FixedSize h

instance CmbFlexWidth SizeReq where
  flexWidth w = FlexSize w 1

instance CmbFlexHeight SizeReq where
  flexHeight h = FlexSize h 1

instance CmbRangeWidth SizeReq where
  rangeWidth w1 w2 = RangeSize w1 w2 1

instance CmbRangeHeight SizeReq where
  rangeHeight h1 h2 = RangeSize h1 h2 1

instance CmbMinWidth SizeReq where
  minWidth w = MinSize w 1

instance CmbMinHeight SizeReq where
  minHeight h = MinSize h 1

instance CmbMaxWidth SizeReq where
  maxWidth w = MaxSize w 1

instance CmbMaxHeight SizeReq where
  maxHeight h = MaxSize h 1

-- Text
instance CmbTextFont TextStyle where
  textFont font = def & L.font ?~ font

instance CmbTextSize TextStyle where
  textSize size = def & L.fontSize ?~ FontSize size

instance CmbTextColor TextStyle where
  textColor col = def & L.fontColor ?~ col

instance CmbTextLeft TextStyle where
  textLeft = textAlignH ATLeft

instance CmbTextCenter TextStyle where
  textCenter = textAlignH ATCenter

instance CmbTextRight TextStyle where
  textRight = textAlignH ATRight

instance CmbTextTop TextStyle where
  textTop = textAlignV ATTop

instance CmbTextMiddle TextStyle where
  textMiddle = textAlignV ATMiddle

instance CmbTextBottom TextStyle where
  textBottom = textAlignV ATBottom

instance CmbTextBaseline TextStyle where
  textBaseline = textAlignV ATBaseline

-- Padding

instance CmbPadding Padding where
  padding padd = Padding jp jp jp jp where
    jp = Just padd

instance CmbPaddingL Padding where
  paddingL padd = def & L.left ?~ padd

instance CmbPaddingR Padding where
  paddingR padd = def & L.right ?~ padd

instance CmbPaddingT Padding where
  paddingT padd = def & L.top ?~ padd

instance CmbPaddingB Padding where
  paddingB padd = def & L.bottom ?~ padd

-- Border

instance CmbBorder Border where
  border w col = Border bs bs bs bs where
    bs =  Just (BorderSide w col)

instance CmbBorderL Border where
  borderL w col = def & L.left ?~ BorderSide w col

instance CmbBorderR Border where
  borderR w col = def & L.right ?~ BorderSide w col

instance CmbBorderT Border where
  borderT w col = def & L.top ?~ BorderSide w col

instance CmbBorderB Border where
  borderB w col = def & L.bottom ?~ BorderSide w col

-- Radius

instance CmbRadius Radius where
  radius rad = Radius jrad jrad jrad jrad where
    jrad = Just $ radiusCorner rad

instance CmbRadiusTL Radius where
  radiusTL rad = def & L.topLeft ?~ radiusCorner rad

instance CmbRadiusTR Radius where
  radiusTR rad = def & L.topRight ?~ radiusCorner rad

instance CmbRadiusBL Radius where
  radiusBL rad = def & L.bottomLeft ?~ radiusCorner rad

instance CmbRadiusBR Radius where
  radiusBR rad = def & L.bottomRight ?~ radiusCorner rad

-- Inner radius

instance CmbInnerRadius Radius where
  iradius rad = Radius jrad jrad jrad jrad where
    jrad = Just $ iradiusCorner rad

instance CmbInnerRadiusTL Radius where
  iradiusTL rad = def & L.topLeft ?~ iradiusCorner rad

instance CmbInnerRadiusTR Radius where
  iradiusTR rad = def & L.topRight ?~ iradiusCorner rad

instance CmbInnerRadiusBL Radius where
  iradiusBL rad = def & L.bottomLeft ?~ iradiusCorner rad

instance CmbInnerRadiusBR Radius where
  iradiusBR rad = def & L.bottomRight ?~ iradiusCorner rad

--
-- StyleState instances
--

-- Size
instance CmbWidth StyleState where
  width w = def & L.sizeReqW ?~ FixedSize w

instance CmbHeight StyleState where
  height h = def & L.sizeReqH ?~ FixedSize h

instance CmbFlexWidth StyleState where
  flexWidth w = def & L.sizeReqW ?~ FlexSize w 1

instance CmbFlexHeight StyleState where
  flexHeight h = def & L.sizeReqH ?~ FlexSize h 1

instance CmbRangeWidth StyleState where
  rangeWidth w1 w2 = def & L.sizeReqW ?~ RangeSize w1 w2 1

instance CmbRangeHeight StyleState where
  rangeHeight h1 h2 = def & L.sizeReqH ?~ RangeSize h1 h2 1

instance CmbMinWidth StyleState where
  minWidth w = def & L.sizeReqW ?~ MinSize w 1

instance CmbMinHeight StyleState where
  minHeight h = def & L.sizeReqH ?~ MinSize h 1

instance CmbMaxWidth StyleState where
  maxWidth w = def & L.sizeReqW ?~ MaxSize w 1

instance CmbMaxHeight StyleState where
  maxHeight h = def & L.sizeReqH ?~ MaxSize h 1

instance CmbSizeReqW StyleState where
  sizeReqW srW = def & L.sizeReqW ?~ srW

instance CmbSizeReqH StyleState where
  sizeReqH srH = def & L.sizeReqH ?~ srH

-- Color

instance CmbBgColor StyleState where
  bgColor col = def & L.bgColor ?~ col

instance CmbFgColor StyleState where
  fgColor col = def & L.fgColor ?~ col

instance CmbHlColor StyleState where
  hlColor col = def & L.hlColor ?~ col

-- Cursor

instance CmbCursorIcon StyleState where
  cursorIcon icon = def & L.cursorIcon ?~ icon

-- Text
instance CmbTextFont StyleState where
  textFont font = def & L.text ?~ textFont font

instance CmbTextSize StyleState where
  textSize size = def & L.text ?~ textSize size

instance CmbTextColor StyleState where
  textColor col = def & L.text ?~ textColor col

instance CmbTextLeft StyleState where
  textLeft = styleTextAlignH ATLeft

instance CmbTextCenter StyleState where
  textCenter = styleTextAlignH ATCenter

instance CmbTextRight StyleState where
  textRight = styleTextAlignH ATRight

instance CmbTextTop StyleState where
  textTop = styleTextAlignV ATTop

instance CmbTextMiddle StyleState where
  textMiddle = styleTextAlignV ATMiddle

instance CmbTextBottom StyleState where
  textBottom = styleTextAlignV ATBottom

instance CmbTextBaseline StyleState where
  textBaseline = styleTextAlignV ATBaseline

-- Padding
instance CmbPadding StyleState where
  padding padd = def & L.padding ?~ padding padd

instance CmbPaddingL StyleState where
  paddingL padd = def & L.padding . non def . L.left ?~ padd

instance CmbPaddingR StyleState where
  paddingR padd = def & L.padding . non def . L.right ?~ padd

instance CmbPaddingT StyleState where
  paddingT padd = def & L.padding . non def . L.top ?~ padd

instance CmbPaddingB StyleState where
  paddingB padd = def & L.padding . non def . L.bottom ?~ padd

-- Border
instance CmbBorder StyleState where
  border w col = def & L.border ?~ border w col

instance CmbBorderL StyleState where
  borderL w col = def & L.border . non def . L.left ?~ BorderSide w col

instance CmbBorderR StyleState where
  borderR w col = def & L.border . non def . L.right ?~ BorderSide w col

instance CmbBorderT StyleState where
  borderT w col = def & L.border . non def . L.top ?~ BorderSide w col

instance CmbBorderB StyleState where
  borderB w col = def & L.border . non def . L.bottom ?~ BorderSide w col

-- Radius
instance CmbRadius StyleState where
  radius rad = def & L.radius ?~ radius rad

instance CmbRadiusTL StyleState where
  radiusTL rad = def & L.radius . non def . L.topLeft ?~ radiusCorner rad

instance CmbRadiusTR StyleState where
  radiusTR rad = def & L.radius . non def . L.topRight ?~ radiusCorner rad

instance CmbRadiusBL StyleState where
  radiusBL rad = def & L.radius . non def . L.bottomLeft ?~ radiusCorner rad

instance CmbRadiusBR StyleState where
  radiusBR rad = def & L.radius . non def . L.bottomRight ?~ radiusCorner rad

-- Inner radius
instance CmbInnerRadius StyleState where
  iradius rad = def & L.radius ?~ iradius rad

instance CmbInnerRadiusTL StyleState where
  iradiusTL rad = def & L.radius . non def . L.topLeft ?~ iradiusCorner rad

instance CmbInnerRadiusTR StyleState where
  iradiusTR rad = def & L.radius . non def . L.topRight ?~ iradiusCorner rad

instance CmbInnerRadiusBL StyleState where
  iradiusBL rad = def & L.radius . non def . L.bottomLeft ?~ iradiusCorner rad

instance CmbInnerRadiusBR StyleState where
  iradiusBR rad = def & L.radius . non def . L.bottomRight ?~ iradiusCorner rad

-- Internal

radiusCorner :: Double -> RadiusCorner
radiusCorner rad = RadiusCorner RadiusBoth rad

iradiusCorner :: Double -> RadiusCorner
iradiusCorner rad = RadiusCorner RadiusInner rad

textAlignH :: AlignTH -> TextStyle
textAlignH align = def & L.alignH ?~ align

textAlignV :: AlignTV -> TextStyle
textAlignV align = def & L.alignV ?~ align

styleTextAlignH :: AlignTH -> StyleState
styleTextAlignH align = def & L.text . non def . L.alignH ?~ align

styleTextAlignV :: AlignTV -> StyleState
styleTextAlignV align = def & L.text . non def . L.alignV ?~ align
