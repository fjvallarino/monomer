{-# LANGUAGE FunctionalDependencies #-}

module Monomer.Core.Combinators where

import Control.Lens (ALens')
import Data.Text (Text)

import Monomer.Core.BasicTypes
import Monomer.Core.StyleTypes
import Monomer.Core.WidgetTypes
import Monomer.Graphics.Types

-- Config
class WindowSize t s | t -> s where
  windowSize :: s -> t

-- Input
class ValidInput t s where
  validInput :: ALens' s Bool -> t

class SelectOnFocus t where
  selectOnFocus :: Bool -> t

-- Caption
class AcceptCaption t where
  acceptCaption :: Text -> t

class CancelCaption t where
  cancelCaption :: Text -> t

class CloseCaption t where
  closeCaption :: Text -> t

-- Numeric
class Num a => MinValue t a | t -> a where
  minValue :: a -> t

class Num a => MaxValue t a | t -> a where
  maxValue :: a -> t

-- Container
class IgnoreEmptyClick t where
  ignoreEmptyClick :: Bool -> t

-- Text
class Decimals t where
  decimals :: Int -> t

class MaxLength t where
  maxLength :: Int -> t

class OnTextOverflow t where
  textEllipsis :: t
  textClip :: t

class SelectOnBlur t where
  selectOnBlur :: Bool -> t

-- Text style
class TextFont t where
  textFont :: Font -> t

class TextSize t where
  textSize :: Double -> t

class TextColor t where
  textColor :: Color -> t

class TextLeft t where
  textLeft :: t

class TextCenter t where
  textCenter :: t

class TextRight t where
  textRight :: t

class TextTop t where
  textTop :: t

class TextMiddle t where
  textMiddle :: t

class TextBottom t where
  textBottom :: t

-- Events
class OnBlur t e | t -> e  where
  onBlur :: e -> t

class OnBlurReq t s | t -> s where
  onBlurReq :: WidgetRequest s -> t

class OnClick t e | t -> e  where
  onClick :: e -> t

class OnClickReq t s | t -> s where
  onClickReq :: WidgetRequest s -> t

class OnClickEmpty t e | t -> e  where
  onClickEmpty :: e -> t

class OnClickEmptyReq t s | t -> s where
  onClickEmptyReq :: WidgetRequest s -> t

class OnChange t a e | t -> e where
  onChange :: (a -> e) -> t

class OnChangeIdx t a e | t -> e where
  onChangeIdx :: (Int -> a -> e) -> t

class OnChangeReq t s | t -> s where
  onChangeReq :: WidgetRequest s -> t

class OnChangeIdxReq t s | t -> s where
  onChangeIdxReq :: (Int -> WidgetRequest s) -> t

class OnLoadError t a e | t -> e where
  onLoadError :: (a -> e) -> t

-- Size
class RangeWidth t where
  rangeWidth :: Double -> Double -> t

class RangeHeight t where
  rangeHeight :: Double -> Double -> t

class FlexWidth t where
  flexWidth :: Double -> t

class FlexHeight t where
  flexHeight :: Double -> t

class Width t where
  width :: Double -> t

class Height t where
  height :: Double -> t

class MinWidth t where
  minWidth :: Double -> t

class MinHeight t where
  minHeight :: Double -> t

class MaxWidth t where
  maxWidth :: Double -> t

class MaxHeight t where
  maxHeight :: Double -> t

class ResizeFactor t where
  resizeFactor :: Double -> t

-- Style
infixl 5 `style`
infixl 5 `hover`
infixl 5 `focus`
infixl 5 `disabled`

class Style_ t where
  style :: t -> [StyleState] -> t

class Hover_ t where
  hover :: t -> [StyleState] -> t

class Focus_ t where
  focus :: t -> [StyleState] -> t

class Disabled_ t where
  disabled :: t -> [StyleState] -> t

class BgColor t where
  bgColor :: Color -> t

class FgColor t where
  fgColor :: Color -> t

class HlColor t where
  hlColor :: Color -> t

class Transparency t where
  transparency :: Double -> t

-- Item List
class ItemListStyle t s | t -> s where
  itemListStyle :: s -> t

class ItemNormalStyle t s | t -> s where
  itemNormalStyle :: s -> t

class ItemHoverStyle t s | t -> s where
  itemHoverStyle :: s -> t

class ItemSelectedStyle t s | t -> s where
  itemSelectedStyle :: s -> t

-- Align
class AlignLeft t where
  alignLeft :: t

class AlignCenter t where
  alignCenter :: t

class AlignRight t where
  alignRight :: t

class AlignTop t where
  alignTop :: t

class AlignMiddle t where
  alignMiddle :: t

class AlignBottom t where
  alignBottom :: t

-- Margin
class Margin_ t where
  margin :: Double -> t

class MarginL t where
  marginL :: Double -> t

class MarginR t where
  marginR :: Double -> t

class MarginT t where
  marginT :: Double -> t

class MarginB t where
  marginB :: Double -> t

-- Padding
class Padding_ t where
  padding :: Double -> t

class PaddingL t where
  paddingL :: Double -> t

class PaddingR t where
  paddingR :: Double -> t

class PaddingT t where
  paddingT :: Double -> t

class PaddingB t where
  paddingB :: Double -> t

-- Border
class Border_ t where
  border :: Double -> Color -> t

class BorderL t where
  borderL :: Double -> Color -> t

class BorderR t where
  borderR :: Double -> Color -> t

class BorderT t where
  borderT :: Double -> Color -> t

class BorderB t where
  borderB :: Double -> Color -> t

-- Radius
class Radius_ t where
  radius :: Double -> t

class RadiusTL t where
  radiusTL :: Double -> t

class RadiusTR t where
  radiusTR :: Double -> t

class RadiusBL t where
  radiusBL :: Double -> t

class RadiusBR t where
  radiusBR :: Double -> t

-- Inner Radius
class InnerRadius_ t where
  iradius :: Double -> t

class InnerRadiusTL t where
  iradiusTL :: Double -> t

class InnerRadiusTR t where
  iradiusTR :: Double -> t

class InnerRadiusBL t where
  iradiusBL :: Double -> t

class InnerRadiusBR t where
  iradiusBR :: Double -> t
