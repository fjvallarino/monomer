{-# LANGUAGE FunctionalDependencies #-}

module Monomer.Core.Combinators where

import Control.Lens (ALens')
import Data.Text (Text)

import Monomer.Core.BasicTypes
import Monomer.Core.StyleTypes
import Monomer.Core.WidgetTypes
import Monomer.Graphics.Types

-- Lifecycle
class CmbMergeRequired t s | t -> s where
  mergeRequired :: (s -> s -> Bool) -> t

-- Input
class CmbValidInput t s | t -> s where
  validInput :: ALens' s Bool -> t

class CmbSelectOnFocus t where
  selectOnFocus :: Bool -> t

-- Caption
class CmbAcceptCaption t where
  acceptCaption :: Text -> t

class CmbCancelCaption t where
  cancelCaption :: Text -> t

class CmbCloseCaption t where
  closeCaption :: Text -> t

-- Numeric
class Num a => CmbMinValue t a | t -> a where
  minValue :: a -> t

class Num a => CmbMaxValue t a | t -> a where
  maxValue :: a -> t

class Num a => CmbDragRate t a | t -> a where
  dragRate :: a -> t

-- Container
class CmbIgnoreEmptyArea t where
  ignoreEmptyArea :: Bool -> t

-- Text
class CmbDecimals t where
  decimals :: Int -> t

class CmbMaxLength t where
  maxLength :: Int -> t

class CmbTextMode t where
  textSingleLine :: t
  textMultiLine :: t

class CmbTextOverflow t where
  textEllipsis :: t
  textClip :: t

class CmbTextTrim t where
  textTrim :: t
  textKeepSpaces :: t

class CmbSelectOnBlur t where
  selectOnBlur :: Bool -> t

-- Text style
class CmbTextFont t where
  textFont :: Font -> t

class CmbTextSize t where
  textSize :: Double -> t

class CmbTextColor t where
  textColor :: Color -> t

class CmbTextLeft t where
  textLeft :: t

class CmbTextCenter t where
  textCenter :: t

class CmbTextRight t where
  textRight :: t

class CmbTextTop t where
  textTop :: t

class CmbTextMiddle t where
  textMiddle :: t

class CmbTextBottom t where
  textBottom :: t

-- Events
class CmbOnFocus t e | t -> e  where
  onFocus :: e -> t

class CmbOnFocusReq t s | t -> s where
  onFocusReq :: WidgetRequest s -> t

class CmbOnBlur t e | t -> e  where
  onBlur :: e -> t

class CmbOnBlurReq t s | t -> s where
  onBlurReq :: WidgetRequest s -> t

class CmbOnClick t e | t -> e  where
  onClick :: e -> t

class CmbOnClickReq t s | t -> s where
  onClickReq :: WidgetRequest s -> t

class CmbOnClickEmpty t e | t -> e  where
  onClickEmpty :: e -> t

class CmbOnClickEmptyReq t s | t -> s where
  onClickEmptyReq :: WidgetRequest s -> t

class CmbOnChange t a e | t -> e where
  onChange :: (a -> e) -> t

class CmbOnChangeIdx t a e | t -> e where
  onChangeIdx :: (Int -> a -> e) -> t

class CmbOnChangeReq t s | t -> s where
  onChangeReq :: WidgetRequest s -> t

class CmbOnChangeIdxReq t s | t -> s where
  onChangeIdxReq :: (Int -> WidgetRequest s) -> t

class CmbOnLoadError t a e | t -> e where
  onLoadError :: (a -> e) -> t

-- Size
class CmbRangeWidth t where
  rangeWidth :: Double -> Double -> t

class CmbRangeHeight t where
  rangeHeight :: Double -> Double -> t

class CmbFlexWidth t where
  flexWidth :: Double -> t

class CmbFlexHeight t where
  flexHeight :: Double -> t

class CmbWidth t where
  width :: Double -> t

class CmbHeight t where
  height :: Double -> t

class CmbMinWidth t where
  minWidth :: Double -> t

class CmbMinHeight t where
  minHeight :: Double -> t

class CmbMaxWidth t where
  maxWidth :: Double -> t

class CmbMaxHeight t where
  maxHeight :: Double -> t

class CmbSizeReqW t where
  sizeReqW :: SizeReq -> t

class CmbSizeReqH t where
  sizeReqH :: SizeReq -> t

class CmbResizeFactor t where
  resizeFactor :: Double -> t

class CmbResizeFactorDim t where
  resizeFactorW :: Double -> t
  resizeFactorH :: Double -> t

-- Style
infixl 5 `style`
infixl 5 `hover`
infixl 5 `focus`
infixl 5 `disabled`

class CmbStyle t where
  style :: t -> [StyleState] -> t

class CmbHover t where
  hover :: t -> [StyleState] -> t

class CmbFocus t where
  focus :: t -> [StyleState] -> t

class CmbDisabled t where
  disabled :: t -> [StyleState] -> t

class CmbBgColor t where
  bgColor :: Color -> t

class CmbFgColor t where
  fgColor :: Color -> t

class CmbHlColor t where
  hlColor :: Color -> t

class CmbTransparency t where
  transparency :: Double -> t

-- Item List
class CmbItemListStyle t s | t -> s where
  itemListStyle :: s -> t

class CmbItemNormalStyle t s | t -> s where
  itemNormalStyle :: s -> t

class CmbItemHoverStyle t s | t -> s where
  itemHoverStyle :: s -> t

class CmbItemSelectedStyle t s | t -> s where
  itemSelectedStyle :: s -> t

-- Align
class CmbAlignLeft t where
  alignLeft :: t

class CmbAlignCenter t where
  alignCenter :: t

class CmbAlignRight t where
  alignRight :: t

class CmbAlignTop t where
  alignTop :: t

class CmbAlignMiddle t where
  alignMiddle :: t

class CmbAlignBottom t where
  alignBottom :: t

-- Margin
class CmbMargin t where
  margin :: Double -> t

class CmbMarginL t where
  marginL :: Double -> t

class CmbMarginR t where
  marginR :: Double -> t

class CmbMarginT t where
  marginT :: Double -> t

class CmbMarginB t where
  marginB :: Double -> t

-- Padding
class CmbPadding t where
  padding :: Double -> t

class CmbPaddingL t where
  paddingL :: Double -> t

class CmbPaddingR t where
  paddingR :: Double -> t

class CmbPaddingT t where
  paddingT :: Double -> t

class CmbPaddingB t where
  paddingB :: Double -> t

-- Border
class CmbBorder t where
  border :: Double -> Color -> t

class CmbBorderL t where
  borderL :: Double -> Color -> t

class CmbBorderR t where
  borderR :: Double -> Color -> t

class CmbBorderT t where
  borderT :: Double -> Color -> t

class CmbBorderB t where
  borderB :: Double -> Color -> t

-- Radius
class CmbRadius t where
  radius :: Double -> t

class CmbRadiusTL t where
  radiusTL :: Double -> t

class CmbRadiusTR t where
  radiusTR :: Double -> t

class CmbRadiusBL t where
  radiusBL :: Double -> t

class CmbRadiusBR t where
  radiusBR :: Double -> t

-- Inner Radius
class CmbInnerRadius t where
  iradius :: Double -> t

class CmbInnerRadiusTL t where
  iradiusTL :: Double -> t

class CmbInnerRadiusTR t where
  iradiusTR :: Double -> t

class CmbInnerRadiusBL t where
  iradiusBL :: Double -> t

class CmbInnerRadiusBR t where
  iradiusBR :: Double -> t
