{-# LANGUAGE FunctionalDependencies #-}

module Monomer.Widget.Widgets.WidgetCombinators where

import Control.Lens (ALens')
import Data.Text (Text)

import Monomer.Common.Style
import Monomer.Graphics.Types
import Monomer.Widget.Types

class ValidInput t s where
  validInput :: ALens' s Bool -> t

class Caption t where
  caption :: Text -> t

class SelectOnFocus t where
  selectOnFocus :: Bool -> t

class MaxLength t where
  maxLength :: Int -> t

class Decimals t where
  decimals :: Int -> t

class Num a => MinValue t a | t -> a where
  minValue :: a -> t

class Num a => MaxValue t a | t -> a where
  maxValue :: a -> t

class ResizeFactor t where
  resizeFactor :: Double -> t

class OnClick t e | t -> e  where
  onClick :: e -> t

class OnClickReq t s | t -> s where
  onClickReq :: WidgetRequest s -> t

class OnChange t a e | t -> e where
  onChange :: (a -> e) -> t

class OnChangeIdx t a e | t -> e where
  onChangeIdx :: (Int -> a -> e) -> t

class OnChangeReq t s | t -> s where
  onChangeReq :: WidgetRequest s -> t

class OnChangeReqIdx t s | t -> s where
  onChangeReqIdx :: (Int -> WidgetRequest s) -> t

class SelectedStyle t where
  selectedStyle :: StyleState -> t

class HighlightedStyle t where
  highlightedStyle :: StyleState -> t

class HoverStyle t where
  hoverStyle :: StyleState -> t

class HighlightedColor t where
  highlightedColor :: Color -> t

class Transparency t where
  transparency :: Double -> t

class OnTextOverflow t where
  textEllipsis :: t
  textClip :: t
