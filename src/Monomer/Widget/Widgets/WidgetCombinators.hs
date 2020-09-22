{-# LANGUAGE FunctionalDependencies #-}

module Monomer.Widget.Widgets.WidgetCombinators where

import Control.Lens (ALens')
import Data.Text (Text)

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

class OnClick t e | t -> e  where
  onClick :: e -> t

class OnClickReq t s | t -> s where
  onClickReq :: WidgetRequest s -> t

class OnChange t a e | t -> e where
  onChange :: (a -> e) -> t

class OnChangeReq t s | t -> s where
  onChangeReq :: WidgetRequest s -> t
