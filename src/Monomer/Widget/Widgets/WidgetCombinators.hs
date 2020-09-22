{-# LANGUAGE FunctionalDependencies #-}

module Monomer.Widget.Widgets.WidgetCombinators where

import Control.Lens (ALens')

import Monomer.Widget.Types

class ValidInput t s where
  validInput :: ALens' s Bool -> t

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

class OnChange t a e where
  onChange :: (a -> e) -> t

class OnChangeReq t s where
  onChangeReq :: WidgetRequest s -> t
