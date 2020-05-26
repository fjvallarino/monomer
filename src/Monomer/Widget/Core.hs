{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Monomer.Widget.Core where

import Control.Applicative
import Control.Monad

import Monomer.Common.Style
import Monomer.Widget.Types

cascadeStyle :: (Monad m) => Style -> WidgetInstance s e m -> WidgetInstance s e m
cascadeStyle parentStyle widgetInstance@WidgetInstance{..} = newNode where
  newNode = widgetInstance { _instanceStyle = newStyle, _instanceChildren = newChildren }
  newChildren = fmap (cascadeStyle newStyle) _instanceChildren
  newStyle = Style {
    _fixedWidth = _fixedWidth _instanceStyle,
    _fixedHeight = _fixedHeight _instanceStyle,
    _padding = _padding parentStyle <> _padding _instanceStyle,
    _bgRadius = _bgRadius parentStyle <> _bgRadius _instanceStyle,
    _bgColor = _bgColor parentStyle <|> _bgColor _instanceStyle,
    _border = _border parentStyle <> _border _instanceStyle,
    _textStyle = _textStyle parentStyle <> _textStyle _instanceStyle
  }
