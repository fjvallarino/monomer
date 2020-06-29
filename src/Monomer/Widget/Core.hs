{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Core where

import Control.Applicative
import Control.Monad

import Monomer.Common.Style
import Monomer.Widget.Types

cascadeStyle :: Style -> WidgetInstance s e -> WidgetInstance s e
cascadeStyle parentStyle widgetInstance@WidgetInstance{..} = newNode where
  newNode = widgetInstance { _instanceStyle = newStyle, _instanceChildren = newChildren }
  newChildren = fmap (cascadeStyle newStyle) _instanceChildren
  newStyle = Style {
    _styleWidth = _styleWidth _instanceStyle,
    _styleHeight = _styleHeight _instanceStyle,
    _stylePadding = _stylePadding parentStyle <> _stylePadding _instanceStyle,
    _styleBorder = _styleBorder parentStyle <> _styleBorder _instanceStyle,
    _styleRadius = _styleRadius parentStyle <> _styleRadius _instanceStyle,
    _styleBgColor = _styleBgColor parentStyle <|> _styleBgColor _instanceStyle,
    _styleFgColor = _styleFgColor parentStyle <|> _styleFgColor _instanceStyle,
    _styleBgHoverColor = _styleBgHoverColor parentStyle <|> _styleBgHoverColor _instanceStyle,
    _styleFgHoverColor = _styleFgHoverColor parentStyle <|> _styleFgHoverColor _instanceStyle,
    _styleText = _styleText parentStyle <> _styleText _instanceStyle
  }
