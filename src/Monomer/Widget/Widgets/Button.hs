{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Button (button) where

import Control.Monad

import qualified Data.Text as T

import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Common.Types
import Monomer.Common.Util
import Monomer.Event.Types
import Monomer.Graphics.Drawing
import Monomer.Graphics.Types
import Monomer.Widget.BaseWidget
import Monomer.Widget.Types
import Monomer.Widget.Util

button :: (Monad m) => T.Text -> e -> WidgetInstance s e m
button label onClick = defaultWidgetInstance "button" (makeButton label onClick)

makeButton :: (Monad m) => T.Text -> e -> Widget s e m
makeButton label onClick = createWidget {
    _widgetHandleEvent = handleEvent,
    _widgetPreferredSize = preferredSize,
    _widgetRender = render
  }
  where
    handleEvent ctx evt app widgetInstance = case evt of
      Click (Point x y) _ status -> resultEvents events widgetInstance where
        isPressed = status == PressedBtn -- && inRect (_instanceViewport instance) (Point x y)
        events = if isPressed then [onClick] else []
      _ -> Nothing

    preferredSize renderer app widgetInstance = singleton sizeReq where
      Style{..} = _instanceStyle widgetInstance
      size = calcTextBounds renderer _textStyle label
      sizeReq = SizeReq size FlexibleSize FlexibleSize

    render renderer ts ctx app WidgetInstance{..} =
      do
        drawBgRect renderer _instanceRenderArea _instanceStyle
        drawText_ renderer _instanceRenderArea (_textStyle _instanceStyle) label
