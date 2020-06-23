{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Button (button) where

import Control.Monad
import Data.Text (Text)

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Event.Types
import Monomer.Graphics.Drawing
import Monomer.Graphics.Types
import Monomer.Widget.BaseWidget
import Monomer.Widget.Types
import Monomer.Widget.Util

button :: Text -> e -> WidgetInstance s e
button label onClick = defaultWidgetInstance "button" (makeButton label onClick)

makeButton :: Text -> e -> Widget s e
makeButton label onClick = createWidget {
    _widgetHandleEvent = handleEvent,
    _widgetPreferredSize = preferredSize,
    _widgetRender = render
  }
  where
    handleEvent wctx ctx evt widgetInstance = case evt of
      Click (Point x y) _ status -> Just $ resultEvents events widgetInstance where
        isPressed = status == PressedBtn -- && inRect (_instanceViewport instance) (Point x y)
        events = if isPressed then [onClick] else []
      _ -> Nothing

    preferredSize renderer wctx widgetInstance = singleNode sizeReq where
      Style{..} = _instanceStyle widgetInstance
      size = calcTextBounds renderer _textStyle label
      sizeReq = SizeReq size FlexibleSize FlexibleSize

    render renderer wctx ctx WidgetInstance{..} =
      do
        drawBgRect renderer _instanceRenderArea _instanceStyle
        drawText_ renderer _instanceRenderArea (_textStyle _instanceStyle) label
