{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Button (
  ButtonConfig(..),
  button,
  button_,
  buttonConfig
) where

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

data ButtonConfig s e = ButtonConfig {
  _btnLabel :: Text,
  _btnOnChange :: [e],
  _btnOnChangeReq :: [WidgetRequest s]
}

buttonConfig :: e -> Text -> ButtonConfig s e
buttonConfig onClick label = ButtonConfig label [onClick] []

button :: e -> Text -> WidgetInstance s e
button onClick label = button_ config where
  config = buttonConfig onClick label

button_ :: ButtonConfig s e -> WidgetInstance s e
button_ config = defaultWidgetInstance "button" (makeButton config)

makeButton :: ButtonConfig s e -> Widget s e
makeButton config = createWidget {
    _widgetHandleEvent = handleEvent,
    _widgetPreferredSize = preferredSize,
    _widgetRender = render
  }
  where
    handleEvent wctx ctx evt widgetInstance = case evt of
      Click (Point x y) _ -> Just $ resultReqsEvents requests events widgetInstance where
        requests = _btnOnChangeReq config
        events = _btnOnChange config
      _ -> Nothing

    preferredSize wctx widgetInstance = singleNode sizeReq where
      Style{..} = _instanceStyle widgetInstance
      size = getTextBounds wctx _styleText (_btnLabel config)
      sizeReq = SizeReq size FlexibleSize FlexibleSize

    render renderer wctx ctx WidgetInstance{..} =
      do
        drawStyledBackground renderer _instanceRenderArea _instanceStyle
        drawStyledText_ renderer _instanceRenderArea _instanceStyle (_btnLabel config)
