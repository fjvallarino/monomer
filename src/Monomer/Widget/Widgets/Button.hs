{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Button (
  ButtonConfig(..),
  button,
  button_,
  buttonConfig
) where

import Control.Monad
import Data.Default
import Data.Text (Text)

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Event.Types
import Monomer.Graphics.Drawing
import Monomer.Graphics.Types
import Monomer.Widget.BaseSingle
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
makeButton config = widget where
  widget = createSingle def {
    singleHandleEvent = handleEvent,
    singleUpdateSizeReq = updateSizeReq,
    singleRender = render
  }

  handleEvent wenv ctx evt widgetInst = case evt of
    Click p _
      | pointInViewport p widgetInst -> Just result
      where
        requests = _btnOnChangeReq config
        events = _btnOnChange config
        result = resultReqsEvents requests events widgetInst
    _ -> Nothing

  updateSizeReq wenv widgetInst = newInst where
    Style{..} = _wiStyle widgetInst
    size = getTextBounds wenv _styleText (_btnLabel config)
    sizeReq = SizeReq size FlexibleSize StrictSize
    newInst = widgetInst {
      _wiSizeReq = sizeReq
    }

  render renderer wenv widgetInst@WidgetInstance{..} = do
    drawWidgetBg renderer wenv widgetInst
    drawStyledText_ renderer _wiRenderArea _wiStyle (_btnLabel config)
