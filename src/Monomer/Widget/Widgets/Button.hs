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
    singleGetSizeReq = getSizeReq,
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

  getSizeReq wenv widgetInst = sizeReq where
    theme = activeTheme wenv widgetInst
    style = activeStyle wenv widgetInst
    size = getTextSize wenv theme style (_btnLabel config)
    sizeReq = SizeReq size FlexibleSize StrictSize

  render renderer wenv widgetInst = do
    drawStyledBackground renderer renderArea style
    drawStyledText_ renderer renderArea style (_btnLabel config)
    where
      style = activeStyle wenv widgetInst
      renderArea = _wiRenderArea widgetInst
