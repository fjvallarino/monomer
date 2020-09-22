{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widget.Widgets.Button (
  button,
  button_
) where

import Data.Default
import Data.Text (Text)

import Monomer.Event.Types
import Monomer.Graphics.Drawing
import Monomer.Graphics.Types
import Monomer.Widget.BaseSingle
import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.Widgets.WidgetCombinators

data ButtonConfig s e = ButtonConfig {
  _btnOnClick :: [e],
  _btnOnClickReq :: [WidgetRequest s]
}

instance Default (ButtonConfig s e) where
  def = ButtonConfig {
    _btnOnClick = [],
    _btnOnClickReq = []
  }

instance Semigroup (ButtonConfig s e) where
  (<>) t1 t2 = ButtonConfig {
    _btnOnClick = _btnOnClick t1 <> _btnOnClick t2,
    _btnOnClickReq = _btnOnClickReq t1 <> _btnOnClickReq t2
  }

instance Monoid (ButtonConfig s e) where
  mempty = def

instance OnClick (ButtonConfig s e) e where
  onClick handler = def {
    _btnOnClick = [handler]
  }

instance OnClickReq (ButtonConfig s e) s where
  onClickReq req = def {
    _btnOnClickReq = [req]
  }

button :: Text -> e -> WidgetInstance s e
button label handler = button_ label (onClick handler)

button_ :: Text -> ButtonConfig s e -> WidgetInstance s e
button_ label config = defaultWidgetInstance "button" (makeButton label config)

makeButton :: Text -> ButtonConfig s e -> Widget s e
makeButton label config = widget where
  widget = createSingle def {
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  handleEvent wenv ctx evt widgetInst = case evt of
    Click p _
      | pointInViewport p widgetInst -> Just result
      where
        requests = _btnOnClickReq config
        events = _btnOnClick config
        result = resultReqsEvents requests events widgetInst
    _ -> Nothing

  getSizeReq wenv widgetInst = sizeReq where
    theme = activeTheme wenv widgetInst
    style = activeStyle wenv widgetInst
    size = getTextSize wenv theme style label
    sizeReq = SizeReq size FlexibleSize StrictSize

  render renderer wenv widgetInst =
    drawStyledText_ renderer renderArea style label
    where
      style = activeStyle wenv widgetInst
      renderArea = _wiRenderArea widgetInst
