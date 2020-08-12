{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Label (label) where

import Control.Monad
import Data.Text (Text)

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Graphics.Drawing
import Monomer.Widget.BaseWidget
import Monomer.Widget.Types
import Monomer.Widget.Util

label :: Text -> WidgetInstance s e
label caption = defaultWidgetInstance "label" (makeLabel caption)

makeLabel :: Text -> Widget s e
makeLabel caption = widget where
  widget = createWidget {
    widgetPreferredSize = preferredSize,
    widgetRender = render
  }

  preferredSize wenv widgetInst = newInst where
    Style{..} = _wiStyle widgetInst
    size = getTextBounds wenv _styleText caption
    sizeReq = SizeReq size FlexibleSize StrictSize
    newInst = widgetInst {
      _wiSizeReq = sizeReq
    }

  render renderer wenv widgetInst@WidgetInstance{..} = do
    drawWidgetBg renderer wenv widgetInst
    drawStyledText_ renderer _wiRenderArea _wiStyle caption
