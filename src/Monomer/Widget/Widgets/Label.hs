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
makeLabel caption = createWidget {
    _widgetPreferredSize = preferredSize,
    _widgetRender = render
  }
  where
    preferredSize wenv widgetInstance = singleNode sizeReq where
      Style{..} = _instanceStyle widgetInstance
      size = getTextBounds wenv _styleText caption
      sizeReq = SizeReq size FlexibleSize StrictSize

    render renderer wenv ctx WidgetInstance{..} =
      do
        drawStyledBackground renderer _instanceRenderArea _instanceStyle
        drawStyledText_ renderer _instanceRenderArea _instanceStyle caption
