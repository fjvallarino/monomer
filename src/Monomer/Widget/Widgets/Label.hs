module Monomer.Widget.Widgets.Label (label) where

import Control.Monad
import Data.Default
import Data.Text (Text)

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Graphics.Drawing
import Monomer.Widget.BaseSingle
import Monomer.Widget.Types
import Monomer.Widget.Util

label :: Text -> WidgetInstance s e
label caption = defaultWidgetInstance "label" (makeLabel caption)

makeLabel :: Text -> Widget s e
makeLabel caption = widget where
  widget = createSingle def {
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  getSizeReq wenv widgetInst = sizeReq where
    style = activeStyle wenv widgetInst
    size = getTextBounds wenv style caption
    sizeReq = SizeReq size FlexibleSize StrictSize

  render renderer wenv widgetInst = do
    drawStyledBackground renderer renderArea style
    drawStyledText_ renderer renderArea style caption
    where
      renderArea = _wiRenderArea widgetInst
      style = activeStyle wenv widgetInst
