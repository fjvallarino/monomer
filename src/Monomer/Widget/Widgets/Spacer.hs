module Monomer.Widget.Widgets.Spacer (
  spacer
) where

import Data.Default

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Widget.BaseSingle
import Monomer.Widget.Types
import Monomer.Widget.Util

data SpacerCfg = SpacerCfg {
  _spcWidth :: Maybe Double,
  _spcHeight :: Maybe Double
}

spacer :: WidgetInstance s e
spacer = defaultWidgetInstance "spacer" makeSpacer

defaultSpace :: Double
defaultSpace = 10

makeSpacer :: Widget s e
makeSpacer = widget where
  widget = createSingle def {
    singleGetSizeReq = getSizeReq
  }

  getSizeReq wenv widgetInst = sizeReq where
    size = 10
    factor = 0.5
    sizeReq = (FlexSize size factor, FlexSize size factor)
