module Monomer.Widget.Widgets.Spacer (spacer) where

import Control.Monad
import Data.Default

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Widget.BaseSingle
import Monomer.Widget.Types
import Monomer.Widget.Util

spacer :: WidgetInstance s e
spacer = defaultWidgetInstance "spacer" makeSpacer

defaultSpace :: Double
defaultSpace = 10

makeSpacer :: Widget s e
makeSpacer = widget where
  widget = createSingle def {
    singleUpdateSizeReq = updateSizeReq
  }

  updateSizeReq wenv widgetInst = newInst where
    size = Size defaultSpace defaultSpace
    sizeReq = SizeReq size RemainderSize RemainderSize
    newInst = widgetInst {
      _wiSizeReq = sizeReq
    }
