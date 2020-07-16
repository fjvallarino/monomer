module Monomer.Widget.Widgets.Spacer (spacer) where

import Control.Monad

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Widget.BaseWidget
import Monomer.Widget.Types
import Monomer.Widget.Util

spacer :: WidgetInstance s e
spacer = defaultWidgetInstance "spacer" makeSpacer

defaultSpace :: Double
defaultSpace = 10

makeSpacer :: Widget s e
makeSpacer = createWidget {
    _widgetPreferredSize = preferredSize
  }
  where
    preferredSize wctx widgetInstance = singleNode sizeReq where
      sizeReq = SizeReq (Size defaultSpace defaultSpace) RemainderSize RemainderSize
