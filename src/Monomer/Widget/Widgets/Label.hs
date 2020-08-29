{- HLINT ignore "Reduce duplication" -}

module Monomer.Widget.Widgets.Label (label) where

import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Graphics.Drawing
import Monomer.Widget.BaseSingle
import Monomer.Widget.Types
import Monomer.Widget.Util

data LabelState = LabelState {
  _lstCaption :: Text,
  _lstCaptionFit :: Text
} deriving (Eq, Show)

label :: Text -> WidgetInstance s e
label caption = defaultWidgetInstance "label" (makeLabel state) where
  state = LabelState caption caption

makeLabel :: LabelState -> Widget s e
makeLabel state@(LabelState caption captionFit) = widget where
  widget = createSingle def {
    singleMerge = merge,
    singleGetState = makeState state,
    singleGetSizeReq = getSizeReq,
    singleResize = resize,
    singleRender = render
  }

  merge wenv oldState widgetInst = resultWidget newInstance where
    newState = fromMaybe state (useState oldState)
    newInstance = widgetInst {
      _wiWidget = makeLabel newState
    }

  getSizeReq wenv widgetInst = sizeReq where
    theme = activeTheme wenv widgetInst
    style = activeStyle wenv widgetInst
    size = getTextSize wenv theme style caption
    sizeReq = SizeReq size FlexibleSize StrictSize

  resize wenv viewport renderArea widgetInst = newInst where
    theme = activeTheme wenv widgetInst
    style = activeStyle wenv widgetInst
    size = getTextSize wenv theme style caption
    (newCaptionFit, newSize) = fitText wenv theme style renderArea caption
    newWidget
      | captionFit == newCaptionFit = _wiWidget widgetInst
      | otherwise = makeLabel (LabelState caption newCaptionFit)
    newInst = widgetInst {
      _wiWidget = newWidget,
      _wiViewport = viewport,
      _wiRenderArea = renderArea
    }

  render renderer wenv widgetInst = do
    drawStyledBackground renderer renderArea style
    drawStyledText_ renderer renderArea style captionFit
    where
      renderArea = _wiRenderArea widgetInst
      style = activeStyle wenv widgetInst
