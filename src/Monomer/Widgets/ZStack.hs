module Monomer.Widgets.ZStack where

import Control.Monad (forM_, when)
import Data.Default
import Data.List (foldl')
import Data.Sequence (Seq(..), (<|), (|>))

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

zstack :: (Traversable t) => t (WidgetInstance s e) -> WidgetInstance s e
zstack children = (defaultWidgetInstance "zstack" (makeZStack False)) {
  _wiChildren = Seq.reverse $ foldl' (|>) Empty children
}

makeZStack :: Bool -> Widget s e
makeZStack isHorizontal = widget where
  baseWidget = createContainer def {
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }
  widget = baseWidget {
    widgetRender = render
  }

  getSizeReq wenv widgetInst children = (newSizeReqW, newSizeReqH) where
    vchildren = Seq.filter _wiVisible children
    nReqs = length vchildren
    vreqsW = _wiSizeReqW <$> vchildren
    vreqsH = _wiSizeReqH <$> vchildren
    fixedReqs reqs = Seq.filter isFixedSizeReq reqs
    fixedW = nReqs > 0 && Seq.length (fixedReqs vreqsW) == nReqs
    fixedH = nReqs > 0 && Seq.length (fixedReqs vreqsH) == nReqs
    factor = 1
    width
      | Seq.null vreqsW = 0
      | otherwise = maximum $ fmap getMaxSizeReq vreqsW
    height
      | Seq.null vreqsH = 0
      | otherwise = maximum $ fmap getMaxSizeReq vreqsH
    newSizeReqW
      | not isHorizontal && fixedW = FixedSize width
      | otherwise = FlexSize width factor
    newSizeReqH
      | isHorizontal && fixedH = FixedSize height
      | otherwise = FlexSize height factor

  resize wenv viewport renderArea children widgetInst = resized where
    assignedAreas = fmap (const (viewport, renderArea)) children
    resized = (widgetInst, assignedAreas)

  render renderer wenv widgetInst =
    drawInScissor renderer True viewport $
      drawStyledAction renderer renderArea style $ \_ ->
        forM_ children $ \child -> when (isVisible child) $
          widgetRender (_wiWidget child) renderer wenv child
    where
      style = instanceStyle wenv widgetInst
      children = Seq.reverse $ _wiChildren widgetInst
      viewport = _wiViewport widgetInst
      renderArea = _wiRenderArea widgetInst
      isVisible c = _wiVisible c
