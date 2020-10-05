module Monomer.Widget.Widgets.Grid (
  hgrid,
  vgrid
) where

import Data.Default
import Data.List (foldl')
import Data.Sequence (Seq(..), (|>))

import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Widget.Internal
import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.BaseContainer

hgrid :: (Traversable t) => t (WidgetInstance s e) -> WidgetInstance s e
hgrid children = (defaultWidgetInstance "hgrid" (makeFixedGrid True)) {
  _wiChildren = foldl' (|>) Empty children
}

vgrid :: (Traversable t) => t (WidgetInstance s e) -> WidgetInstance s e
vgrid children = (defaultWidgetInstance "vgrid" (makeFixedGrid False)) {
  _wiChildren = foldl' (|>) Empty children
}

makeFixedGrid :: Bool -> Widget s e
makeFixedGrid isHorizontal = widget where
  widget = createContainer def {
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  getSizeReq wenv widgetInst children = (newSizeReqW, newSizeReqH) where
    vchildren = Seq.filter _wiVisible children
    nReqs = length vchildren
    vreqsW = _wiSizeReqW <$> vchildren
    vreqsH = _wiSizeReqH <$> vchildren
    fixedReqs reqs = Seq.filter isFixedReq reqs
    fixedW = nReqs > 0 && Seq.length (fixedReqs vreqsW) == nReqs
    fixedH = nReqs > 0 && Seq.length (fixedReqs vreqsH) == nReqs
    factor = 1
    wMul
      | isHorizontal = fromIntegral (length vchildren)
      | otherwise = 1
    hMul
      | isHorizontal = 1
      | otherwise = fromIntegral (length vchildren)
    width
      | Seq.null vchildren = 0
      | otherwise = wMul * (maximum . fmap getMaxSize) vreqsW
    height
      | Seq.null vchildren = 0
      | otherwise = hMul * (maximum . fmap getMaxSize) vreqsH
    newSizeReqW
      | not isHorizontal && fixedW = FixedSize width
      | otherwise = FlexSize width factor
    newSizeReqH
      | isHorizontal && fixedH = FixedSize height
      | otherwise = FlexSize height factor

  resize wenv viewport renderArea children widgetInst = resized where
    Rect l t w h = renderArea
    vchildren = Seq.filter _wiVisible children
    cols = if isHorizontal then length vchildren else 1
    rows = if isHorizontal then 1 else length vchildren
    cw = if cols > 0 then w / fromIntegral cols else 0
    ch = if rows > 0 then h / fromIntegral rows else 0
    cx i
      | rows > 0 = l + fromIntegral (i `div` rows) * cw
      | otherwise = 0
    cy i
      | cols > 0 = t + fromIntegral (i `div` cols) * ch
      | otherwise = 0
    foldHelper (newAreas, index) child = (newAreas |> newArea, newIndex) where
      visible = _wiVisible child
      newIndex = index + if _wiVisible child then 1 else 0
      newViewport = if visible then calcViewport index else def
      newArea = (newViewport, newViewport)
    calcViewport i = Rect (cx i) (cy i) cw ch
    assignedAreas = fst $ foldl' foldHelper (Seq.empty, 0) vchildren
    resized = (widgetInst, assignedAreas)

getMaxSize :: SizeReq -> Coord
getMaxSize (FixedSize c) = c
getMaxSize (FlexSize c _) = c
getMaxSize (BoundedSize _ c2 _) = c2
