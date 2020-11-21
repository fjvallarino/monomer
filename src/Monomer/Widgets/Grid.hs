module Monomer.Widgets.Grid (
  hgrid,
  vgrid
) where

import Data.Default
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq(..), (|>))

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

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

  isVertical = not isHorizontal

  getSizeReq wenv inst children = (newSizeReqW, newSizeReqH) where
    vchildren = Seq.filter _wiVisible children
    newSizeReqW = getDimSizeReq isHorizontal _wiSizeReqW vchildren
    newSizeReqH = getDimSizeReq isVertical _wiSizeReqH vchildren

  getDimSizeReq mainAxis accesor vchildren
    | Seq.null vreqs = FixedSize 0
    | mainAxis = foldl1 sizeReqMergeSum (Seq.replicate nreqs maxSize)
    | otherwise = maxSize
    where
      vreqs = accesor <$> vchildren
      nreqs = Seq.length vreqs
      maxSize = foldl1 sizeReqMergeMax vreqs

  resize wenv viewport renderArea children inst = resized where
    style = activeStyle wenv inst
    contentArea = fromMaybe def (removeOuterBounds style renderArea)
    Rect l t w h = contentArea
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
    foldHelper (currAreas, index) child = (newAreas, newIndex) where
      (newIndex, newViewport)
        | _wiVisible child = (index + 1, calcViewport index)
        | otherwise = (index, def)
      newArea = (newViewport, newViewport)
      newAreas = currAreas |> newArea
    calcViewport i = Rect (cx i) (cy i) cw ch
    assignedAreas = fst $ foldl' foldHelper (Seq.empty, 0) children
    resized = (inst, assignedAreas)
