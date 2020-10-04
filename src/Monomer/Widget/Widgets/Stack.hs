module Monomer.Widget.Widgets.Stack (
  hstack,
  vstack
) where

import Data.Default
import Data.List (foldl')
import Data.Sequence (Seq(..), (<|), (|>))

import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Widget.BaseContainer
import Monomer.Widget.Internal
import Monomer.Widget.Types
import Monomer.Widget.Util

hstack :: (Traversable t) => t (WidgetInstance s e) -> WidgetInstance s e
hstack children = (defaultWidgetInstance "hstack" (makeStack True)) {
  _wiChildren = foldl' (|>) Empty children
}

vstack :: (Traversable t) => t (WidgetInstance s e) -> WidgetInstance s e
vstack children = (defaultWidgetInstance "vstack" (makeStack False)) {
  _wiChildren = foldl' (|>) Empty children
}

makeStack :: Bool -> Widget s e
makeStack isHorizontal = widget where
  widget = createContainer def {
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  getSizeReq wenv widgetInst children = (newSizeReqW, newSizeReqH) where
    vchildren = Seq.filter _wiVisible children
    nReqs = length vchildren
    vreqsW = _wiSizeReqW <$> vchildren
    vreqsH = _wiSizeReqH <$> vchildren
    strictReqs reqs = Seq.filter isStrictReq reqs
    strictW = nReqs > 0 && Seq.length (strictReqs vreqsW) == nReqs
    strictH = nReqs > 0 && Seq.length (strictReqs vreqsH) == nReqs
    factor = 1
    Size width height = calcSize vchildren
    newSizeReqW
      | not isHorizontal && strictW = FixedSize width
      | otherwise = FlexSize width factor
    newSizeReqH
      | isHorizontal && strictH = FixedSize height
      | otherwise = FlexSize height factor

  resize wenv viewport renderArea children widgetInst = resized where
    Rect l t w h = renderArea
    vchildren = Seq.filter _wiVisible children
    mainSize = if isHorizontal then w else h
    mainStart = if isHorizontal then l else t
    sChildren = Seq.filter (isStrictReq . mainReqSelector) vchildren
    fChildren = Seq.filter (not . isStrictReq . mainReqSelector) vchildren
    fExists = not $ null fChildren
    sSize = sizeSelector $ calcSize sChildren
    fSize = sizeSelector $ calcSize fChildren
    rSize = max 0 (mainSize - sSize)
    fExtra
      | fExists && fSize > 0 = (rSize - fSize) / fSize
      | otherwise = 0
    assignedArea = Seq.zip newViewports newViewports
    (newViewports, _) = foldl' foldHelper (Seq.empty, mainStart) children
    foldHelper (accum, offset) child = (newAccum, newOffset) where
      newSize = resizeChild renderArea fExtra offset child
      newAccum = accum |> newSize
      newOffset = offset + rectSelector newSize
    resized = (widgetInst, assignedArea)

  resizeChild renderArea fExtra offset child = result where
    Rect l t w h = renderArea
    emptyRect = Rect l t 0 0
    calcMainSize = case mainReqSelector child of
      FixedSize sz -> sz
      FlexSize sz factor -> (1 + fExtra) * sz -- factor still not accounted for
    calcSndSize total = case sndReqSelector child of
      FixedSize sz -> sz
      _ -> total
    hRect = Rect offset t calcMainSize (calcSndSize h)
    vRect = Rect l offset (calcSndSize w) calcMainSize
    result
      | not $ _wiVisible child = emptyRect
      | isHorizontal = hRect
      | otherwise = vRect

  calcSize vchildren = Size width height where
    (maxWidth, sumWidth, maxHeight, sumHeight) = calcDimensions vchildren
    width
      | isHorizontal = sumWidth
      | otherwise = maxWidth
    height
      | isHorizontal = maxHeight
      | otherwise = sumHeight

  calcDimensions vchildren = (maxWidth, sumWidth, maxHeight, sumHeight) where
    vreqsW = _wiSizeReqW <$> vchildren
    vreqsH = _wiSizeReqH <$> vchildren
    sumWidth = (sum . fmap getReqCoord) vreqsW
    sumHeight = (sum . fmap getReqCoord) vreqsH
    maxWidth
      | Seq.null vchildren = 0
      | otherwise = (maximum . fmap getReqCoord) vreqsW
    maxHeight
      | Seq.null vchildren = 0
      | otherwise = (maximum . fmap getReqCoord) vreqsH

  mainReqSelector
    | isHorizontal = _wiSizeReqW
    | otherwise = _wiSizeReqH

  sndReqSelector
    | isHorizontal = _wiSizeReqH
    | otherwise = _wiSizeReqW

  sizeSelector
    | isHorizontal = _sW
    | otherwise = _sH

  rectSelector
    | isHorizontal = _rW
    | otherwise = _rH
