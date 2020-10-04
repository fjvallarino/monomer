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
    mainSize = if isHorizontal then w else h
    mainStart = if isHorizontal then l else t
    vchildren = Seq.filter _wiVisible children
    sChildren = Seq.filter (isStrictReq . mainReqSelector) vchildren
    fChildren = Seq.filter (not . isStrictReq . mainReqSelector) vchildren
    fExists = not $ null fChildren
    sSize = sizeSelector $ calcSize sChildren
    fSize = sizeSelector $ calcSize fChildren
    fSizeFactor = sizeSelector $ calcSizeFactor fChildren
    rSize = max 0 (mainSize - sSize)
    fExtra
      | fExists && fSize > 0 = (rSize - fSize) / fSizeFactor
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
      -- factor still not accounted for
      FlexSize sz factor -> (1 + fExtra * factor) * sz  -- (1 + fExtra) * sz
    calcSndSize total = case sndReqSelector child of
      FixedSize sz -> sz
      _ -> total
    hRect = Rect offset t calcMainSize (calcSndSize h)
    vRect = Rect l offset (calcSndSize w) calcMainSize
    result
      | not $ _wiVisible child = emptyRect
      | isHorizontal = hRect
      | otherwise = vRect

  calcSize vchildren = calcSize_ vchildren False
  calcSizeFactor vchildren = calcSize_ vchildren True

  calcSize_ vchildren useFactor = Size width height where
    (maxW, sumW, maxH, sumH) = calcDimensions vchildren useFactor
    width
      | isHorizontal = sumW
      | otherwise = maxW
    height
      | isHorizontal = maxH
      | otherwise = sumH

  calcDimensions vchildren useFactor = (maxW, sumW, maxH, sumH) where
    getReqSize
      | useFactor = getReqFactored
      | otherwise = getReqCoord
    vreqsW = _wiSizeReqW <$> vchildren
    vreqsH = _wiSizeReqH <$> vchildren
    sumW = (sum . fmap getReqSize) vreqsW
    sumH = (sum . fmap getReqSize) vreqsH
    maxW
      | Seq.null vchildren = 0
      | otherwise = (maximum . fmap getReqSize) vreqsW
    maxH
      | Seq.null vchildren = 0
      | otherwise = (maximum . fmap getReqSize) vreqsH

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
