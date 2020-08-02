{-# LANGUAGE MultiWayIf #-}

module Monomer.Widget.Widgets.Stack (
  hstack,
  vstack
) where

import Control.Monad
import Data.List (foldl')
import Data.Sequence (Seq(..), (<|), (|>))

import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Tree
import Monomer.Widget.Types
import Monomer.Widget.BaseContainer
import Monomer.Widget.Util

hstack :: (Traversable t) => t (WidgetInstance s e) -> WidgetInstance s e
hstack children = (defaultWidgetInstance "hstack" (makeStack True)) {
  _instanceChildren = foldl' (|>) Empty children
}

vstack :: (Traversable t) => t (WidgetInstance s e) -> WidgetInstance s e
vstack children = (defaultWidgetInstance "vstack" (makeStack False)) {
  _instanceChildren = foldl' (|>) Empty children
}

makeStack :: Bool -> Widget s e
makeStack isHorizontal = widget where
  widget = createContainer {
    _widgetPreferredSize = containerPreferredSize preferredSize,
    _widgetResize = containerResize resize
  }

  preferredSize wenv widgetInst children reqs = Node reqSize reqs where
    (_, vreqs) = visibleChildrenReq children reqs
    size = calcPreferredSize vreqs
    reqSize = SizeReq size FlexibleSize FlexibleSize

  resize wenv viewport renderArea children reqs widgetInst = resized where
    Rect l t w h = renderArea
    childrenPairs = Seq.zip children reqs
    (vchildren, vreqs) = visibleChildrenReq children reqs
    mainSize = if isHorizontal then w else h
    mainStart = if isHorizontal then l else t
    policyFilter policy req = policySelector req == policy
    sChildren = Seq.filter (policyFilter StrictSize) vreqs
    fChildren = Seq.filter (policyFilter FlexibleSize) vreqs
    rChildren = Seq.filter (policyFilter RemainderSize) vreqs
    fExists = not $ null fChildren
    rExists = not $ null rChildren
    sSize = sizeSelector $ calcPreferredSize sChildren
    fSize = sizeSelector $ calcPreferredSize fChildren
    rSize = max 0 (mainSize - sSize)
    fCount = fromIntegral $ length fChildren
    rCount = fromIntegral $ length rChildren
    fExtra
      | fExists = (rSize - fSize) / fSize
      | otherwise = 0
    rUnit
      | rExists && not fExists = rSize / rCount
      | otherwise = 0
    assignedArea = Seq.zip newViewports newViewports
    (newViewports, _) = foldl' foldHelper (Seq.empty, mainStart) childrenPairs
    foldHelper (accum, offset) childPair = (newAccum, newOffset) where
      newSize = resizeChild renderArea fExtra rUnit offset childPair
      newAccum = accum |> newSize
      newOffset = offset + rectSelector newSize
    resized = (widgetInst, assignedArea)

  resizeChild renderArea fExtra rUnit offset childPair = result where
    Rect l t w h = renderArea
    childInstance = fst childPair
    req = nodeValue $ snd childPair
    srSize = _sizeRequested req
    emptyRect = Rect l t 0 0
    hRect = Rect offset t calcNewSize h
    vRect = Rect l offset w calcNewSize
    calcNewSize = case policySelector req of
      StrictSize -> sizeSelector srSize
      FlexibleSize -> (1 + fExtra) * sizeSelector srSize
      RemainderSize -> rUnit
    result
      | not $ _instanceVisible childInstance = emptyRect
      | isHorizontal = hRect
      | otherwise = vRect

  calcPreferredSize vreqs = Size width height where
    (maxWidth, sumWidth, maxHeight, sumHeight) = calcDimensions vreqs
    width = if isHorizontal then sumWidth else maxWidth
    height = if isHorizontal then maxHeight else sumHeight

  calcDimensions vreqs = (maxWidth, sumWidth, maxHeight, sumHeight) where
    sumWidth = (sum . fmap (_w . _sizeRequested)) vreqs
    sumHeight = (sum . fmap (_h . _sizeRequested)) vreqs
    maxWidth
      | Seq.null vreqs = 0
      | otherwise = (maximum . fmap (_w . _sizeRequested)) vreqs
    maxHeight
      | Seq.null vreqs = 0
      | otherwise = (maximum . fmap (_h . _sizeRequested)) vreqs

  sizeSelector = if isHorizontal then _w else _h
  rectSelector = if isHorizontal then _rw else _rh
  policySelector = if isHorizontal then _sizePolicyWidth else _sizePolicyHeight
