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
makeStack isHorizontal = createContainer {
    _widgetPreferredSize = containerPreferredSize preferredSize,
    _widgetResize = containerResize resize
  }
  where
    preferredSize wenv widgetInstance children reqs = Node reqSize reqs where
      (_, vreqs) = visibleChildrenReq children reqs
      reqSize = SizeReq (calcPreferredSize vreqs) FlexibleSize FlexibleSize

    resize wenv viewport renderArea widgetInstance children reqs = (widgetInstance, assignedArea) where
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
      fAvg = if fExists then fSize / fCount else 0
      fBigFilter c = sizeSelector (_sizeRequested c) >= fAvg
      fBigCount = fromIntegral $ Seq.length (Seq.filter fBigFilter fChildren)
      fExtra = if fExists then (rSize - fSize) / fBigCount else 0
      rUnit = if rExists && not fExists then rSize / rCount else 0
      assignedArea = Seq.zip newViewports newViewports
      (newViewports, _) = foldl' foldHelper (Seq.empty, mainStart) childrenPairs
      foldHelper (accum, offset) childPair = (newAccum, newOffset) where
        newSize = resizeChild renderArea fAvg fExtra rUnit offset childPair
        newAccum = accum |> newSize
        newOffset = offset + rectSelector newSize

    resizeChild renderArea fAvg fExtra rUnit offset childPair = result where
      Rect l t w h = renderArea
      result = if | not $ _instanceVisible childInstance -> emptyRect
                  | isHorizontal -> hRect
                  | otherwise -> vRect
      childInstance = fst childPair
      req = nodeValue $ snd childPair
      srSize = _sizeRequested req
      emptyRect = Rect l t 0 0
      hRect = Rect offset t calcNewSize h
      vRect = Rect l offset w calcNewSize
      calcNewSize = case policySelector req of
        StrictSize -> sizeSelector srSize
        FlexibleSize
          | sizeSelector srSize >= fAvg -> sizeSelector srSize + fExtra
          | otherwise -> sizeSelector srSize
        RemainderSize -> rUnit

    calcPreferredSize vreqs = Size width height where
      (maxWidth, sumWidth, maxHeight, sumHeight) = calcDimensions vreqs
      width = if isHorizontal then sumWidth else maxWidth
      height = if isHorizontal then maxHeight else sumHeight

    calcDimensions vreqs = (maxWidth, sumWidth, maxHeight, sumHeight) where
      maxWidth = if Seq.null vreqs then 0 else (maximum . fmap (_w . _sizeRequested)) vreqs
      sumWidth = (sum . fmap (_w . _sizeRequested)) vreqs
      maxHeight = if null vreqs then 0 else (maximum . fmap (_h . _sizeRequested)) vreqs
      sumHeight = (sum . fmap (_h . _sizeRequested)) vreqs

    sizeSelector = if isHorizontal then _w else _h
    rectSelector = if isHorizontal then _rw else _rh
    policySelector = if isHorizontal then _sizePolicyWidth else _sizePolicyHeight
