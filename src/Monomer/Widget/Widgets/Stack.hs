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
    preferredSize renderer app childrenPairs = Node reqSize childrenReqs where
      reqSize = SizeReq (calcPreferredSize childrenPairs) FlexibleSize FlexibleSize
      childrenReqs = fmap snd childrenPairs

    resize app viewport renderArea widgetInstance childrenPairs = (widgetInstance, assignedArea) where
      Rect l t w h = renderArea
      visibleChildren = Seq.filter (_instanceVisible . fst) childrenPairs
      policySelector = if isHorizontal then _sizePolicyWidth else _sizePolicyHeight
      sizeSelector = if isHorizontal then _w else _h
      rectSelector = if isHorizontal then _rw else _rh
      mSize = if isHorizontal then w else h
      mStart = if isHorizontal then l else t
      policyFilter policy childPair = policySelector (nodeValue $ snd childPair) == policy
      sChildren = Seq.filter (policyFilter StrictSize) visibleChildren
      fChildren = Seq.filter (policyFilter FlexibleSize) visibleChildren
      rChildren = Seq.filter (policyFilter RemainderSize) visibleChildren
      remainderCount = length rChildren
      remainderExist = not $ null rChildren
      sSize = sizeSelector $ calcPreferredSize sChildren
      fSize = sizeSelector $ calcPreferredSize fChildren
      fCount = fromIntegral $ length fChildren
      fExtra = if fCount > 0 && not remainderExist
                  then (mSize - sSize - fSize) / fCount
                  else 0
      remainderTotal = mSize - (sSize + fCount * fExtra)
      remainderUnit = if remainderExist then max 0 remainderTotal / fromIntegral remainderCount else 0
      newViewports = Seq.reverse revViewports
      assignedArea = Seq.zip newViewports newViewports
      (revViewports, _) = foldl' foldHelper (Seq.empty, mStart) childrenPairs
      foldHelper (accum, offset) childPair = (newSize <| accum, offset + rectSelector newSize) where
        newSize = resizeChild offset childPair
      resizeChild offset childPair = result where
        result = if | not $ _instanceVisible widgetInstance -> emptyRect
                    | isHorizontal -> hRect
                    | otherwise -> vRect
        widgetInstance = fst childPair
        req = nodeValue $ snd childPair
        srSize = _sizeRequested req
        emptyRect = Rect l t 0 0
        hRect = Rect offset t calcNewSize h
        vRect = Rect l offset w calcNewSize
        calcNewSize = case policySelector req of
          StrictSize -> sizeSelector srSize
          FlexibleSize -> sizeSelector srSize + fExtra
          RemainderSize -> remainderUnit

    calcPreferredSize childrenPairs = Size width height where
      (maxWidth, sumWidth, maxHeight, sumHeight) = calcDimensions childrenPairs
      width = if isHorizontal then sumWidth else maxWidth
      height = if isHorizontal then maxHeight else sumHeight

    calcDimensions childrenPairs = (maxWidth, sumWidth, maxHeight, sumHeight) where
      visiblePairs = Seq.filter (_instanceVisible . fst) childrenPairs
      visibleChildren = fmap (nodeValue . snd) visiblePairs 
      maxWidth = if Seq.null visibleChildren then 0 else (maximum . fmap (_w . _sizeRequested)) visibleChildren
      sumWidth = (sum . fmap (_w . _sizeRequested)) visibleChildren
      maxHeight = if null visibleChildren then 0 else (maximum . fmap (_h . _sizeRequested)) visibleChildren
      sumHeight = (sum . fmap (_h . _sizeRequested)) visibleChildren
