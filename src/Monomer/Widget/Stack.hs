{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Stack (hstack, vstack) where

import Control.Monad
import Control.Monad.State

import Monomer.Common.Core
import Monomer.Common.Event
import Monomer.Common.Style
import Monomer.Common.Types
import Monomer.Data.Tree

hstack :: (MonadState s m) => [WidgetNode s e m] -> WidgetNode s e m
hstack = parentWidget makeHStack

makeHStack :: (MonadState s m) => Widget s e m
makeHStack = makeStack "hstack" Horizontal

vstack :: (MonadState s m) => [WidgetNode s e m] -> WidgetNode s e m
vstack = parentWidget makeVStack

makeVStack :: (MonadState s m) => Widget s e m
makeVStack = makeStack "vstack" Vertical

makeStack :: (MonadState s m) => WidgetType -> Direction -> Widget s e m
makeStack widgetType direction = baseWidget {
    _widgetType = widgetType,
    _widgetHandleEvent = handleEvent,
    _widgetPreferredSize = preferredSize,
    _widgetResizeChildren = resizeChildren,
    _widgetRender = render
  }
  where
    isHorizontal = direction == Horizontal
    focusable = False
    handleEvent _ _ = Nothing
    preferredSize _ _ children = return reqSize where
      reqSize = sizeReq (calcPreferredSize children) FlexibleSize FlexibleSize
    resizeChildren _ (Rect l t w h) style children = Just $ WidgetResizeResult newViewports newViewports Nothing where
      visibleChildren = filter _srVisible children
      policySelector = if isHorizontal then _srPolicyWidth else _srPolicyHeight
      sizeSelector = if isHorizontal then _w else _h
      rectSelector = if isHorizontal then _rw else _rh
      mSize = if isHorizontal then w else h
      mStart = if isHorizontal then l else t
      sChildren = filter (\c -> policySelector c == StrictSize) visibleChildren
      fChildren = filter (\c -> policySelector c == FlexibleSize) visibleChildren
      rChildren = filter (\c -> policySelector c == RemainderSize) visibleChildren
      remainderCount = length rChildren
      remainderExist = not $ null rChildren
      sSize = sizeSelector $ calcPreferredSize sChildren
      fSize = sizeSelector $ calcPreferredSize fChildren
      fRatio = if | mSize - sSize > fSize &&     remainderExist -> 1
                  | mSize - sSize > fSize && not remainderExist -> (mSize - sSize) / fSize
                  | mSize - sSize > 0                           -> (mSize - sSize) / fSize
                  | otherwise                                   -> 0
      remainderTotal = mSize - (sSize + fSize * fRatio)
      remainderUnit = if remainderExist then max 0 remainderTotal / fromIntegral remainderCount else 0
      newViewports = reverse revViewports
      (revViewports, _) = foldl foldHelper ([], mStart) children
      foldHelper (accum, offset) child = (newSize : accum, offset + rectSelector newSize) where
        newSize = resizeChild offset child
      resizeChild offset sr = if not (_srVisible sr) then emptyRect else if isHorizontal then hRect else vRect where
        srSize = _srSize sr
        emptyRect = Rect l t 0 0
        hRect = Rect offset t newSize h
        vRect = Rect l offset w newSize
        newSize = case policySelector sr of
          StrictSize -> sizeSelector srSize
          FlexibleSize -> sizeSelector srSize * fRatio
          RemainderSize -> remainderUnit
    calcPreferredSize children = Size width height where
      maxWidth = if null children then 0 else (maximum . map (_w . _srSize)) children
      sumWidth = (sum . map (_w . _srSize)) children
      maxHeight = if null children then 0 else (maximum . map (_h . _srSize)) children
      sumHeight = (sum . map (_h . _srSize)) children
      width = if isHorizontal then sumWidth else maxWidth
      height = if isHorizontal then maxHeight else sumHeight
    render _ _ _ _ = return ()
    renderPost _ _ _ _ = return ()
