{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Grid (empty, hgrid, vgrid) where

import Control.Monad

import Monomer.Common.Core
import Monomer.Common.Types

import qualified Data.Text as T

hgrid :: (Monad m) => [WidgetNode s e m] -> WidgetNode s e m
hgrid = parentWidget makeHGrid

makeHGrid :: (Monad m) => Widget s e m
makeHGrid = makeFixedGrid "hgrid" Horizontal

vgrid :: (Monad m) => [WidgetNode s e m] -> WidgetNode s e m
vgrid = parentWidget makeVGrid

makeVGrid :: (Monad m) => Widget s e m
makeVGrid = makeFixedGrid "vgrid" Vertical

makeFixedGrid :: (Monad m) => WidgetType -> Direction -> Widget s e m
makeFixedGrid widgetType direction = baseWidget {
    _widgetType = widgetType,
    _widgetHandleEvent = handleEvent,
    _widgetPreferredSize = preferredSize,
    _widgetResizeChildren = resizeChildren
  }
  where
    focusable = False
    handleEvent _ _ _ = Nothing
    preferredSize _ _ _ children = return reqSize where
      reqSize = sizeReq (Size width height) FlexibleSize FlexibleSize
      width = if null children then 0 else (fromIntegral wMul) * (maximum . map (_w . _srSize)) children
      height = if null children then 0 else (fromIntegral hMul) * (maximum . map (_h . _srSize)) children
      wMul = if direction == Horizontal then length children else 1
      hMul = if direction == Horizontal then 1 else length children
    resizeChildren _ (Rect l t w h) style children = Just $ WidgetResizeResult newViewports newViewports Nothing where
      visibleChildren = filter _srVisible children
      cols = if direction == Horizontal then (length visibleChildren) else 1
      rows = if direction == Horizontal then 1 else (length visibleChildren)
      foldHelper (accum, index) child = (index : accum, index + if _srVisible child then 1 else 0)
      indices = reverse . fst $ foldl foldHelper ([], 0) children
      newViewports = fmap resizeChild indices
      resizeChild i = Rect (cx i) (cy i) cw ch
      cw = if cols > 0 then w / fromIntegral cols else 0
      ch = if rows > 0 then h / fromIntegral rows else 0
      cx i = if rows > 0 then l + (fromIntegral $ i `div` rows) * cw else 0
      cy i = if cols > 0 then t + (fromIntegral $ i `div` cols) * ch else 0
