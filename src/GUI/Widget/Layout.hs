{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.Layout where

import Control.Monad
import Control.Monad.State

import Data.Default

import GUI.Core
import GUI.Data.Tree
import GUI.Widget.Core
import GUI.Widget.Style

import qualified Data.Text as T

hgrid_ :: (Monad m) => [Tree (WidgetNode e m)] -> Tree (WidgetNode e m)
hgrid_ = parentWidget makeHGrid

makeHGrid :: (Monad m) => Widget e m
makeHGrid = makeFixedGrid "hgrid" Horizontal

vgrid_ :: (Monad m) => [Tree (WidgetNode e m)] -> Tree (WidgetNode e m)
vgrid_ = parentWidget makeVGrid

makeVGrid :: (Monad m) => Widget e m
makeVGrid = makeFixedGrid "vgrid" Vertical

makeFixedGrid :: (Monad m) => WidgetType -> Direction -> Widget e m
makeFixedGrid widgetType direction = Widget widgetType handleEvent preferredSize resizeChildren render
  where
    handleEvent _ _ = NoEvents
    render _ _ _ _ = return ()
    preferredSize _ _ children = return $ Size width height where
      width = (fromIntegral wMul) * (maximum . map _w) children
      height = (fromIntegral hMul) * (maximum . map _h) children
      wMul = if direction == Horizontal then length children else 1
      hMul = if direction == Horizontal then 1 else length children
    resizeChildren (Rect l t w h) style children = newWidgets where
      cols = if direction == Horizontal then (length children) else 1
      rows = if direction == Horizontal then 1 else (length children)
      newWidgets = fmap resizeChild [0..(length children - 1)]
      resizeChild i = Rect (cx i) (cy i) cw ch
      cw = w / fromIntegral cols
      ch = h / fromIntegral rows
      cx i = l + (fromIntegral $ i `div` rows) * cw
      cy i = t + (fromIntegral $ i `div` cols) * ch
