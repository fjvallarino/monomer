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

hgrid_ :: (MonadState s m) => [Tree (WidgetNode s e m)] -> Tree (WidgetNode s e m)
hgrid_ = parentWidget makeHGrid

makeHGrid :: (MonadState s m) => Widget s e m
makeHGrid = makeFixedGrid "hgrid" Horizontal

vgrid_ :: (MonadState s m) => [Tree (WidgetNode s e m)] -> Tree (WidgetNode s e m)
vgrid_ = parentWidget makeVGrid

makeVGrid :: (MonadState s m) => Widget s e m
makeVGrid = makeFixedGrid "vgrid" Vertical

makeFixedGrid :: (MonadState s m) => WidgetType -> Direction -> Widget s e m
makeFixedGrid widgetType direction = Widget widgetType widgetFocusable handleEvent preferredSize resizeChildren render
  where
    widgetFocusable = False
    handleEvent _ _ _ = NoEvents
    render _ _ _ _ _ _ = return ()
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

{--
makeSizedGrid :: (Monad m) => Direction -> Widget e m
makeSizedGrid direction = Widget widgetType handleEvent preferredSize resizeChildren render
  where
    widgetType = "directionalLayout"
    handleEvent _ _ = NoEvents
    render _ _ _ _ = return ()
    preferredSize _ _ children = return $ Size (width children) (height children) where
      width = if direction == Horizontal then (sum . map _w) else (maximum . (map _w))
      height = if direction == Horizontal then (maximum . (map _h)) else (sum . (map _h))
    resizeChildren rect style children = []
--}

{--
hgrid :: (Monad m) => Rect -> [Widget s m] -> m Bool -> Widget s m
hgrid rect widgets isVisible = makeGrid rect (length widgets) 1 widgets isVisible

vgrid :: (Monad m) => Rect -> [Widget s m] -> m Bool -> Widget s m
vgrid rect widgets isVisible = makeGrid rect 1 (length widgets) widgets isVisible

makeGrid :: (Monad m) => Rect -> Int -> Int -> [Widget s m] -> m Bool -> Widget s m
makeGrid r@(Rect l t w h) rows cols widgets iv = widget
  where
    widget = Widget widgetData iv (handleEvent widgets) (render widgets) resize showMe
    widgetData = WidgetData l t w h
    handleEvent widgets _ e = do
      newWidgets <- mapM (\wt -> _handleEvent wt (Rect l t w h) e) widgets
      pure $ makeGrid r rows cols newWidgets iv
    render widgets r _ = mapM_ (\Widget{..} -> whenM _isVisible $ _render r (widgetDataToRect _widgetData)) widgets
    showMe = show $ fmap _widgetData widgets
    resize _ = makeGrid r rows cols newWidgets iv
      where
        newWidgets = fmap resizeChild (zip [0..] widgets)
        resizeChild (i, child@(Widget {..})) = _resize $ child { _widgetData = (WidgetData (cx i) (cy i) cw ch) }
        cw = w / fromIntegral cols
        ch = h / fromIntegral rows
        cx i = l + (fromIntegral $ i `mod` rows) * cw
        cy i = t + (fromIntegral $ i `div` cols) * ch
--}
