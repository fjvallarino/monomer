{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.Layout (empty, hgrid, vgrid, hstack, spacer) where

import Control.Monad
import Control.Monad.State

import Data.Default

import GUI.Common.Core
import GUI.Common.Style
import GUI.Data.Tree
import GUI.Widget.Core

import qualified Data.Text as T

empty :: (MonadState s m) => WidgetNode s e m
empty = singleWidget makeHGrid

hgrid :: (MonadState s m) => [WidgetNode s e m] -> WidgetNode s e m
hgrid = parentWidget makeHGrid

makeHGrid :: (MonadState s m) => Widget s e m
makeHGrid = makeFixedGrid "hgrid" Horizontal

vgrid :: (MonadState s m) => [WidgetNode s e m] -> WidgetNode s e m
vgrid = parentWidget makeVGrid

makeVGrid :: (MonadState s m) => Widget s e m
makeVGrid = makeFixedGrid "vgrid" Vertical

makeFixedGrid :: (MonadState s m) => WidgetType -> Direction -> Widget s e m
makeFixedGrid widgetType direction = Widget {
    _widgetType = widgetType,
    _widgetFocusable = False,
    _widgetRestoreState = defaultRestoreState,
    _widgetSaveState = defaultSaveState,
    _widgetHandleEvent = handleEvent,
    _widgetHandleCustom = defaultCustomHandler,
    _widgetPreferredSize = preferredSize,
    _widgetResizeChildren = resizeChildren,
    _widgetRender = render
  }
  where
    focusable = False
    handleEvent _ _ = Nothing
    preferredSize _ _ children = return reqSize where
      reqSize = SizeReq (Size width height) FlexibleSize FlexibleSize
      width = (fromIntegral wMul) * (maximum . map (_w . _srSize)) children
      height = (fromIntegral hMul) * (maximum . map (_h . _srSize)) children
      wMul = if direction == Horizontal then length children else 1
      hMul = if direction == Horizontal then 1 else length children
    resizeChildren _ (Rect l t w h) style children = Just $ WidgetResizeResult newViewports newViewports Nothing where
      cols = if direction == Horizontal then (length children) else 1
      rows = if direction == Horizontal then 1 else (length children)
      newViewports = fmap resizeChild [0..(length children - 1)]
      resizeChild i = Rect (cx i) (cy i) cw ch
      cw = w / fromIntegral cols
      ch = h / fromIntegral rows
      cx i = l + (fromIntegral $ i `div` rows) * cw
      cy i = t + (fromIntegral $ i `div` cols) * ch
    render renderer WidgetInstance{..} children ts = do
      handleRenderChildren renderer children ts

hstack :: (MonadState s m) => [WidgetNode s e m] -> WidgetNode s e m
hstack = parentWidget (makeHStack "hstack" Horizontal)

makeHStack :: (MonadState s m) => WidgetType -> Direction -> Widget s e m
makeHStack widgetType direction = Widget {
    _widgetType = widgetType,
    _widgetFocusable = False,
    _widgetRestoreState = defaultRestoreState,
    _widgetSaveState = defaultSaveState,
    _widgetHandleEvent = handleEvent,
    _widgetHandleCustom = defaultCustomHandler,
    _widgetPreferredSize = preferredSize,
    _widgetResizeChildren = resizeChildren,
    _widgetRender = render
  }
  where
    focusable = False
    handleEvent _ _ = Nothing
    preferredSize _ _ children = return reqSize where
      reqSize = SizeReq (calcPreferredSize children) FlexibleSize FlexibleSize
    resizeChildren _ (Rect l t w h) style children = Just $ WidgetResizeResult newViewports newViewports Nothing where
      sChildren = filter (\c -> _srPolicyWidth c == StrictSize) children
      fChildren = filter (\c -> _srPolicyWidth c == FlexibleSize) children
      rChildren = filter (\c -> _srPolicyWidth c == RemainderSize) children
      remainderCount = length rChildren
      remainderExist = not $ null rChildren
      Size sw _  = calcPreferredSize sChildren
      Size fw _  = calcPreferredSize fChildren
      fRatio = if | w - sw > fw &&     remainderExist -> 1
                  | w - sw > fw && not remainderExist -> (w - sw) / fw
                  | w - sw > 0                        -> (w - sw) / fw
                  | otherwise                         -> 0
      remainderTotal = w - (sw + fw * fRatio)
      remainderUnit = if remainderExist then max 0 remainderTotal / fromIntegral remainderCount else 0
      newViewports = reverse revViewports
      (revViewports, _) = foldl foldHelper ([], l) children
      foldHelper (accum, left) child = (newSize : accum, left + nw) where
        newSize@(Rect _ _ nw _) = resizeChild left child
      resizeChild left (SizeReq (Size cw _) srW _) = Rect left t newWidth h where
        newWidth = case srW of
          StrictSize -> cw
          FlexibleSize -> cw * fRatio
          RemainderSize -> remainderUnit
    calcPreferredSize children = Size width height where
      maxWidth = if null children then 0 else (maximum . map (_w . _srSize)) children
      sumWidth = (sum . map (_w . _srSize)) children
      maxHeight = if null children then 0 else (maximum . map (_h . _srSize)) children
      sumHeight = (sum . map (_h . _srSize)) children
      width = if direction == Horizontal then sumWidth else maxWidth
      height = if direction == Horizontal then maxHeight else sumHeight
    render renderer WidgetInstance{..} children ts = do
      handleRenderChildren renderer children ts


spacer :: (MonadState s m) => WidgetNode s e m
spacer = singleWidget makeSpacer

makeSpacer :: (MonadState s m) => Widget s e m
makeSpacer = Widget {
    _widgetType = "spacer",
    _widgetFocusable = False,
    _widgetRestoreState = defaultRestoreState,
    _widgetSaveState = defaultSaveState,
    _widgetHandleEvent = handleEvent,
    _widgetHandleCustom = defaultCustomHandler,
    _widgetPreferredSize = preferredSize,
    _widgetResizeChildren = resizeChildren,
    _widgetRender = render
  }
  where
    handleEvent view evt = Nothing
    preferredSize renderer (style@Style{..}) _ = return $ SizeReq (Size 10 10) RemainderSize RemainderSize
    resizeChildren _ _ _ _ = Nothing
    render renderer WidgetInstance{..} _ ts = return ()

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
