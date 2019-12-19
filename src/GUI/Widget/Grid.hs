{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.Grid (empty, hgrid, vgrid) where

import Control.Monad
import Control.Monad.State

import Data.Default

import GUI.Common.Core
import GUI.Common.Event
import GUI.Common.Style
import GUI.Common.Types
import GUI.Data.Tree

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
