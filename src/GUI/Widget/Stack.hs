{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.Stack (hstack) where

import Control.Monad
import Control.Monad.State

import GUI.Common.Core
import GUI.Common.Style
import GUI.Data.Tree
import GUI.Widget.Core

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
