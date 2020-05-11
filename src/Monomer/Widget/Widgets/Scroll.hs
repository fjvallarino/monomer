{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Scroll (scroll) where

import Control.Monad
import Data.Default
import Data.Typeable
import GHC.Generics

import Monomer.Common.Types
import Monomer.Common.Util
import Monomer.Event.Types
import Monomer.Graphics.Color
import Monomer.Graphics.Drawing
import Monomer.Graphics.Renderer
import Monomer.Graphics.Types
import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.Widgets.Base


data ScrollState = ScrollState {
  _scDeltaX :: !Double,
  _scDeltaY :: !Double,
  _scChildSize :: Size
} deriving (Eq, Show, Typeable, Generic)

scroll :: (Monad m) => WidgetNode s e m -> WidgetNode s e m
scroll managedWidget = parentWidget (makeScroll (ScrollState 0 0 def)) [managedWidget]

makeScroll :: (Monad m) => ScrollState -> Widget s e m
makeScroll state@(ScrollState dx dy cs@(Size cw ch)) = baseWidget {
    _widgetType = "scroll",
    _widgetRestoreState = defaultRestoreState makeScroll,
    _widgetSaveState = makeState state,
    _widgetHandleEvent = handleEvent,
    _widgetPreferredSize = preferredSize,
    _widgetResizeChildren = resizeChildren,
    _widgetRender = render,
    _widgetRenderPost = renderPost
  }
  where
    stepSize = 50
    wheelRate = 10
    widgetType = "scroll"
    focusable = False
    handleEvent app view@(Rect rx ry rw rh) evt = case evt of
      Click (Point px py) btn status -> result where
        result = if isPressed then Just $ WidgetEventResult [ResizeChildren] [] (Just $ makeScroll newState) id else Nothing
        isPressed = status == PressedBtn && inRect view (Point px py)
        isLeftClick = isPressed && btn == LeftBtn
        isRigthClick = isPressed && btn == RightBtn
        step = if | isLeftClick -> stepSize
                  | isRigthClick -> -stepSize
                  | otherwise -> 0
        newState = ScrollState (scrollAxis step dx cw rw) dy cs
      WheelScroll _ (Point wx wy) wheelDirection -> result where
        needsUpdate = (wx /= 0 && cw > rw) || (wy /= 0 && ch > rh)
        result = if needsUpdate then Just $ WidgetEventResult [ResizeChildren] [] (Just $ makeScroll newState) id else Nothing
        stepX = wx * if wheelDirection == WheelNormal then -wheelRate else wheelRate
        stepY = wy * if wheelDirection == WheelNormal then wheelRate else -wheelRate
        newState = ScrollState (scrollAxis stepX dx cw rw) (scrollAxis stepY dy ch rh) cs
      _ -> Nothing
    scrollAxis reqDelta currScroll childPos viewportLimit
      | reqDelta >= 0 = if currScroll + reqDelta < 0
                          then currScroll + reqDelta
                          else 0
      | otherwise = if childPos - viewportLimit + currScroll + reqDelta > 0
                      then currScroll + reqDelta
                      else viewportLimit - childPos
    preferredSize _ _ _ children = return $ sizeReq size FlexibleSize FlexibleSize where
      SizeReq size _ _ _ = head children
    resizeChildren (Rect l t w h) _ _ children = Just $ WidgetResizeResult viewport renderArea newWidget where
      Size cw2 ch2 = _srSize (head children)
      areaW = max w cw2
      areaH = max h ch2
      newWidget = Just $ makeScroll (ScrollState dx dy (Size areaW areaH))
      viewport = [Rect l t w h]
      renderArea = [Rect (l + dx) (t + dy) areaW areaH]
    render renderer app WidgetInstance{..} ts =
      do
        scissor renderer _widgetInstanceViewport
    renderPost renderer app WidgetInstance{..} ts =
      do
        resetScissor renderer

        when (barRatioH < 1) $ do
          drawRect renderer scrollRectH (Just $ darkGray { _alpha = 0.6 }) Nothing

        when (barRatioV < 1) $ do
          drawRect renderer scrollRectV (Just $ darkGray { _alpha = 0.6 }) Nothing
      where
        barThickness = 10
        vpLeft = (_rx _widgetInstanceViewport)
        vpTop = (_ry _widgetInstanceViewport)
        vpWidth = (_rw _widgetInstanceViewport)
        vpHeight = (_rh _widgetInstanceViewport)
        barTop = vpHeight - barThickness
        barLeft = vpWidth - barThickness
        barRatioH = vpWidth / cw
        barRatioV = vpHeight / ch
        scrollRectH = Rect (vpLeft - barRatioH * dx) (vpTop + barTop) (barRatioH * vpWidth) barThickness
        scrollRectV = Rect (vpLeft + barLeft) (vpTop - barRatioV * dy) barThickness (barRatioV * vpHeight)
