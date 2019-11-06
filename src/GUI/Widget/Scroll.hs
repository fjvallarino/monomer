{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.Scroll (scroll) where

import Data.Default
import Debug.Trace
import Control.Monad
import Control.Monad.State

import qualified Data.Text as T

import GUI.Common.Core
import GUI.Common.Drawing
import GUI.Common.Style
import GUI.Data.Tree
import GUI.Widget.Core

data ScrollState = ScrollState {
  _scDeltaX :: !Double,
  _scDeltaY :: !Double,
  _scChildSize :: Size
} deriving (Eq, Show)

scroll :: (MonadState s m) => WidgetNode s e m -> WidgetNode s e m
scroll managedWidget = parentWidget (makeScroll (ScrollState 0 0 def)) [managedWidget]

makeScroll :: (MonadState s m) => ScrollState -> Widget s e m
makeScroll state@(ScrollState dx dy cs@(Size cw ch)) = Widget {
    _widgetType = "scroll",
    _widgetFocusable = False,
    _widgetHandleEvent = handleEvent,
    _widgetPreferredSize = preferredSize,
    _widgetResizeChildren = resizeChildren,
    _widgetRender = render
  }
  where
    stepSize = 50
    widgetType = "scroll"
    focusable = False
    handleEvent view@(Rect rx ry rw rh) evt = case evt of
      Click (Point px py) btn status -> eventResultRequest [ResizeChildren] [] (makeScroll newState) where
        isPressed = status == PressedBtn && inRect view (Point px py)
        isLeftClick = traceShow state $ isPressed && btn == LeftBtn
        isRigthClick = isPressed && btn == RightBtn
        newDx = if | isLeftClick -> if dx + stepSize < 0 then dx + stepSize else 0
                   | isRigthClick -> if cw - rw + dx - stepSize > 0 then dx - stepSize else rw - cw
                   | otherwise -> dx
        newState = ScrollState newDx dy cs
      _ -> Nothing
    preferredSize _ _ children = return (head children)
    resizeChildren (Rect l t w h) _ children = Just $ WidgetResizeResult viewport renderArea newWidget where
      Size cw2 ch2 = (head children)
      newWidget = Just $ makeScroll (ScrollState dx dy (Size cw2 ch2))
      viewport = [Rect l t w h]
      renderArea = [Rect (l + dx) (t + dy) cw2 ch2]
    render renderer WidgetInstance{..} children ts =
      do
        scissor renderer _widgetInstanceViewport
        handleRenderChildren renderer children ts
        resetScissor renderer

        drawText renderer _widgetInstanceRenderArea (_textStyle _widgetInstanceStyle) (T.pack (show dx))

        when (barRatioH < 1) $ do
          drawRect renderer scrollRectH (Just darkGray) Nothing

        when (barRatioV < 1) $ do
          drawRect renderer scrollRectV (Just darkGray) Nothing
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
