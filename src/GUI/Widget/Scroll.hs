{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.Scroll (scroll) where

import Data.Default
import Data.Typeable
import Control.Monad
import Control.Monad.State

import qualified Data.Text as T

import GUI.Common.Core
import GUI.Common.Drawing
import GUI.Common.Style
import GUI.Data.Tree
import GUI.Widget.Core

import GHC.Generics

data ScrollState = ScrollState {
  _scDeltaX :: !Double,
  _scDeltaY :: !Double,
  _scChildSize :: Size
} deriving (Eq, Show, Typeable, Generic)

scroll :: (MonadState s m) => WidgetNode s e m -> WidgetNode s e m
scroll managedWidget = parentWidget (makeScroll (ScrollState 0 0 def)) [managedWidget]

makeScroll :: (MonadState s m) => ScrollState -> Widget s e m
makeScroll state@(ScrollState dx dy cs@(Size cw ch)) = Widget {
    _widgetType = "scroll",
    _widgetFocusable = False,
    _widgetRestoreState = fmap makeScroll . useState,
    _widgetSaveState = makeState state,
    _widgetHandleEvent = handleEvent,
    _widgetHandleCustom = defaultCustomHandler,
    _widgetPreferredSize = preferredSize,
    _widgetResizeChildren = resizeChildren,
    _widgetRender = render
  }
  where
    stepSize = 50
    wheelRate = 10
    widgetType = "scroll"
    focusable = False
    handleEvent view@(Rect rx ry rw rh) evt = case evt of
      Click (Point px py) btn status -> result where
        result = if isPressed then resultReqsEventsWidget [ResizeChildren] [] (makeScroll newState) else Nothing
        isPressed = status == PressedBtn && inRect view (Point px py)
        isLeftClick = isPressed && btn == LeftBtn
        isRigthClick = isPressed && btn == RightBtn
        step = if | isLeftClick -> stepSize
                  | isRigthClick -> -stepSize
                  | otherwise -> 0
        newState = ScrollState (scrollX step dx cw rw) dy cs
      WheelScroll _ (Point wx wy) wheelDirection -> result where
        needsUpdate = (wx /= 0 && cw > rw) || (wy /= 0 && ch > rh)
        result = if needsUpdate then resultReqsEventsWidget [ResizeChildren] [] (makeScroll newState) else Nothing
        stepX = wx * if wheelDirection == WheelNormal then -wheelRate else wheelRate
        stepY = wy * if wheelDirection == WheelNormal then wheelRate else -wheelRate
        newState = ScrollState (scrollX stepX dx cw rw) (scrollX stepY dy ch rh) cs
      _ -> Nothing
    scrollX reqDelta currScroll childPos viewportLimit
      | reqDelta >= 0 = if currScroll + reqDelta < 0
                          then currScroll + reqDelta
                          else 0
      | otherwise = if childPos - viewportLimit + currScroll + reqDelta > 0
                      then currScroll + reqDelta
                      else viewportLimit - childPos
    preferredSize _ _ children = return (head children)
    resizeChildren (Rect l t w h) _ _ children = Just $ WidgetResizeResult viewport renderArea newWidget where
      SizeReq (Size cw2 ch2) _ _ = (head children)
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
