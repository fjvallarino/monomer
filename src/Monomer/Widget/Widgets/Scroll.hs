{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Scroll (scroll) where

import Control.Monad
import Data.Default
import Data.Maybe
import Data.Typeable

import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Tree
import Monomer.Event.Types
import Monomer.Graphics.Color
import Monomer.Graphics.Drawing
import Monomer.Graphics.Renderer
import Monomer.Graphics.Types
import Monomer.Widget.BaseContainer
import Monomer.Widget.PathContext
import Monomer.Widget.Types
import Monomer.Widget.Util

data ScrollState = ScrollState {
  _scDeltaX :: !Double,
  _scDeltaY :: !Double,
  _scChildSize :: Size,
  _scReqSize :: Tree SizeReq
} deriving (Typeable)

defaultState = ScrollState 0 0 def (singleNode def)

scroll :: WidgetInstance s e -> WidgetInstance s e
scroll managedWidget = makeInstance (makeScroll defaultState) managedWidget

makeInstance :: Widget s e -> WidgetInstance s e -> WidgetInstance s e
makeInstance widget managedWidget = (defaultWidgetInstance "scroll" widget) {
  _instanceChildren = Seq.singleton managedWidget,
  _instanceFocusable = False
}

makeScroll :: ScrollState -> Widget s e
makeScroll state@(ScrollState dx dy cs@(Size cw ch) prevReqs) = createContainer {
    _widgetHandleEvent = containerHandleEvent handleEvent,
    _widgetPreferredSize = containerPreferredSize preferredSize,
    _widgetResize = scrollResize Nothing,
    _widgetRender = render
  }
  where
    stepSize = 50
    wheelRate = 10
    handleEvent wctx ctx evt widgetInstance = case evt of
      Click (Point px py) btn status -> result where
        viewport = _instanceViewport widgetInstance
        result = if | isPressed -> Just $ resultWidget (rebuildWidget wctx newState widgetInstance prevReqs)
                    | otherwise -> Nothing
        isPressed = status == PressedBtn && inRect viewport (Point px py)
        isLeftClick = isPressed && btn == LeftBtn
        isRigthClick = isPressed && btn == RightBtn
        step = if | isLeftClick -> stepSize
                  | isRigthClick -> -stepSize
                  | otherwise -> 0
        newState = ScrollState (scrollAxis step dx cw (_rw viewport)) dy cs prevReqs
      WheelScroll _ (Point wx wy) wheelDirection -> result where
        Rect rx ry rw rh = _instanceViewport widgetInstance
        needsUpdate = (wx /= 0 && cw > rw) || (wy /= 0 && ch > rh)
        result = if | needsUpdate -> Just $ resultWidget (rebuildWidget wctx newState widgetInstance prevReqs)
                    | otherwise   -> Nothing
        stepX = wx * if wheelDirection == WheelNormal then -wheelRate else wheelRate
        stepY = wy * if wheelDirection == WheelNormal then wheelRate else -wheelRate
        newState = ScrollState (scrollAxis stepX dx cw rw) (scrollAxis stepY dy ch rh) cs prevReqs
      _ -> Nothing
    scrollAxis reqDelta currScroll childPos viewportLimit
      | reqDelta >= 0 = if currScroll + reqDelta < 0
                          then currScroll + reqDelta
                          else 0
      | otherwise = if childPos - viewportLimit + currScroll + reqDelta > 0
                      then currScroll + reqDelta
                      else viewportLimit - childPos

    rebuildWidget wctx newState widgetInstance reqs = newInstance where
      newWidget = makeScroll newState
      tempInstance = widgetInstance { _instanceWidget = newWidget }
      newInstance = scrollResize (Just newWidget) wctx (_instanceViewport tempInstance) (_instanceRenderArea tempInstance) tempInstance reqs

    preferredSize renderer wctx childrenPairs = Node sizeReq childrenReqs where
      childrenReqs = fmap snd childrenPairs
      sizeReq = SizeReq (_sizeRequested . nodeValue $ Seq.index childrenReqs 0) FlexibleSize FlexibleSize

    scrollResize updatedWidget wctx viewport renderArea widgetInstance reqs = newInstance where
      Rect l t w h = renderArea
      child = Seq.index (_instanceChildren widgetInstance) 0
      childReq = fromMaybe (singleNode def) (Seq.lookup 0 (nodeChildren reqs))

      Size cw2 ch2 = _sizeRequested $ nodeValue childReq
      areaW = max w cw2
      areaH = max h ch2
      childRenderArea = Rect (l + dx) (t + dy) areaW areaH

      newWidget = fromMaybe (makeScroll $ ScrollState dx dy (Size areaW areaH) reqs) updatedWidget
      newChildWidget = _widgetResize (_instanceWidget child) wctx viewport childRenderArea child childReq

      newInstance = widgetInstance {
        _instanceViewport = viewport,
        _instanceRenderArea = renderArea,
        _instanceWidget = newWidget,
        _instanceChildren = Seq.singleton newChildWidget
      }

    render renderer wctx ctx widgetInstance =
      do
        setScissor renderer viewport
        containerRender ignoreRender renderer wctx ctx widgetInstance
        resetScissor renderer

        when (barRatioH < 1) $
          drawRect renderer scrollRectH (Just $ darkGray { _alpha = 0.6 }) Nothing

        when (barRatioV < 1) $
          drawRect renderer scrollRectV (Just $ darkGray { _alpha = 0.6 }) Nothing
      where
        barThickness = 10
        viewport = _instanceViewport widgetInstance
        vpLeft = _rx viewport
        vpTop = _ry viewport
        vpWidth = _rw viewport
        vpHeight = _rh viewport
        barTop = vpHeight - barThickness
        barLeft = vpWidth - barThickness
        barRatioH = vpWidth / cw
        barRatioV = vpHeight / ch
        scrollRectH = Rect (vpLeft - barRatioH * dx) (vpTop + barTop) (barRatioH * vpWidth) barThickness
        scrollRectV = Rect (vpLeft + barLeft) (vpTop - barRatioV * dy) barThickness (barRatioV * vpHeight)
