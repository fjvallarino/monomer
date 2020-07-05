{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Scroll (scroll) where

import Debug.Trace

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

data ScrollContext = ScrollContext {
  hScrollRatio :: Double,
  vScrollRatio :: Double,
  hScrollRequired :: Bool,
  vScrollRequired :: Bool,
  hMouseInScroll :: Bool,
  vMouseInScroll :: Bool,
  hMouseInThumb :: Bool,
  vMouseInThumb :: Bool,
  hScrollRect :: Rect,
  vScrollRect :: Rect,
  hThumbRect :: Rect,
  vThumbRect :: Rect
}

defaultState = ScrollState 0 0 def (singleNode def)

barThickness = 10
stepSize = 50
wheelRate = 10

scroll :: WidgetInstance s e -> WidgetInstance s e
scroll managedWidget = makeInstance (makeScroll defaultState) managedWidget

makeInstance :: Widget s e -> WidgetInstance s e -> WidgetInstance s e
makeInstance widget managedWidget = (defaultWidgetInstance "scroll" widget) {
  _instanceChildren = Seq.singleton managedWidget,
  _instanceFocusable = False
}

makeScroll :: ScrollState -> Widget s e
makeScroll state@(ScrollState dx dy cs@(Size childWidth childHeight) prevReqs) = createContainer {
    _widgetHandleEvent = containerHandleEvent handleEvent,
    _widgetPreferredSize = containerPreferredSize preferredSize,
    _widgetResize = scrollResize Nothing,
    _widgetRender = render
  }
  where
    handleEvent wctx ctx evt widgetInstance = case evt of
      Click (Point px py) btn status -> result where
        isLeftClick = status == ReleasedBtn && btn == LeftBtn
        hMid = _rw hThumbRect / 2
        vMid = _rh vThumbRect / 2
        hDelta = (rx - px + hMid) / hScrollRatio
        vDelta = (ry - py + vMid) / vScrollRatio
        hChanged = hMouseInScroll && not hMouseInThumb
        vChanged = vMouseInScroll && not vMouseInThumb
        newDeltaX = if hChanged then scrollAxis hDelta 0 childWidth rw else dx
        newDeltaY = if vChanged then scrollAxis vDelta 0 childHeight rh else dy
        newState = ScrollState newDeltaX newDeltaY cs prevReqs
        result = if isLeftClick && (hChanged || vChanged)
                    then Just $ resultReqs [IgnoreChildrenEvents] (rebuildWidget wctx newState widgetInstance prevReqs)
                    else Nothing
      WheelScroll _ (Point wx wy) wheelDirection -> result where
        needsUpdate = (wx /= 0 && childWidth > rw) || (wy /= 0 && childHeight > rh)
        result = if | needsUpdate -> Just $ resultReqs [IgnoreChildrenEvents] (rebuildWidget wctx newState widgetInstance prevReqs)
                    | otherwise   -> Nothing
        stepX = wx * if wheelDirection == WheelNormal then -wheelRate else wheelRate
        stepY = wy * if wheelDirection == WheelNormal then wheelRate else -wheelRate
        newState = ScrollState (scrollAxis stepX dx childWidth rw) (scrollAxis stepY dy childHeight rh) cs prevReqs
      _ -> Nothing
      where
        viewport = _instanceViewport widgetInstance
        Rect rx ry rw rh = _instanceViewport widgetInstance
        ScrollContext{..} = scrollStatus wctx state viewport
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

      Size childWidth2 childHeight2 = _sizeRequested $ nodeValue childReq
      areaW = max w childWidth2
      areaH = max h childHeight2
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
        containerRender defaultRender renderer wctx ctx widgetInstance
        resetScissor renderer

        when (hScrollRequired && hMouseInScroll) $
          drawRect renderer hScrollRect (Just barColor) Nothing

        when (vScrollRequired && vMouseInScroll) $
          drawRect renderer vScrollRect (Just barColor) Nothing

        when hScrollRequired $
          drawRect renderer hThumbRect (Just handleColorH) Nothing

        when vScrollRequired $
          drawRect renderer vThumbRect (Just handleColorV) Nothing
      where
        viewport = _instanceViewport widgetInstance
        ScrollContext{..} = scrollStatus wctx state viewport
        barColor = darkGray { _alpha = 0.4 }
        handleColorH = if hMouseInThumb then gray else darkGray
        handleColorV = if vMouseInThumb then gray else darkGray

scrollStatus :: WidgetContext s e -> ScrollState -> Rect -> ScrollContext
scrollStatus wctx scrollState viewport = ScrollContext{..} where
  ScrollState dx dy (Size childWidth childHeight) _ = scrollState
  mousePos = statusMousePos (_wcInputStatus wctx)
  vpLeft = _rx viewport
  vpTop = _ry viewport
  vpWidth = _rw viewport
  vpHeight = _rh viewport
  hScrollTop = vpHeight - barThickness
  vScrollLeft = vpWidth - barThickness
  hScrollRatio = min (vpWidth / childWidth) 1
  vScrollRatio = min (vpHeight / childHeight) 1
  hScrollRequired = hScrollRatio < 1
  vScrollRequired = vScrollRatio < 1
  hScrollRect = Rect vpLeft (vpTop + hScrollTop) (vpLeft + vpWidth) (vpTop + vpHeight)
  vScrollRect = Rect (vpLeft + vScrollLeft) vpTop (vpLeft + vpWidth) (vpTop + vpHeight)
  hThumbRect = Rect (vpLeft - hScrollRatio * dx) (vpTop + hScrollTop) (hScrollRatio * vpWidth) barThickness
  vThumbRect = Rect (vpLeft + vScrollLeft) (vpTop - vScrollRatio * dy) barThickness (vScrollRatio * vpHeight)
  hMouseInScroll = inRect hScrollRect mousePos
  vMouseInScroll = inRect vScrollRect mousePos
  hMouseInThumb = inRect hThumbRect mousePos
  vMouseInThumb = inRect vThumbRect mousePos
