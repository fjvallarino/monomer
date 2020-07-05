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
  _scDraggingX :: Bool,
  _scDraggingY :: Bool,
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

defaultState = ScrollState False False 0 0 def (singleNode def)

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
makeScroll state@(ScrollState dgx dgy dx dy cs prevReqs) = createContainer {
    _widgetHandleEvent = containerHandleEvent handleEvent,
    _widgetPreferredSize = containerPreferredSize preferredSize,
    _widgetResize = scrollResize Nothing,
    _widgetRender = render
  }
  where
    Size childWidth childHeight = cs
    handleEvent wctx ctx evt widgetInstance = case evt of
      Click (Point px py) btn status -> result where
        isLeftPressed = status == PressedBtn && btn == LeftBtn
        isButtonReleased = status == ReleasedBtn
        newState = if | isLeftPressed && hMouseInThumb -> state { _scDraggingX = True }
                      | isLeftPressed && vMouseInThumb -> state { _scDraggingY = True }
                      | isButtonReleased -> state { _scDraggingX = False, _scDraggingY = False }
                      | otherwise -> state
        newInstance = widgetInstance {
          _instanceWidget = makeScroll newState
        }
        result = if | isLeftPressed && (hMouseInThumb || vMouseInThumb) -> Just $ resultWidget newInstance
                    | isButtonReleased && (_scDraggingX state || _scDraggingY state) -> Just $ resultWidget newInstance
                    | otherwise -> Nothing
      Move (Point px py) -> result where
        hMid = _rw hThumbRect / 2
        vMid = _rh vThumbRect / 2
        hDelta = (rx - px + hMid) / hScrollRatio
        vDelta = (ry - py + vMid) / vScrollRatio
        draggingX = _scDraggingX state
        draggingY = _scDraggingY state
        newDeltaX = if draggingX then scrollAxis hDelta 0 childWidth rw else dx
        newDeltaY = if draggingY then scrollAxis vDelta 0 childHeight rh else dy
        newState = state { _scDeltaX = newDeltaX, _scDeltaY = newDeltaY }
        result = if draggingX || draggingY
                    then Just $ resultReqs [IgnoreChildrenEvents] (rebuildWidget wctx newState widgetInstance prevReqs)
                    else Nothing
      WheelScroll _ (Point wx wy) wheelDirection -> result where
        needsUpdate = (wx /= 0 && childWidth > rw) || (wy /= 0 && childHeight > rh)
        result = if | needsUpdate -> Just $ resultReqs [IgnoreChildrenEvents] (rebuildWidget wctx newState widgetInstance prevReqs)
                    | otherwise   -> Nothing
        stepX = wx * if wheelDirection == WheelNormal then -wheelRate else wheelRate
        stepY = wy * if wheelDirection == WheelNormal then wheelRate else -wheelRate
        newState = state {
          _scDeltaX = scrollAxis stepX dx childWidth rw,
          _scDeltaY = scrollAxis stepY dy childHeight rh
        }
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

      newWidget = fromMaybe (makeScroll $ state { _scChildSize = Size areaW areaH, _scReqSize = reqs }) updatedWidget
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
  ScrollState _ _ dx dy (Size childWidth childHeight) _ = scrollState
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
