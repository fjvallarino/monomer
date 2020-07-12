{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Scroll (ScrollMessage(..), scroll) where

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

data ActiveBar = HBar | VBar deriving (Eq)

newtype ScrollMessage = ScrollTo Rect deriving Typeable

data ScrollState = ScrollState {
  _scDragging :: Maybe ActiveBar,
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

defaultState = ScrollState Nothing 0 0 def (singleNode def)

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
makeScroll state@(ScrollState dragging dx dy cs prevReqs) = createContainer {
    _widgetGetState = getState,
    _widgetMerge = containerMergeTrees merge,
    _widgetHandleEvent = containerHandleEvent handleEvent,
    _widgetHandleMessage = containerHandleMessage handleMessage,
    _widgetPreferredSize = containerPreferredSize preferredSize,
    _widgetResize = scrollResize Nothing,
    _widgetRender = render
  }
  where
    Size childWidth childHeight = cs

    getState = makeState state

    merge wctx ctx oldState widgetInstance = newInstance where
      newState = fromMaybe state (useState oldState)
      newInstance = widgetInstance {
        _instanceWidget = makeScroll newState
      }

    handleEvent wctx ctx evt widgetInstance = case evt of
      Click point btn status -> result where
        isLeftPressed = status == PressedBtn && btn == LeftBtn
        isButtonReleased = status == ReleasedBtn
        newState = if | isLeftPressed && hMouseInThumb -> state { _scDragging = Just HBar }
                      | isLeftPressed && vMouseInThumb -> state { _scDragging = Just VBar }
                      | isLeftPressed && hMouseInScroll -> updateScrollThumb state HBar point viewport sctx
                      | isLeftPressed && vMouseInScroll -> updateScrollThumb state VBar point viewport sctx
                      | isButtonReleased -> state { _scDragging = Nothing }
                      | otherwise -> state
        newInstance = widgetInstance {
          _instanceWidget = makeScroll newState
        }
        result = if | isLeftPressed && (hMouseInThumb || vMouseInThumb) -> Just $ resultWidget newInstance
                    | isLeftPressed && (hMouseInScroll || vMouseInScroll) -> Just $ resultWidget newInstance
                    | isButtonReleased && isJust (_scDragging state) -> Just $ resultWidget newInstance
                    | otherwise -> Nothing
      Move point -> result where
        updatedState = fmap (\dg -> updateScrollThumb state dg point viewport sctx) dragging
        makeResult newState = resultReqs [IgnoreChildrenEvents] (rebuildWidget wctx newState widgetInstance prevReqs)
        result = fmap makeResult updatedState
      WheelScroll _ (Point wx wy) wheelDirection -> result where
        needsUpdate = (wx /= 0 && childWidth > vw) || (wy /= 0 && childHeight > vh)
        result = if | needsUpdate -> Just $ resultReqs [IgnoreChildrenEvents] (rebuildWidget wctx newState widgetInstance prevReqs)
                    | otherwise   -> Nothing
        stepX = wx * if wheelDirection == WheelNormal then -wheelRate else wheelRate
        stepY = wy * if wheelDirection == WheelNormal then wheelRate else -wheelRate
        newState = state {
          _scDeltaX = scrollAxis stepX dx childWidth vw,
          _scDeltaY = scrollAxis stepY dy childHeight vh
        }
      _ -> Nothing
      where
        viewport = _instanceViewport widgetInstance
        Rect vx vy vw vh = _instanceViewport widgetInstance
        sctx@ScrollContext{..} = scrollStatus wctx state viewport

    scrollAxis reqDelta currScroll childPos viewportLimit
      | reqDelta >= 0 = if currScroll + reqDelta < 0
                          then currScroll + reqDelta
                          else 0
      | otherwise = if childPos - viewportLimit + currScroll + reqDelta > 0
                      then currScroll + reqDelta
                      else viewportLimit - childPos

    handleMessage wctx ctx message widgetInstance = cast message >>= handleScrollMessage where
      handleScrollMessage (ScrollTo rect) = scrollTo wctx widgetInstance rect

    scrollTo wctx widgetInstance rect
      | rectInRect rect viewport = Nothing
      | otherwise = Just $ resultWidget newInstance
      where
        viewport = _instanceViewport widgetInstance
        Rect rx ry rw rh = rect
        Rect vx vy vw vh = viewport
        diffL = vx - rx
        diffR = vx + vw - (rx + rw)
        diffT = vy - ry
        diffB = vy + vh - (ry + rh)
        stepX = if | rectInRectH rect viewport -> dx
                   | abs diffL <= abs diffR -> diffL + dx
                   | otherwise -> diffR + dx
        stepY = if | rectInRectV rect viewport -> dy
                   | abs diffT <= abs diffB -> diffT + dy
                   | otherwise -> diffB + dy
        newState = state {
          _scDeltaX = scrollAxis stepX 0 childWidth vw,
          _scDeltaY = scrollAxis stepY 0 childHeight vh
        }
        newInstance = rebuildWidget wctx newState widgetInstance prevReqs

    updateScrollThumb state activeBar point viewport sctx = newState where
      Point px py = point
      ScrollContext{..} = sctx
      Rect rx ry rw rh = viewport
      hMid = _rw hThumbRect / 2
      vMid = _rh vThumbRect / 2
      hDelta = (rx - px + hMid) / hScrollRatio
      vDelta = (ry - py + vMid) / vScrollRatio
      newDeltaX = if activeBar == HBar then scrollAxis hDelta 0 childWidth rw else dx
      newDeltaY = if activeBar == VBar then scrollAxis vDelta 0 childHeight rh else dy
      newState = state { _scDeltaX = newDeltaX, _scDeltaY = newDeltaY }

    rebuildWidget wctx newState widgetInstance reqs = newInstance where
      newWidget = makeScroll newState
      tempInstance = widgetInstance { _instanceWidget = newWidget }
      newInstance = scrollResize (Just newWidget) wctx (_instanceViewport tempInstance) (_instanceRenderArea tempInstance) tempInstance reqs

    preferredSize renderer wctx widgetInstance childrenPairs = Node sizeReq childrenReqs where
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
        draggingH = _scDragging state == Just HBar
        draggingV = _scDragging state == Just VBar
        handleColorH = if hMouseInThumb || draggingH then gray else darkGray
        handleColorV = if vMouseInThumb || draggingV then gray else darkGray

scrollStatus :: WidgetContext s e -> ScrollState -> Rect -> ScrollContext
scrollStatus wctx scrollState viewport = ScrollContext{..} where
  ScrollState _ dx dy (Size childWidth childHeight) _ = scrollState
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
  hMouseInScroll = pointInRect mousePos hScrollRect
  vMouseInScroll = pointInRect mousePos vScrollRect
  hMouseInThumb = pointInRect mousePos hThumbRect
  vMouseInThumb = pointInRect mousePos vThumbRect
