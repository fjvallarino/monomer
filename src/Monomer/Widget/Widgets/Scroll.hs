{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Scroll (
  ScrollMessage(..),
  scroll,
  scrollConfig
) where

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
import Monomer.Widget.Types
import Monomer.Widget.Util

data ActiveBar = HBar | VBar deriving (Eq)

data ScrollConfig = ScrollConfig {
  _scActiveBarColor :: Maybe Color,
  _scIdleBarColor :: Maybe Color,
  _scActiveThumbColor :: Color,
  _scIdleThumbColor :: Color,
  _scBarThickness :: Double,
  _scWheelRate :: Double
}

data ScrollState = ScrollState {
  _sstDragging :: Maybe ActiveBar,
  _sstDeltaX :: !Double,
  _sstDeltaY :: !Double,
  _sstChildSize :: Size,
  _sstReqSize :: Tree SizeReq
} deriving (Typeable)

newtype ScrollMessage = ScrollTo Rect deriving Typeable

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

scrollConfig = ScrollConfig {
  _scActiveBarColor = Just $ darkGray { _alpha = 0.4 },
  _scIdleBarColor = Nothing,
  _scActiveThumbColor = gray,
  _scIdleThumbColor = darkGray,
  _scBarThickness = 10,
  _scWheelRate = 10
}

defaultState = ScrollState {
  _sstDragging = Nothing,
  _sstDeltaX = 0,
  _sstDeltaY = 0,
  _sstChildSize = def,
  _sstReqSize = singleNode def
}

scroll :: WidgetInstance s e -> WidgetInstance s e
scroll managedWidget = scroll_ scrollConfig managedWidget

scroll_ :: ScrollConfig -> WidgetInstance s e -> WidgetInstance s e
scroll_ config managedWidget = makeInstance (makeScroll config defaultState) managedWidget

makeInstance :: Widget s e -> WidgetInstance s e -> WidgetInstance s e
makeInstance widget managedWidget = (defaultWidgetInstance "scroll" widget) {
  _instanceChildren = Seq.singleton managedWidget,
  _instanceFocusable = False
}

makeScroll :: ScrollConfig -> ScrollState -> Widget s e
makeScroll config state@(ScrollState dragging dx dy cs prevReqs) = createContainer {
    _widgetGetState = makeState state,
    _widgetMerge = containerMergeTrees merge,
    _widgetHandleEvent = containerHandleEvent handleEvent,
    _widgetHandleMessage = containerHandleMessage handleMessage,
    _widgetPreferredSize = containerPreferredSize preferredSize,
    _widgetResize = scrollResize Nothing,
    _widgetRender = render
  }
  where
    Size childWidth childHeight = cs

    merge wenv oldState widgetInstance = resultWidget newInstance where
      newState = fromMaybe state (useState oldState)
      newInstance = widgetInstance {
        _instanceWidget = makeScroll config newState
      }

    handleEvent wenv target evt widgetInstance = case evt of
      ButtonAction point btn status -> result where
        isLeftPressed = status == PressedBtn && btn == LeftBtn
        isButtonReleased = status == ReleasedBtn
        isDragging = isJust $ _sstDragging state
        newState = if | isLeftPressed && hMouseInThumb && not isDragging -> state { _sstDragging = Just HBar }
                      | isLeftPressed && vMouseInThumb && not isDragging -> state { _sstDragging = Just VBar }
                      | isButtonReleased && hMouseInScroll && not isDragging -> updateScrollThumb state HBar point viewport sctx
                      | isButtonReleased && vMouseInScroll && not isDragging -> updateScrollThumb state VBar point viewport sctx
                      | isButtonReleased -> state { _sstDragging = Nothing }
                      | otherwise -> state
        newInstance = widgetInstance {
          _instanceWidget = makeScroll config newState
        }
        handledResult = Just $ resultReqs [IgnoreChildrenEvents] newInstance
        result = if | isLeftPressed && (hMouseInThumb || vMouseInThumb) -> handledResult
                    | isButtonReleased && (hMouseInScroll || vMouseInScroll) -> handledResult
                    | isButtonReleased && isDragging -> handledResult
                    | otherwise -> Nothing
      Click point btn -> result where
        isDragging = isJust $ _sstDragging state
        handledResult = Just $ resultReqs [IgnoreChildrenEvents] widgetInstance
        result = if | hMouseInScroll || vMouseInScroll || isDragging -> handledResult
                    | otherwise -> Nothing
      Move point -> result where
        updatedState = fmap (\dg -> updateScrollThumb state dg point viewport sctx) dragging
        makeResult newState = resultReqs [IgnoreChildrenEvents] (rebuildWidget wenv newState widgetInstance prevReqs)
        result = fmap makeResult updatedState
      WheelScroll _ (Point wx wy) wheelDirection -> result where
        needsUpdate = (wx /= 0 && childWidth > vw) || (wy /= 0 && childHeight > vh)
        result = if | needsUpdate -> Just $ resultReqs [IgnoreChildrenEvents] (rebuildWidget wenv newState widgetInstance prevReqs)
                    | otherwise   -> Nothing
        wheelRate = _scWheelRate config
        stepX = wx * if wheelDirection == WheelNormal then -wheelRate else wheelRate
        stepY = wy * if wheelDirection == WheelNormal then wheelRate else -wheelRate
        newState = state {
          _sstDeltaX = scrollAxis stepX dx childWidth vw,
          _sstDeltaY = scrollAxis stepY dy childHeight vh
        }
      _ -> Nothing
      where
        viewport = _instanceViewport widgetInstance
        Rect vx vy vw vh = _instanceViewport widgetInstance
        sctx@ScrollContext{..} = scrollStatus config wenv state viewport

    scrollAxis reqDelta currScroll childPos viewportLimit
      | reqDelta >= 0 = if currScroll + reqDelta < 0
                          then currScroll + reqDelta
                          else 0
      | otherwise = if childPos - viewportLimit + currScroll + reqDelta > 0
                      then currScroll + reqDelta
                      else viewportLimit - childPos

    handleMessage wenv ctx message widgetInstance = cast message >>= handleScrollMessage where
      handleScrollMessage (ScrollTo rect) = scrollTo wenv widgetInstance rect

    scrollTo wenv widgetInstance rect
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
          _sstDeltaX = scrollAxis stepX 0 childWidth vw,
          _sstDeltaY = scrollAxis stepY 0 childHeight vh
        }
        newInstance = rebuildWidget wenv newState widgetInstance prevReqs

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
      newState = state { _sstDeltaX = newDeltaX, _sstDeltaY = newDeltaY }

    rebuildWidget wenv newState widgetInstance reqs = newInstance where
      newWidget = makeScroll config newState
      tempInstance = widgetInstance { _instanceWidget = newWidget }
      newInstance = scrollResize (Just newWidget) wenv (_instanceViewport tempInstance) (_instanceRenderArea tempInstance) tempInstance reqs

    preferredSize wenv widgetInstance children reqs = Node sizeReq reqs where
      sizeReq = SizeReq (_sizeRequested . nodeValue $ Seq.index reqs 0) FlexibleSize FlexibleSize

    scrollResize updatedWidget wenv viewport renderArea widgetInstance reqs = newInstance where
      Rect l t w h = renderArea
      child = Seq.index (_instanceChildren widgetInstance) 0
      childReq = fromMaybe (singleNode def) (Seq.lookup 0 (nodeChildren reqs))

      Size childWidth2 childHeight2 = _sizeRequested $ nodeValue childReq
      areaW = max w childWidth2
      areaH = max h childHeight2
      childRenderArea = Rect (l + dx) (t + dy) areaW areaH

      newWidget = fromMaybe (makeScroll config $ state { _sstChildSize = Size areaW areaH, _sstReqSize = reqs }) updatedWidget
      newChildWidget = _widgetResize (_instanceWidget child) wenv viewport childRenderArea child childReq

      newInstance = widgetInstance {
        _instanceViewport = viewport,
        _instanceRenderArea = renderArea,
        _instanceWidget = newWidget,
        _instanceChildren = Seq.singleton newChildWidget
      }

    render renderer wenv widgetInstance =
      do
        setScissor renderer viewport
        containerRender defaultContainerRender renderer wenv widgetInstance
        resetScissor renderer

        when hScrollRequired $
          drawRect renderer hScrollRect barColorH Nothing

        when vScrollRequired $
          drawRect renderer vScrollRect barColorV Nothing

        when hScrollRequired $
          drawRect renderer hThumbRect (Just thumbColorH) Nothing

        when vScrollRequired $
          drawRect renderer vThumbRect (Just thumbColorV) Nothing
      where
        viewport = _instanceViewport widgetInstance
        ScrollContext{..} = scrollStatus config wenv state viewport
        draggingH = _sstDragging state == Just HBar
        draggingV = _sstDragging state == Just VBar
        barColorH = if hMouseInScroll then _scActiveBarColor config else _scIdleBarColor config
        barColorV = if vMouseInScroll then _scActiveBarColor config else _scIdleBarColor config
        thumbColorH = if hMouseInThumb || draggingH then _scActiveThumbColor config else _scIdleThumbColor config
        thumbColorV = if vMouseInThumb || draggingV then _scActiveThumbColor config else _scIdleThumbColor config

scrollStatus :: ScrollConfig -> WidgetEnv s e -> ScrollState -> Rect -> ScrollContext
scrollStatus config wenv scrollState viewport = ScrollContext{..} where
  ScrollState _ dx dy (Size childWidth childHeight) _ = scrollState
  barThickness = _scBarThickness config
  mousePos = statusMousePos (_weInputStatus wenv)
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
