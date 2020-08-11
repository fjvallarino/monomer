{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Scroll (
  ScrollMessage(..),
  scroll,
  scrollConfig
) where

import Control.Lens ((&), (.~))
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

data ActiveBar
  = HBar
  | VBar
  deriving (Eq)

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

newtype ScrollMessage
  = ScrollTo Rect
  deriving Typeable

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

scrollConfig :: ScrollConfig
scrollConfig = ScrollConfig {
  _scActiveBarColor = Just $ darkGray & alpha .~ 0.4,
  _scIdleBarColor = Nothing,
  _scActiveThumbColor = gray,
  _scIdleThumbColor = darkGray,
  _scBarThickness = 10,
  _scWheelRate = 10
}

defaultState :: ScrollState
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
scroll_ config managed = makeInstance (makeScroll config defaultState) managed

makeInstance :: Widget s e -> WidgetInstance s e -> WidgetInstance s e
makeInstance widget managedWidget = (defaultWidgetInstance "scroll" widget) {
  _wiChildren = Seq.singleton managedWidget,
  _wiFocusable = False
}

makeScroll :: ScrollConfig -> ScrollState -> Widget s e
makeScroll config state = widget where
  widget = createContainer {
    widgetGetState = makeState state,
    widgetMerge = containerMergeTrees merge,
    widgetHandleEvent = containerHandleEvent handleEvent,
    widgetHandleMessage = containerHandleMessage handleMessage,
    widgetPreferredSize = containerPreferredSize preferredSize,
    widgetResize = scrollResize Nothing,
    widgetRender = render
  }

  ScrollState dragging dx dy cs prevReqs = state
  Size childWidth childHeight = cs

  merge wenv oldState widgetInst = resultWidget newInstance where
    newState = fromMaybe state (useState oldState)
    newInstance = widgetInst {
      _wiWidget = makeScroll config newState
    }

  handleEvent wenv target evt widgetInst = case evt of
    ButtonAction point btn status -> result where
      leftPressed = status == PressedBtn && btn == LeftBtn
      btnReleased = status == ReleasedBtn
      isDragging = isJust $ _sstDragging state
      startDrag = leftPressed && not isDragging
      jumpScrollH = btnReleased && not isDragging && hMouseInScroll
      jumpScrollV = btnReleased && not isDragging && vMouseInScroll
      newState
        | startDrag && hMouseInThumb = state { _sstDragging = Just HBar }
        | startDrag && vMouseInThumb = state { _sstDragging = Just VBar }
        | jumpScrollH = updateScrollThumb state HBar point viewport sctx
        | jumpScrollV = updateScrollThumb state VBar point viewport sctx
        | btnReleased = state { _sstDragging = Nothing }
        | otherwise = state
      newInstance = widgetInst {
        _wiWidget = makeScroll config newState
      }
      handledResult = Just $ resultReqs [IgnoreChildrenEvents] newInstance
      result
        | leftPressed && (hMouseInThumb || vMouseInThumb) = handledResult
        | btnReleased && (hMouseInScroll || vMouseInScroll) = handledResult
        | btnReleased && isDragging = handledResult
        | otherwise = Nothing
    Click point btn -> result where
      isDragging = isJust $ _sstDragging state
      handledResult = Just $ resultReqs [IgnoreChildrenEvents] widgetInst
      result
        | hMouseInScroll || vMouseInScroll || isDragging = handledResult
        | otherwise = Nothing
    Move point -> result where
      drag bar = updateScrollThumb state bar point viewport sctx
      makeWidget state = rebuildWidget wenv state widgetInst prevReqs
      makeResult state = resultReqs [IgnoreChildrenEvents] (makeWidget state)
      result = fmap (makeResult . drag) dragging
    WheelScroll _ (Point wx wy) wheelDirection -> result where
      changedX = wx /= 0 && childWidth > vw
      changedY = wy /= 0 && childHeight > vh
      needsUpdate = changedX || changedY
      makeWidget state = rebuildWidget wenv state widgetInst prevReqs
      makeResult state = resultReqs [IgnoreChildrenEvents] (makeWidget state)
      wheelRate = _scWheelRate config
      result
        | needsUpdate = Just $ makeResult newState
        | otherwise = Nothing
      stepX
        | wheelDirection == WheelNormal = -wheelRate * wx
        | otherwise = wheelRate * wx
      stepY
        | wheelDirection == WheelNormal = wheelRate * wy
        | otherwise = -wheelRate * wy
      newState = state {
        _sstDeltaX = scrollAxis stepX dx childWidth vw,
        _sstDeltaY = scrollAxis stepY dy childHeight vh
      }
    _ -> Nothing
    where
      viewport = _wiViewport widgetInst
      Rect vx vy vw vh = _wiViewport widgetInst
      sctx@ScrollContext{..} = scrollStatus config wenv state viewport

  scrollAxis reqDelta currScroll childPos vpLimit
    | reqDelta >= 0 && currScroll + reqDelta < 0 = currScroll + reqDelta
    | reqDelta >= 0 = 0
    | childPos - vpLimit + currScroll + reqDelta > 0 = currScroll + reqDelta
    | otherwise = vpLimit - childPos

  handleMessage wenv ctx message widgetInst = result where
    handleScrollMessage (ScrollTo rect) = scrollTo wenv widgetInst rect
    result = cast message >>= handleScrollMessage

  scrollTo wenv widgetInst rect
    | rectInRect rect viewport = Nothing
    | otherwise = Just $ resultWidget newInstance
    where
      viewport = _wiViewport widgetInst
      Rect rx ry rw rh = rect
      Rect vx vy vw vh = viewport
      diffL = vx - rx
      diffR = vx + vw - (rx + rw)
      diffT = vy - ry
      diffB = vy + vh - (ry + rh)
      stepX
        | rectInRectH rect viewport = dx
        | abs diffL <= abs diffR = diffL + dx
        | otherwise = diffR + dx
      stepY
        | rectInRectV rect viewport = dy
        | abs diffT <= abs diffB = diffT + dy
        | otherwise = diffB + dy
      newState = state {
        _sstDeltaX = scrollAxis stepX 0 childWidth vw,
        _sstDeltaY = scrollAxis stepY 0 childHeight vh
      }
      newInstance = rebuildWidget wenv newState widgetInst prevReqs

  updateScrollThumb state activeBar point viewport sctx = newState where
    Point px py = point
    ScrollContext{..} = sctx
    Rect rx ry rw rh = viewport
    hMid = _rw hThumbRect / 2
    vMid = _rh vThumbRect / 2
    hDelta = (rx - px + hMid) / hScrollRatio
    vDelta = (ry - py + vMid) / vScrollRatio
    newDeltaX
      | activeBar == HBar = scrollAxis hDelta 0 childWidth rw
      | otherwise = dx
    newDeltaY
      | activeBar == VBar = scrollAxis vDelta 0 childHeight rh
      | otherwise = dy
    newState = state {
      _sstDeltaX = newDeltaX,
      _sstDeltaY = newDeltaY
    }

  rebuildWidget wenv newState widgetInst reqs = newInst where
    newWidget = makeScroll config newState
    tempInst = widgetInst { _wiWidget = newWidget }
    widget = _wiViewport tempInst
    renderArea = _wiRenderArea tempInst
    newInst = scrollResize (Just newWidget) wenv widget renderArea reqs tempInst

  preferredSize wenv widgetInst children reqs = Node sizeReq reqs where
    size = _srSize . nodeValue $ Seq.index reqs 0
    sizeReq = SizeReq size FlexibleSize FlexibleSize

  scrollResize uWidget wenv viewport renderArea reqs widgetInst = newInst where
    Rect l t w h = renderArea
    child = Seq.index (_wiChildren widgetInst) 0
    childReq = fromMaybe (singleNode def) (Seq.lookup 0 (nodeChildren reqs))

    Size childWidth2 childHeight2 = _srSize $ nodeValue childReq
    areaW = max w childWidth2
    areaH = max h childHeight2
    cRenderArea = Rect (l + dx) (t + dy) areaW areaH

    defWidget = makeScroll config $ state {
      _sstChildSize = Size areaW areaH,
      _sstReqSize = reqs
    }
    newWidget = fromMaybe defWidget uWidget
    cWidget = _wiWidget child
    newChild = widgetResize cWidget wenv viewport cRenderArea childReq child

    newInst = widgetInst {
      _wiViewport = viewport,
      _wiRenderArea = renderArea,
      _wiWidget = newWidget,
      _wiChildren = Seq.singleton newChild
    }

  render renderer wenv widgetInst = do
    setScissor renderer viewport
    containerRender defaultContainerRender renderer wenv widgetInst
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
      viewport = _wiViewport widgetInst
      ScrollContext{..} = scrollStatus config wenv state viewport
      draggingH = _sstDragging state == Just HBar
      draggingV = _sstDragging state == Just VBar
      barColorH
        | hMouseInScroll = _scActiveBarColor config
        | otherwise = _scIdleBarColor config
      barColorV
        | vMouseInScroll = _scActiveBarColor config
        | otherwise = _scIdleBarColor config
      thumbColorH
        | hMouseInThumb || draggingH = _scActiveThumbColor config
        | otherwise =  _scIdleThumbColor config
      thumbColorV
        | vMouseInThumb || draggingV = _scActiveThumbColor config
        | otherwise = _scIdleThumbColor config

scrollStatus
  :: ScrollConfig -> WidgetEnv s e -> ScrollState -> Rect -> ScrollContext
scrollStatus config wenv scrollState viewport = ScrollContext{..} where
  ScrollState _ dx dy (Size childWidth childHeight) _ = scrollState
  barThickness = _scBarThickness config
  mousePos = _ipsMousePos (_weInputStatus wenv)
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
  hScrollRect = Rect {
    _rx = vpLeft,
    _ry = vpTop + hScrollTop,
    _rw = vpLeft + vpWidth,
    _rh = vpTop + vpHeight
  }
  vScrollRect = Rect {
    _rx = vpLeft + vScrollLeft,
    _ry = vpTop,
    _rw = vpLeft + vpWidth,
    _rh = vpTop + vpHeight
  }
  hThumbRect = Rect {
    _rx = vpLeft - hScrollRatio * dx,
    _ry = vpTop + hScrollTop,
    _rw = hScrollRatio * vpWidth,
    _rh = barThickness
  }
  vThumbRect = Rect {
    _rx = vpLeft + vScrollLeft,
    _ry = vpTop - vScrollRatio * dy,
    _rw = barThickness,
    _rh = vScrollRatio * vpHeight
  }
  hMouseInScroll = pointInRect mousePos hScrollRect
  vMouseInScroll = pointInRect mousePos vScrollRect
  hMouseInThumb = pointInRect mousePos hThumbRect
  vMouseInThumb = pointInRect mousePos vThumbRect
