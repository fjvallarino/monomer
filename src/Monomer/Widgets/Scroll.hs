{-# LANGUAGE RecordWildCards #-}

module Monomer.Widgets.Scroll (
  ScrollCfg,
  ScrollMessage(..),
  scroll,
  activeBarColor,
  idleBarColor,
  activeThumbColor,
  idleThumbColor
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (.~))
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Typeable

import qualified Data.Sequence as Seq

import Monomer.Lens
import Monomer.Widgets.Container

data ActiveBar
  = HBar
  | VBar
  deriving (Eq)

data ScrollCfg = ScrollCfg {
  _scActiveBarColor :: Maybe Color,
  _scIdleBarColor :: Maybe Color,
  _scActiveThumbColor :: Maybe Color,
  _scIdleThumbColor :: Maybe Color
}

instance Default ScrollCfg where
  def = ScrollCfg {
    _scActiveBarColor = Just $ darkGray & a .~ 0.4,
    _scIdleBarColor = Nothing,
    _scActiveThumbColor = Just gray,
    _scIdleThumbColor = Just darkGray
  }

instance Semigroup ScrollCfg where
  (<>) t1 t2 = ScrollCfg {
    _scActiveBarColor = _scActiveBarColor t2 <|> _scActiveBarColor t1,
    _scIdleBarColor = _scIdleBarColor t2 <|> _scIdleBarColor t1,
    _scActiveThumbColor = _scActiveThumbColor t2 <|> _scActiveThumbColor t1,
    _scIdleThumbColor = _scIdleThumbColor t2 <|> _scIdleThumbColor t1
  }

instance Monoid ScrollCfg where
  mempty = def

data ScrollState = ScrollState {
  _sstDragging :: Maybe ActiveBar,
  _sstDeltaX :: !Double,
  _sstDeltaY :: !Double,
  _sstChildSize :: Size
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

instance Default ScrollState where
  def = ScrollState {
    _sstDragging = Nothing,
    _sstDeltaX = 0,
    _sstDeltaY = 0,
    _sstChildSize = def
  }

activeBarColor :: Color -> ScrollCfg
activeBarColor col = def {
  _scActiveBarColor = Just col
}

idleBarColor :: Color -> ScrollCfg
idleBarColor col = def {
  _scIdleBarColor = Just col
}

activeThumbColor :: Color -> ScrollCfg
activeThumbColor col = def {
  _scActiveThumbColor = Just col
}

idleThumbColor :: Color -> ScrollCfg
idleThumbColor col = def {
  _scIdleThumbColor = Just col
}

barThickness :: Double
barThickness = 10

wheelRate :: Double
wheelRate = 10

scroll :: WidgetInstance s e -> WidgetInstance s e
scroll managedWidget = scroll_ def managedWidget

scroll_ :: [ScrollCfg] -> WidgetInstance s e -> WidgetInstance s e
scroll_ configs managed = makeInstance (makeScroll config def) managed where
  config = mconcat configs

makeInstance :: Widget s e -> WidgetInstance s e -> WidgetInstance s e
makeInstance widget managedWidget = (defaultWidgetInstance "scroll" widget) {
  _wiChildren = Seq.singleton managedWidget,
  _wiFocusable = False
}

makeScroll :: ScrollCfg -> ScrollState -> Widget s e
makeScroll config state = widget where
  baseWidget = createContainer def {
    containerGetState = makeState state,
    containerMerge = merge,
    containerHandleEvent = handleEvent,
    containerHandleMessage = handleMessage,
    containerGetSizeReq = getSizeReq
  }
  widget = baseWidget {
    widgetResize = scrollResize Nothing state,
    widgetRender = render
  }

  ScrollState dragging dx dy cs = state
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
      makeWidget state = rebuildWidget wenv state widgetInst
      makeResult state = resultReqs [IgnoreChildrenEvents] (makeWidget state)
      result = fmap (makeResult . drag) dragging
    WheelScroll _ (Point wx wy) wheelDirection -> result where
      changedX = wx /= 0 && childWidth > vw
      changedY = wy /= 0 && childHeight > vh
      needsUpdate = changedX || changedY
      makeWidget state = rebuildWidget wenv state widgetInst
      makeResult state = resultReqs [IgnoreChildrenEvents] (makeWidget state)
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
        _sstDeltaX = scrollAxis (stepX + dx) childWidth vw,
        _sstDeltaY = scrollAxis (stepY + dy) childHeight vh
      }
    _ -> Nothing
    where
      viewport = _wiViewport widgetInst
      Rect vx vy vw vh = _wiViewport widgetInst
      sctx@ScrollContext{..} = scrollStatus config wenv state viewport

  scrollAxis reqDelta childLength vpLength
    | maxDelta == 0 = 0
    | reqDelta < 0 = max reqDelta (-maxDelta)
    | otherwise = min reqDelta 0
    where
      maxDelta = max 0 (childLength - vpLength)

  handleMessage wenv ctx message widgetInst = result where
    handleScrollMessage (ScrollTo rect) = scrollTo wenv widgetInst rect
    result = cast message >>= handleScrollMessage

  scrollTo wenv widgetInst rect = result where
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
      _sstDeltaX = scrollAxis stepX childWidth vw,
      _sstDeltaY = scrollAxis stepY childHeight vh
    }
    newInstance = rebuildWidget wenv newState widgetInst
    result
      | rectInRect rect viewport = Nothing
      | otherwise = Just $ resultWidget newInstance

  updateScrollThumb state activeBar point viewport sctx = newState where
    Point px py = point
    ScrollContext{..} = sctx
    Rect rx ry rw rh = viewport
    hMid = _rW hThumbRect / 2
    vMid = _rH vThumbRect / 2
    hDelta = (rx - px + hMid) / hScrollRatio
    vDelta = (ry - py + vMid) / vScrollRatio
    newDeltaX
      | activeBar == HBar = scrollAxis hDelta childWidth rw
      | otherwise = dx
    newDeltaY
      | activeBar == VBar = scrollAxis vDelta childHeight rh
      | otherwise = dy
    newState = state {
      _sstDeltaX = newDeltaX,
      _sstDeltaY = newDeltaY
    }

  rebuildWidget wenv newState widgetInst = newInst where
    newWidget = makeScroll config newState
    tempInst = widgetInst { _wiWidget = newWidget }
    vp = _wiViewport tempInst
    ra = _wiRenderArea tempInst
    newInst = scrollResize (Just newWidget) newState wenv vp ra tempInst

  getSizeReq wenv widgetInst children = sizeReq where
    child = Seq.index children 0
    w = getMinSizeReq $ _wiSizeReqW child
    h = getMinSizeReq $ _wiSizeReqH child
    factor = 1

    sizeReq = (FlexSize w factor, FlexSize h factor)

  scrollResize uWidget state wenv viewport renderArea widgetInst = newInst where
    Rect l t w h = renderArea
    dx = _sstDeltaX state
    dy = _sstDeltaY state

    child = Seq.index (_wiChildren widgetInst) 0
    childWidth2 = getMinSizeReq $ _wiSizeReqW child
    childHeight2 = getMinSizeReq $ _wiSizeReqH child

    areaW = max w childWidth2
    areaH = max h childHeight2
    cRenderArea = Rect (l + dx) (t + dy) areaW areaH

    defWidget = makeScroll config $ state {
      _sstChildSize = Size areaW areaH
    }
    newWidget = fromMaybe defWidget uWidget
    cWidget = _wiWidget child
    newChild = widgetResize cWidget wenv viewport cRenderArea child

    newInst = widgetInst {
      _wiViewport = viewport,
      _wiRenderArea = renderArea,
      _wiWidget = newWidget,
      _wiChildren = Seq.singleton newChild
    }

  render renderer wenv widgetInst = do
    setScissor renderer viewport
    renderWrapper defaultRender renderer wenv widgetInst
    resetScissor renderer

    when hScrollRequired $
      drawRect renderer hScrollRect barColorH Nothing

    when vScrollRequired $
      drawRect renderer vScrollRect barColorV Nothing

    when hScrollRequired $
      drawRect renderer hThumbRect thumbColorH Nothing

    when vScrollRequired $
      drawRect renderer vThumbRect thumbColorV Nothing

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
  :: ScrollCfg -> WidgetEnv s e -> ScrollState -> Rect -> ScrollContext
scrollStatus config wenv scrollState viewport = ScrollContext{..} where
  ScrollState _ dx dy (Size childWidth childHeight) = scrollState
  mousePos = _ipsMousePos (_weInputStatus wenv)
  vpLeft = _rX viewport
  vpTop = _rY viewport
  vpWidth = _rW viewport
  vpHeight = _rH viewport
  hScrollTop = vpHeight - barThickness
  vScrollLeft = vpWidth - barThickness
  hScrollRatio = min (vpWidth / childWidth) 1
  vScrollRatio = min (vpHeight / childHeight) 1
  hScrollRequired = hScrollRatio < 1
  vScrollRequired = vScrollRatio < 1
  hScrollRect = Rect {
    _rX = vpLeft,
    _rY = vpTop + hScrollTop,
    _rW = vpLeft + vpWidth,
    _rH = vpTop + vpHeight
  }
  vScrollRect = Rect {
    _rX = vpLeft + vScrollLeft,
    _rY = vpTop,
    _rW = vpLeft + vpWidth,
    _rH = vpTop + vpHeight
  }
  hThumbRect = Rect {
    _rX = vpLeft - hScrollRatio * dx,
    _rY = vpTop + hScrollTop,
    _rW = hScrollRatio * vpWidth,
    _rH = barThickness
  }
  vThumbRect = Rect {
    _rX = vpLeft + vScrollLeft,
    _rY = vpTop - vScrollRatio * dy,
    _rW = barThickness,
    _rH = vScrollRatio * vpHeight
  }
  hMouseInScroll = pointInRect mousePos hScrollRect
  vMouseInScroll = pointInRect mousePos vScrollRect
  hMouseInThumb = pointInRect mousePos hThumbRect
  vMouseInThumb = pointInRect mousePos vThumbRect
