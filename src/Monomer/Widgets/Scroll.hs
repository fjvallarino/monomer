{-# LANGUAGE RecordWildCards #-}

module Monomer.Widgets.Scroll (
  ScrollCfg,
  ScrollMessage(..),
  scroll,
  scroll_,
  barHoverColor,
  barColor,
  thumbHoverColor,
  thumbColor,
  barWidth
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~))
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Typeable

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

import qualified Monomer.Lens as L

data ActiveBar
  = HBar
  | VBar
  deriving (Eq)

data ScrollCfg = ScrollCfg {
  _scBarColor :: Maybe Color,
  _scBarHoverColor :: Maybe Color,
  _scThumbColor :: Maybe Color,
  _scThumbHoverColor :: Maybe Color,
  _scWidth :: Maybe Double
}

instance Default ScrollCfg where
  def = ScrollCfg {
    _scBarColor = Nothing,
    _scBarHoverColor = Nothing,
    _scThumbColor = Nothing,
    _scThumbHoverColor = Nothing,
    _scWidth = Nothing
  }

instance Semigroup ScrollCfg where
  (<>) t1 t2 = ScrollCfg {
    _scBarColor = _scBarColor t2 <|> _scBarColor t1,
    _scBarHoverColor = _scBarHoverColor t2 <|> _scBarHoverColor t1,
    _scThumbColor = _scThumbColor t2 <|> _scThumbColor t1,
    _scThumbHoverColor = _scThumbHoverColor t2 <|> _scThumbHoverColor t1,
    _scWidth = _scWidth t2 <|> _scWidth t1
  }

instance Monoid ScrollCfg where
  mempty = ScrollCfg {
    _scBarColor = Nothing,
    _scBarHoverColor = Nothing,
    _scThumbColor = Nothing,
    _scThumbHoverColor = Nothing,
    _scWidth = Nothing
  }

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

barColor :: Color -> ScrollCfg
barColor col = def {
  _scBarColor = Just col
}

barHoverColor :: Color -> ScrollCfg
barHoverColor col = def {
  _scBarHoverColor = Just col
}

thumbColor :: Color -> ScrollCfg
thumbColor col = def {
  _scThumbColor = Just col
}

thumbHoverColor :: Color -> ScrollCfg
thumbHoverColor col = def {
  _scThumbHoverColor = Just col
}

barWidth :: Double -> ScrollCfg
barWidth w = def {
  _scWidth = Just w
}

wheelRate :: Double
wheelRate = 10

scroll :: WidgetInstance s e -> WidgetInstance s e
scroll managedWidget = scroll_ managedWidget [def]

scroll_ :: WidgetInstance s e -> [ScrollCfg] -> WidgetInstance s e
scroll_ managed configs = makeInstance (makeScroll config def) managed where
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

  merge wenv oldState inst = resultWidget newInstance where
    newState = fromMaybe state (useState oldState)
    newInstance = inst {
      _wiWidget = makeScroll config newState
    }

  handleEvent wenv target evt inst = case evt of
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
        | jumpScrollH = updateScrollThumb state HBar point renderArea sctx
        | jumpScrollV = updateScrollThumb state VBar point renderArea sctx
        | btnReleased = state { _sstDragging = Nothing }
        | otherwise = state
      newInstance = inst {
        _wiWidget = makeScroll config newState
      }
      handledResult = Just $ resultReqs scrollReqs newInstance
      result
        | leftPressed && (hMouseInThumb || vMouseInThumb) = handledResult
        | btnReleased && (hMouseInScroll || vMouseInScroll) = handledResult
        | btnReleased && isDragging = handledResult
        | otherwise = Nothing
    Click point btn -> result where
      isDragging = isJust $ _sstDragging state
      handledResult = Just $ resultReqs scrollReqs inst
      result
        | hMouseInScroll || vMouseInScroll || isDragging = handledResult
        | otherwise = Nothing
    Move point -> result where
      drag bar = updateScrollThumb state bar point renderArea sctx
      makeWidget state = rebuildWidget wenv state inst
      makeResult state = resultReqs scrollReqs (makeWidget state)
      result = fmap (makeResult . drag) dragging
    WheelScroll _ (Point wx wy) wheelDirection -> result where
      changedX = wx /= 0 && childWidth > rw
      changedY = wy /= 0 && childHeight > rh
      needsUpdate = changedX || changedY
      makeWidget state = rebuildWidget wenv state inst
      makeResult state = resultReqs scrollReqs (makeWidget state)
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
        _sstDeltaX = scrollAxis (stepX + dx) childWidth rw,
        _sstDeltaY = scrollAxis (stepY + dy) childHeight rh
      }
    _ -> Nothing
    where
      renderArea = _wiRenderArea inst
      Rect rx ry rw rh = _wiRenderArea inst
      sctx@ScrollContext{..} = scrollStatus config wenv state inst
      scrollReqs = [IgnoreChildrenEvents, IgnoreParentEvents]

  scrollAxis reqDelta childLength vpLength
    | maxDelta == 0 = 0
    | reqDelta < 0 = max reqDelta (-maxDelta)
    | otherwise = min reqDelta 0
    where
      maxDelta = max 0 (childLength - vpLength)

  handleMessage wenv ctx message inst = result where
    handleScrollMessage (ScrollTo rect) = scrollTo wenv inst rect
    result = cast message >>= handleScrollMessage

  scrollTo wenv inst rect = result where
    renderArea = _wiRenderArea inst
    Rect rx ry rw rh = rect
    Rect vx vy vw vh = renderArea
    diffL = vx - rx
    diffR = vx + vw - (rx + rw)
    diffT = vy - ry
    diffB = vy + vh - (ry + rh)
    stepX
      | rectInRectH rect renderArea = dx
      | abs diffL <= abs diffR = diffL + dx
      | otherwise = diffR + dx
    stepY
      | rectInRectV rect renderArea = dy
      | abs diffT <= abs diffB = diffT + dy
      | otherwise = diffB + dy
    newState = state {
      _sstDeltaX = scrollAxis stepX childWidth vw,
      _sstDeltaY = scrollAxis stepY childHeight vh
    }
    newInstance = rebuildWidget wenv newState inst
    result
      | rectInRect rect renderArea = Nothing
      | otherwise = Just $ resultWidget newInstance

  updateScrollThumb state activeBar point renderArea sctx = newState where
    Point px py = point
    ScrollContext{..} = sctx
    Rect rx ry rw rh = renderArea
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

  rebuildWidget wenv newState inst = newInst where
    newWidget = makeScroll config newState
    tempInst = inst { _wiWidget = newWidget }
    vp = _wiViewport tempInst
    ra = _wiRenderArea tempInst
    newInst = scrollResize (Just newWidget) newState wenv vp ra tempInst

  getSizeReq wenv inst children = sizeReq where
    child = Seq.index children 0
    w = getMinSizeReq $ _wiSizeReqW child
    h = getMinSizeReq $ _wiSizeReqH child
    factor = 1

    sizeReq = (FlexSize w factor, FlexSize h factor)

  scrollResize uWidget state wenv viewport renderArea inst = newInst where
    Rect l t w h = renderArea
    dx = _sstDeltaX state
    dy = _sstDeltaY state

    child = Seq.index (_wiChildren inst) 0
    childWidth2 = getMinSizeReq $ _wiSizeReqW child
    childHeight2 = getMinSizeReq $ _wiSizeReqH child

    areaW = max w childWidth2
    areaH = max h childHeight2
    newDx = scrollAxis dx areaW w
    newDy = scrollAxis dy areaH h
    cRenderArea = Rect (l + newDx) (t + newDy) areaW areaH

    defWidget = makeScroll config $ state {
      _sstChildSize = Size areaW areaH
    }
    newWidget = fromMaybe defWidget uWidget
    cWidget = _wiWidget child
    tempChild = widgetResize cWidget wenv viewport cRenderArea child
    newChild = tempChild {
      _wiViewport = viewport,
      _wiRenderArea = cRenderArea
    }

    newInst = inst {
      _wiViewport = viewport,
      _wiRenderArea = renderArea,
      _wiWidget = newWidget,
      _wiChildren = Seq.singleton newChild
    }

  render renderer wenv inst = do
    drawInScissor renderer True viewport $
      renderWrapper defaultRender renderer wenv inst

    when hScrollRequired $
      drawRect renderer hScrollRect barColorH Nothing

    when vScrollRequired $
      drawRect renderer vScrollRect barColorV Nothing

    when hScrollRequired $
      drawRect renderer hThumbRect thumbColorH Nothing

    when vScrollRequired $
      drawRect renderer vThumbRect thumbColorV Nothing

    where
      theme = wenv ^. L.theme
      viewport = _wiViewport inst
      renderArea = _wiRenderArea inst
      ScrollContext{..} = scrollStatus config wenv state inst
      draggingH = _sstDragging state == Just HBar
      draggingV = _sstDragging state == Just VBar
      instBarBCol = _scBarColor config
      instBarHCol = _scBarHoverColor config
      instThumbBCol = _scThumbColor config
      instThumbHCol = _scThumbHoverColor config

      barBCol = instBarBCol <|> Just (theme ^. L.basic . L.scrollBarColor)
      barHCol = instBarHCol <|> Just (theme ^. L.hover . L.scrollBarColor)
      thumbBCol = instThumbBCol <|> Just (theme ^. L.basic . L.scrollThumbColor)
      thumbHCol = instThumbHCol <|> Just (theme ^. L.hover. L.scrollThumbColor)

      barColorH
        | hMouseInScroll = barHCol
        | otherwise = barBCol
      barColorV
        | vMouseInScroll = barHCol
        | otherwise = barBCol
      thumbColorH
        | hMouseInThumb || draggingH = thumbHCol
        | otherwise = thumbBCol
      thumbColorV
        | vMouseInThumb || draggingV = thumbHCol
        | otherwise = thumbBCol

scrollStatus
  :: ScrollCfg
  -> WidgetEnv s e
  -> ScrollState
  -> WidgetInstance s e
  -> ScrollContext
scrollStatus config wenv scrollState inst = ScrollContext{..} where
  ScrollState _ dx dy (Size childWidth childHeight) = scrollState
  mousePos = _ipsMousePos (_weInputStatus wenv)
  theme = activeTheme wenv inst
  renderArea = _wiRenderArea inst
  barW = fromMaybe (theme ^. L.scrollWidth) (_scWidth config)
  raLeft = _rX renderArea
  raTop = _rY renderArea
  raWidth = _rW renderArea
  raHeight = _rH renderArea
  hScrollTop = raHeight - barW
  vScrollLeft = raWidth - barW
  hRatio = raWidth / childWidth
  vRatio = raHeight / childHeight
  hRatioR = (raWidth - barW) / childWidth
  vRatioR = (raHeight - barW) / childHeight
  (hScrollRatio, vScrollRatio)
    | hRatio < 1 && vRatio < 1 = (hRatioR, vRatioR)
    | otherwise = (hRatio, vRatio)
  hScrollRequired = hScrollRatio < 1
  vScrollRequired = vScrollRatio < 1
  hScrollRect = Rect {
    _rX = raLeft,
    _rY = raTop + hScrollTop,
    _rW = raLeft + raWidth,
    _rH = raTop + raHeight
  }
  vScrollRect = Rect {
    _rX = raLeft + vScrollLeft,
    _rY = raTop,
    _rW = raLeft + raWidth,
    _rH = raTop + raHeight
  }
  hThumbRect = Rect {
    _rX = raLeft - hScrollRatio * dx,
    _rY = raTop + hScrollTop,
    _rW = hScrollRatio * raWidth,
    _rH = barW
  }
  vThumbRect = Rect {
    _rX = raLeft + vScrollLeft,
    _rY = raTop - vScrollRatio * dy,
    _rW = barW,
    _rH = vScrollRatio * raHeight
  }
  hMouseInScroll = pointInRect mousePos hScrollRect
  vMouseInScroll = pointInRect mousePos vScrollRect
  hMouseInThumb = pointInRect mousePos hThumbRect
  vMouseInThumb = pointInRect mousePos vThumbRect
