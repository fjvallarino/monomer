{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  scrollStyle,
  barWidth
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (.~), (^?!), cloneLens, ix)
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
  _scStyle :: Maybe (ALens' ThemeState StyleState),
  _scWidth :: Maybe Double
}

instance Default ScrollCfg where
  def = ScrollCfg {
    _scBarColor = Nothing,
    _scBarHoverColor = Nothing,
    _scThumbColor = Nothing,
    _scThumbHoverColor = Nothing,
    _scStyle = Nothing,
    _scWidth = Nothing
  }

instance Semigroup ScrollCfg where
  (<>) t1 t2 = ScrollCfg {
    _scBarColor = _scBarColor t2 <|> _scBarColor t1,
    _scBarHoverColor = _scBarHoverColor t2 <|> _scBarHoverColor t1,
    _scThumbColor = _scThumbColor t2 <|> _scThumbColor t1,
    _scThumbHoverColor = _scThumbHoverColor t2 <|> _scThumbHoverColor t1,
    _scStyle = _scStyle t2 <|> _scStyle t1,
    _scWidth = _scWidth t2 <|> _scWidth t1
  }

instance Monoid ScrollCfg where
  mempty = ScrollCfg {
    _scBarColor = Nothing,
    _scBarHoverColor = Nothing,
    _scThumbColor = Nothing,
    _scThumbHoverColor = Nothing,
    _scStyle = Nothing,
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

scrollStyle :: ALens' ThemeState StyleState -> ScrollCfg
scrollStyle style = def {
  _scStyle = Just style
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
    containerGetBaseStyle = getBaseStyle,
    containerGetState = makeState state,
    containerMerge = merge,
    containerHandleMessage = handleMessage,
    containerGetSizeReq = getSizeReq
  }
  widget = baseWidget {
    widgetHandleEvent = handleEventWrapper True handleEvent,
    widgetResize = scrollResize Nothing state,
    widgetRender = render
  }

  ScrollState dragging dx dy cs = state
  Size childWidth childHeight = cs

  getBaseStyle wenv inst = _scStyle config >>= handler where
    handler lstyle = Just $ collectTheme wenv (cloneLens lstyle)

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
        | jumpScrollH = updateScrollThumb state HBar point contentArea sctx
        | jumpScrollV = updateScrollThumb state VBar point contentArea sctx
        | btnReleased = state { _sstDragging = Nothing }
        | otherwise = state
      newInstance = rebuildWidget wenv newState inst
      handledResult = Just $ resultReqs scrollReqs newInstance
      result
        | leftPressed && (hMouseInThumb || vMouseInThumb) = handledResult
        | btnReleased && (hMouseInScroll || vMouseInScroll) = handledResult
        | btnReleased && isDragging = handledResult
        | otherwise = Nothing
    Move point -> result where
      drag bar = updateScrollThumb state bar point contentArea sctx
      makeWidget state = rebuildWidget wenv state inst
      makeResult state = resultReqs scrollReqs (makeWidget state)
      result = fmap (makeResult . drag) dragging
    WheelScroll _ (Point wx wy) wheelDirection -> result where
      changedX = wx /= 0 && childWidth > cw
      changedY = wy /= 0 && childHeight > ch
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
        _sstDeltaX = scrollAxis (stepX + dx) childWidth cw,
        _sstDeltaY = scrollAxis (stepY + dy) childHeight ch
      }
    _ -> Nothing
    where
      style = scrollActiveStyle wenv inst
      contentArea = getContentArea style inst
      Rect cx cy cw ch = contentArea
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
    style = scrollActiveStyle wenv inst
    contentArea = getContentArea style inst
    Rect rx ry rw rh = rect
    Rect cx cy cw ch = contentArea
    diffL = cx - rx
    diffR = cx + cw - (rx + rw)
    diffT = cy - ry
    diffB = cy + ch - (ry + rh)
    stepX
      | rectInRectH rect contentArea = dx
      | abs diffL <= abs diffR = diffL + dx
      | otherwise = diffR + dx
    stepY
      | rectInRectV rect contentArea = dy
      | abs diffT <= abs diffB = diffT + dy
      | otherwise = diffB + dy
    newState = state {
      _sstDeltaX = scrollAxis stepX childWidth cw,
      _sstDeltaY = scrollAxis stepY childHeight ch
    }
    newInstance = rebuildWidget wenv newState inst
    result
      | rectInRect rect contentArea = Nothing
      | otherwise = Just $ resultWidget newInstance

  updateScrollThumb state activeBar point contentArea sctx = newState where
    Point px py = point
    ScrollContext{..} = sctx
    Rect cx cy cw ch = contentArea
    hMid = _rW hThumbRect / 2
    vMid = _rH vThumbRect / 2
    hDelta = (cx - px + hMid) / hScrollRatio
    vDelta = (cy - py + vMid) / vScrollRatio
    newDeltaX
      | activeBar == HBar = scrollAxis hDelta childWidth cw
      | otherwise = dx
    newDeltaY
      | activeBar == VBar = scrollAxis vDelta childHeight ch
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
    style = scrollActiveStyle wenv inst
    child = Seq.index children 0
    tw = sizeReqMin $ _wiSizeReqW child
    th = sizeReqMin $ _wiSizeReqH child
    Size w h = fromMaybe def (addOuterSize style (Size tw th))
    factor = 1

    sizeReq = (FlexSize w factor, FlexSize h factor)

  scrollResize uWidget state wenv viewport renderArea inst = newInst where
    style = scrollActiveStyle wenv inst
    Rect cl ct cw ch = fromMaybe def (removeOuterBounds style renderArea)
    dx = _sstDeltaX state
    dy = _sstDeltaY state

    child = Seq.index (_wiChildren inst) 0
    childWidth2 = sizeReqMin $ _wiSizeReqW child
    childHeight2 = sizeReqMin $ _wiSizeReqH child

    areaW = max cw childWidth2
    areaH = max ch childHeight2
    newDx = scrollAxis dx areaW cw
    newDy = scrollAxis dy areaH ch
    cRenderArea = Rect (cl + newDx) (ct + newDy) areaW areaH
    cViewport = fromMaybe def (intersectRects viewport cRenderArea)

    defWidget = makeScroll config $ state {
      _sstChildSize = Size areaW areaH
    }
    newWidget = fromMaybe defWidget uWidget
    cWidget = _wiWidget child
    tempChild = widgetResize cWidget wenv viewport cRenderArea child
    newChild = tempChild {
      _wiViewport = cViewport,
      _wiRenderArea = cRenderArea
    }

    newInst = inst {
      _wiViewport = viewport,
      _wiRenderArea = renderArea,
      _wiWidget = newWidget,
      _wiChildren = Seq.singleton newChild
    }

  render renderer wenv inst =
    drawStyledAction renderer renderArea style $ \_ ->
      drawInScissor renderer True viewport $ do
        widgetRender (_wiWidget child) renderer wenv child

        when hScrollRequired $
          drawRect renderer hScrollRect barColorH Nothing

        when vScrollRequired $
          drawRect renderer vScrollRect barColorV Nothing

        when hScrollRequired $
          drawRect renderer hThumbRect thumbColorH Nothing

        when vScrollRequired $
          drawRect renderer vThumbRect thumbColorV Nothing
    where
      style = scrollActiveStyle wenv inst
      child = inst ^. L.children ^?! ix 0
      viewport = fromMaybe def (removeOuterBounds style ( _wiViewport inst))
      renderArea = _wiRenderArea inst

      ScrollContext{..} = scrollStatus config wenv state inst
      draggingH = _sstDragging state == Just HBar
      draggingV = _sstDragging state == Just VBar
      theme = wenv ^. L.theme

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

scrollActiveStyle :: WidgetEnv s e -> WidgetInstance s e -> StyleState
scrollActiveStyle wenv inst
  | isFocused wenv child = focusedStyle wenv inst
  | otherwise = activeStyle wenv inst
  where
    child = inst ^. L.children ^?! ix 0

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
  style = scrollActiveStyle wenv inst
  contentArea = getContentArea style inst
  barW = fromMaybe (theme ^. L.scrollWidth) (_scWidth config)
  caLeft = _rX contentArea
  caTop = _rY contentArea
  caWidth = _rW contentArea
  caHeight = _rH contentArea
  hScrollTop = caHeight - barW
  vScrollLeft = caWidth - barW
  hRatio = caWidth / childWidth
  vRatio = caHeight / childHeight
  hRatioR = (caWidth - barW) / childWidth
  vRatioR = (caHeight - barW) / childHeight
  (hScrollRatio, vScrollRatio)
    | hRatio < 1 && vRatio < 1 = (hRatioR, vRatioR)
    | otherwise = (hRatio, vRatio)
  hScrollRequired = hScrollRatio < 1
  vScrollRequired = vScrollRatio < 1
  hScrollRect = Rect {
    _rX = caLeft,
    _rY = caTop + hScrollTop,
    _rW = caWidth,
    _rH = barW
  }
  vScrollRect = Rect {
    _rX = caLeft + vScrollLeft,
    _rY = caTop,
    _rW = barW,
    _rH = caHeight
  }
  hThumbRect = Rect {
    _rX = caLeft - hScrollRatio * dx,
    _rY = caTop + hScrollTop,
    _rW = hScrollRatio * caWidth,
    _rH = barW
  }
  vThumbRect = Rect {
    _rX = caLeft + vScrollLeft,
    _rY = caTop - vScrollRatio * dy,
    _rW = barW,
    _rH = vScrollRatio * caHeight
  }
  hMouseInScroll = pointInRect mousePos hScrollRect
  vMouseInScroll = pointInRect mousePos vScrollRect
  hMouseInThumb = pointInRect mousePos hThumbRect
  vMouseInThumb = pointInRect mousePos vThumbRect
