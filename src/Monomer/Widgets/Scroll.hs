{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widgets.Scroll (
  ScrollCfg,
  ScrollMessage(..),
  scroll,
  scroll_,
  hscroll,
  hscroll_,
  vscroll,
  vscroll_,
  barHoverColor,
  barColor,
  thumbHoverColor,
  thumbColor,
  scrollStyle,
  barWidth
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (.~), (^?), (^?!), cloneLens, ix)
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Sequence (Seq)
import Data.Typeable

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

import qualified Monomer.Lens as L

data ScrollType
  = ScrollH
  | ScrollV
  | ScrollBoth
  deriving (Eq, Show)

data ActiveBar
  = HBar
  | VBar
  deriving (Eq)

data ScrollCfg = ScrollCfg {
  _scScrollType :: Maybe ScrollType,
  _scBarColor :: Maybe Color,
  _scBarHoverColor :: Maybe Color,
  _scThumbColor :: Maybe Color,
  _scThumbHoverColor :: Maybe Color,
  _scStyle :: Maybe (ALens' ThemeState StyleState),
  _scWidth :: Maybe Double
}

instance Default ScrollCfg where
  def = ScrollCfg {
    _scScrollType = Nothing,
    _scBarColor = Nothing,
    _scBarHoverColor = Nothing,
    _scThumbColor = Nothing,
    _scThumbHoverColor = Nothing,
    _scStyle = Nothing,
    _scWidth = Nothing
  }

instance Semigroup ScrollCfg where
  (<>) t1 t2 = ScrollCfg {
    _scScrollType = _scScrollType t2 <|> _scScrollType t1,
    _scBarColor = _scBarColor t2 <|> _scBarColor t1,
    _scBarHoverColor = _scBarHoverColor t2 <|> _scBarHoverColor t1,
    _scThumbColor = _scThumbColor t2 <|> _scThumbColor t1,
    _scThumbHoverColor = _scThumbHoverColor t2 <|> _scThumbHoverColor t1,
    _scStyle = _scStyle t2 <|> _scStyle t1,
    _scWidth = _scWidth t2 <|> _scWidth t1
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

scrollType :: ScrollType -> ScrollCfg
scrollType st = def {
  _scScrollType = Just st
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

scroll :: WidgetNode s e -> WidgetNode s e
scroll managedWidget = scroll_ managedWidget [def]

scroll_ :: WidgetNode s e -> [ScrollCfg] -> WidgetNode s e
scroll_ managed configs = makeNode (makeScroll config def) managed where
  config = mconcat configs

hscroll :: WidgetNode s e -> WidgetNode s e
hscroll managedWidget = hscroll_ managedWidget [def]

hscroll_ :: WidgetNode s e -> [ScrollCfg] -> WidgetNode s e
hscroll_ managed configs = makeNode (makeScroll config def) managed where
  config = mconcat (scrollType ScrollH : configs)

vscroll :: WidgetNode s e -> WidgetNode s e
vscroll managedWidget = vscroll_ managedWidget [def]

vscroll_ :: WidgetNode s e -> [ScrollCfg] -> WidgetNode s e
vscroll_ managed configs = makeNode (makeScroll config def) managed where
  config = mconcat (scrollType ScrollV : configs)

makeNode :: Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode widget managedWidget = defaultWidgetNode "scroll" widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makeScroll :: ScrollCfg -> ScrollState -> Widget s e
makeScroll config state = widget where
  widget = createContainer def {
    containerChildrenOffset = Just childrenOffset,
    containerStyleOnMerge = True,
    containerGetBaseStyle = getBaseStyle,
    containerGetState = makeState state,
    containerMerge = merge,
    containerHandleEvent = handleEvent,
    containerHandleMessage = handleMessage,
    containerGetSizeReq = getSizeReq,
    containerResize = resize,
    containerRenderAfter = renderAfter
  }

  ScrollState dragging dx dy cs = state
  Size childWidth childHeight = cs
  childrenOffset = Point dx dy

  getBaseStyle wenv node = _scStyle config >>= handler where
    handler lstyle = Just $ collectTheme wenv (cloneLens lstyle)

  merge wenv oldState oldNode node = resultWidget newNode where
    newState = fromMaybe state (useState oldState)
    newNode = node
      & L.widget .~ makeScroll config newState

  handleEvent wenv target evt node = case evt of
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
      newNode = node
        & L.widget .~ makeScroll config newState
      handledResult = Just $ resultReqs newNode scrollReqs
      result
        | leftPressed && (hMouseInThumb || vMouseInThumb) = handledResult
        | btnReleased && (hMouseInScroll || vMouseInScroll) = handledResult
        | btnReleased && isDragging = handledResult
        | otherwise = Nothing
    Move point -> result where
      drag bar = updateScrollThumb state bar point contentArea sctx
      makeWidget state = node
        & L.widget .~ makeScroll config state
      makeResult state = resultReqs (makeWidget state) (RenderOnce : scrollReqs)
      result = fmap (makeResult . drag) dragging
    WheelScroll _ (Point wx wy) wheelDirection -> result where
      changedX = wx /= 0 && childWidth > cw
      changedY = wy /= 0 && childHeight > ch
      needsUpdate = changedX || changedY
      makeWidget state = node
        & L.widget .~ makeScroll config state
      makeResult state = resultReqs (makeWidget state) scrollReqs
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
      style = scrollActiveStyle wenv node
      contentArea = getContentArea style node
      Rect cx cy cw ch = contentArea
      sctx@ScrollContext{..} = scrollStatus config wenv state node
      scrollReqs = [IgnoreChildrenEvents, IgnoreParentEvents]

  scrollAxis reqDelta childLength vpLength
    | maxDelta == 0 = 0
    | reqDelta < 0 = max reqDelta (-maxDelta)
    | otherwise = min reqDelta 0
    where
      maxDelta = max 0 (childLength - vpLength)

  handleMessage wenv ctx message node = result where
    handleScrollMessage (ScrollTo rect) = scrollTo wenv node rect
    result = cast message >>= handleScrollMessage

  scrollTo wenv node rect = result where
    style = scrollActiveStyle wenv node
    contentArea = getContentArea style node
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
    newNode = node
      & L.widget .~ makeScroll config newState
    result
      | rectInRect rect contentArea = Nothing
      | otherwise = Just $ resultWidget newNode

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

  getSizeReq :: ContainerGetSizeReqHandler s e
  getSizeReq wenv node children = sizeReq where
    style = scrollActiveStyle wenv node
    child = Seq.index children 0
    tw = sizeReqMax $ child ^. L.info . L.sizeReqW
    th = sizeReqMax $ child ^. L.info . L.sizeReqH
    Size w h = fromMaybe def (addOuterSize style (Size tw th))
    factor = 1

    sizeReq = (FlexSize w factor, FlexSize h factor)

  resize :: ContainerResizeHandler s e
  resize wenv viewport renderArea children node = result where
    style = scrollActiveStyle wenv node
    scrollType = fromMaybe ScrollBoth (_scScrollType config)

    Rect cl ct cw ch = fromMaybe def (removeOuterBounds style renderArea)
    dx = _sstDeltaX state
    dy = _sstDeltaY state

    child = Seq.index children 0
    cActStyle = activeStyle wenv child
    cReqW = fromMaybe (child ^. L.info . L.sizeReqW) (cActStyle ^. L.sizeReqW)
    cReqH = fromMaybe (child ^. L.info . L.sizeReqH) (cActStyle ^. L.sizeReqH)

    childWidth2 = sizeReqMax cReqW
    childHeight2 = sizeReqMax cReqH
--    childWidth2 = sizeReqMax $ child ^. L.info . L.sizeReqW
--    childHeight2 = sizeReqMax $ child ^. L.info . L.sizeReqH

    areaW
      | scrollType == ScrollV = cw
      | otherwise = max cw childWidth2
    areaH
      | scrollType == ScrollH = ch
      | otherwise = max ch childHeight2
    newDx = scrollAxis dx areaW cw
    newDy = scrollAxis dy areaH ch
    cRenderArea = Rect cl ct areaW areaH
    cViewport = fromMaybe def (intersectRects viewport cRenderArea)

    newWidget = makeScroll config $ state {
      _sstDeltaX = newDx,
      _sstDeltaY = newDy,
      _sstChildSize = Size areaW areaH
    }
    newNode = node & L.widget .~ newWidget
    result = (newNode, Seq.singleton (cViewport, cRenderArea))

  renderAfter renderer wenv node = do
    when hScrollRequired $
      drawRect renderer hScrollRect barColorH Nothing

    when vScrollRequired $
      drawRect renderer vScrollRect barColorV Nothing

    when hScrollRequired $
      drawRect renderer hThumbRect thumbColorH Nothing

    when vScrollRequired $
      drawRect renderer vThumbRect thumbColorV Nothing
    where
      style = scrollActiveStyle wenv node
      vp = node ^. L.info . L.viewport
      viewport = fromMaybe def (removeOuterBounds style vp)
      renderArea = node ^. L.info . L.renderArea

      ScrollContext{..} = scrollStatus config wenv state node
      draggingH = _sstDragging state == Just HBar
      draggingV = _sstDragging state == Just VBar
      theme = wenv ^. L.theme

      cfgBarBCol = _scBarColor config
      cfgBarHCol = _scBarHoverColor config
      cfgThumbBCol = _scThumbColor config
      cfgThumbHCol = _scThumbHoverColor config

      barBCol = cfgBarBCol <|> Just (theme ^. L.basic . L.scrollBarColor)
      barHCol = cfgBarHCol <|> Just (theme ^. L.hover . L.scrollBarColor)
      thumbBCol = cfgThumbBCol <|> Just (theme ^. L.basic . L.scrollThumbColor)
      thumbHCol = cfgThumbHCol <|> Just (theme ^. L.hover. L.scrollThumbColor)

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

scrollActiveStyle :: WidgetEnv s e -> WidgetNode s e -> StyleState
scrollActiveStyle wenv node
  | isFocused wenv child = focusedStyle wenv node
  | otherwise = activeStyle wenv node
  where
    child = node ^. L.children ^?! ix 0

scrollStatus
  :: ScrollCfg
  -> WidgetEnv s e
  -> ScrollState
  -> WidgetNode s e
  -> ScrollContext
scrollStatus config wenv scrollState node = ScrollContext{..} where
  ScrollState _ dx dy (Size childWidth childHeight) = scrollState
  mousePos = wenv ^. L.inputStatus . L.mousePos
  theme = activeTheme wenv node
  style = scrollActiveStyle wenv node
  contentArea = getContentArea style node
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
