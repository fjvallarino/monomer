{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
  scrollWheelRate,
  scrollBarHoverColor,
  scrollBarColor,
  scrollThumbHoverColor,
  scrollThumbColor,
  scrollStyle,
  scrollBarWidth,
  scrollThumbWidth,
  scrollThumbRadius
) where

import Codec.Serialise
import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (.~), (^?!), (<>~), (%~), cloneLens, ix)
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Typeable (cast)
import GHC.Generics

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
  deriving (Eq, Show, Generic, Serialise)

data ScrollCfg = ScrollCfg {
  _scScrollType :: Maybe ScrollType,
  _scWheelRate :: Maybe Double,
  _scBarColor :: Maybe Color,
  _scBarHoverColor :: Maybe Color,
  _scThumbColor :: Maybe Color,
  _scThumbHoverColor :: Maybe Color,
  _scStyle :: Maybe (ALens' ThemeState StyleState),
  _scBarWidth :: Maybe Double,
  _scThumbWidth :: Maybe Double,
  _scThumbRadius :: Maybe Double
}

instance Default ScrollCfg where
  def = ScrollCfg {
    _scScrollType = Nothing,
    _scWheelRate = Nothing,
    _scBarColor = Nothing,
    _scBarHoverColor = Nothing,
    _scThumbColor = Nothing,
    _scThumbHoverColor = Nothing,
    _scStyle = Nothing,
    _scBarWidth = Nothing,
    _scThumbWidth = Nothing,
    _scThumbRadius = Nothing
  }

instance Semigroup ScrollCfg where
  (<>) t1 t2 = ScrollCfg {
    _scScrollType = _scScrollType t2 <|> _scScrollType t1,
    _scWheelRate = _scWheelRate t2 <|> _scWheelRate t1,
    _scBarColor = _scBarColor t2 <|> _scBarColor t1,
    _scBarHoverColor = _scBarHoverColor t2 <|> _scBarHoverColor t1,
    _scThumbColor = _scThumbColor t2 <|> _scThumbColor t1,
    _scThumbHoverColor = _scThumbHoverColor t2 <|> _scThumbHoverColor t1,
    _scStyle = _scStyle t2 <|> _scStyle t1,
    _scBarWidth = _scBarWidth t2 <|> _scBarWidth t1,
    _scThumbWidth = _scThumbWidth t2 <|> _scThumbWidth t1,
    _scThumbRadius = _scThumbRadius t2 <|> _scThumbRadius t1
  }

instance Monoid ScrollCfg where
  mempty = def

data ScrollState = ScrollState {
  _sstDragging :: Maybe ActiveBar,
  _sstDeltaX :: !Double,
  _sstDeltaY :: !Double,
  _sstChildSize :: Size
} deriving (Eq, Show, Generic, Serialise)

scrollType :: ScrollType -> ScrollCfg
scrollType st = def {
  _scScrollType = Just st
}

scrollWheelRate :: Double -> ScrollCfg
scrollWheelRate rate = def {
  _scWheelRate = Just rate
}

scrollBarColor :: Color -> ScrollCfg
scrollBarColor col = def {
  _scBarColor = Just col
}

scrollBarHoverColor :: Color -> ScrollCfg
scrollBarHoverColor col = def {
  _scBarHoverColor = Just col
}

scrollThumbColor :: Color -> ScrollCfg
scrollThumbColor col = def {
  _scThumbColor = Just col
}

scrollThumbHoverColor :: Color -> ScrollCfg
scrollThumbHoverColor col = def {
  _scThumbHoverColor = Just col
}

scrollBarWidth :: Double -> ScrollCfg
scrollBarWidth w = def {
  _scBarWidth = Just w
}

scrollThumbWidth :: Double -> ScrollCfg
scrollThumbWidth w = def {
  _scThumbWidth = Just w
}

scrollThumbRadius :: Double -> ScrollCfg
scrollThumbRadius r = def {
  _scThumbRadius = Just r
}

scrollStyle :: ALens' ThemeState StyleState -> ScrollCfg
scrollStyle style = def {
  _scStyle = Just style
}

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

instance WidgetModel ScrollState where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

newtype ScrollMessage
  = ScrollTo Rect

scroll :: WidgetNode s e -> WidgetNode s e
scroll managedWidget = scroll_ def managedWidget

scroll_ :: [ScrollCfg] -> WidgetNode s e -> WidgetNode s e
scroll_ configs managed = makeNode (makeScroll config def) managed where
  config = mconcat configs

hscroll :: WidgetNode s e -> WidgetNode s e
hscroll managedWidget = hscroll_ def managedWidget

hscroll_ :: [ScrollCfg] -> WidgetNode s e -> WidgetNode s e
hscroll_ configs managed = makeNode (makeScroll config def) managed where
  config = mconcat (scrollType ScrollH : configs)

vscroll :: WidgetNode s e -> WidgetNode s e
vscroll managedWidget = vscroll_ def managedWidget

vscroll_ :: [ScrollCfg] -> WidgetNode s e -> WidgetNode s e
vscroll_ configs managed = makeNode (makeScroll config def) managed where
  config = mconcat (scrollType ScrollV : configs)

makeNode :: Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode widget managedWidget = defaultWidgetNode "scroll" widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makeScroll :: ScrollCfg -> ScrollState -> Widget s e
makeScroll config state = widget where
  widget = createContainer state def {
    containerChildrenOffset = Just offset,
    containerUseScissor = True,
    containerGetBaseStyle = getBaseStyle,
    containerRestore = restore,
    containerHandleEvent = handleEvent,
    containerHandleMessage = handleMessage,
    containerGetSizeReq = getSizeReq,
    containerResize = resize,
    containerRenderAfter = renderAfter
  }

  ScrollState dragging dx dy cs = state
  Size childWidth childHeight = cs
  offset = Point dx dy

  getBaseStyle wenv node = _scStyle config >>= handler where
    handler lstyle = Just $ collectTheme wenv (cloneLens lstyle)

  restore wenv oldState oldNode node = resultWidget newNode where
    newNode = node
      & L.widget .~ makeScroll config oldState

  handleEvent wenv target evt node = case evt of
    ButtonAction point btn status _ -> result where
      leftPressed = status == PressedBtn && btn == wenv ^. L.mainButton
      btnReleased = status == ReleasedBtn && btn == wenv ^. L.mainButton
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
      newRes = rebuildWidget wenv newState node
      handledResult = Just $ newRes
        & L.requests <>~ Seq.fromList scrollReqs
      result
        | leftPressed && (hMouseInThumb || vMouseInThumb) = handledResult
        | btnReleased && (hMouseInScroll || vMouseInScroll) = handledResult
        | btnReleased && isDragging = handledResult
        | otherwise = Nothing
    Move point -> result where
      drag bar = updateScrollThumb state bar point contentArea sctx
      makeWidget state = rebuildWidget wenv state node
      makeResult state = makeWidget state
        & L.requests <>~ Seq.fromList (RenderOnce : scrollReqs)
      result = fmap (makeResult . drag) dragging
    WheelScroll _ (Point wx wy) wheelDirection -> result where
      changedX = wx /= 0 && childWidth > cw
      changedY = wy /= 0 && childHeight > ch
      needsUpdate = changedX || changedY
      makeWidget state = rebuildWidget wenv state node
      makeResult state = makeWidget state
        & L.requests <>~ Seq.fromList scrollReqs
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
      theme = activeTheme wenv node
      style = scrollActiveStyle wenv node
      contentArea = getContentArea style node
      Rect cx cy cw ch = contentArea
      sctx@ScrollContext{..} = scrollStatus config wenv state node
      scrollReqs = [IgnoreParentEvents]
      wheelRate = fromMaybe (theme ^. L.scrollWheelRate) (_scWheelRate config)

  scrollAxis reqDelta childLength vpLength
    | maxDelta == 0 = 0
    | reqDelta < 0 = max reqDelta (-maxDelta)
    | otherwise = min reqDelta 0
    where
      maxDelta = max 0 (childLength - vpLength)

  handleMessage wenv ctx message node = result where
    handleScrollMessage (ScrollTo rect) = scrollTo wenv node rect
    result = cast message >>= handleScrollMessage

  scrollTo wenv node targetRect = result where
    style = scrollActiveStyle wenv node
    contentArea = getContentArea style node
    rect = moveRect offset targetRect
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
    result
      | rectInRect rect contentArea = Nothing
      | otherwise = Just $ rebuildWidget wenv newState node

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

  rebuildWidget wenv newState node = result where
    newNode = node
      & L.widget .~ makeScroll config newState
    result = resultWidget newNode

  getSizeReq :: ContainerGetSizeReqHandler s e a
  getSizeReq wenv currState node children = sizeReq where
    style = scrollActiveStyle wenv node
    child = Seq.index children 0
    tw = sizeReqMaxBounded $ child ^. L.info . L.sizeReqW
    th = sizeReqMaxBounded $ child ^. L.info . L.sizeReqH
    Size w h = fromMaybe def (addOuterSize style (Size tw th))
    factor = 1

    sizeReq = (FlexSize w factor, FlexSize h factor)

  resize :: ContainerResizeHandler s e
  resize wenv viewport children node = result where
    style = scrollActiveStyle wenv node
    scrollType = fromMaybe ScrollBoth (_scScrollType config)

    Rect cl ct cw ch = fromMaybe def (removeOuterBounds style viewport)
    dx = _sstDeltaX state
    dy = _sstDeltaY state

    child = Seq.index (node ^. L.children) 0
    childWidth2 = sizeReqMaxBounded $ child ^. L.info . L.sizeReqW
    childHeight2 = sizeReqMaxBounded $ child ^. L.info . L.sizeReqH

    areaW
      | scrollType == ScrollV = cw
      | otherwise = max cw childWidth2
    areaH
      | scrollType == ScrollH = ch
      | otherwise = max ch childHeight2
    newDx = scrollAxis dx areaW cw
    newDy = scrollAxis dy areaH ch
    cViewport = Rect cl ct areaW areaH
    newState = state {
      _sstDeltaX = newDx,
      _sstDeltaY = newDy,
      _sstChildSize = Size areaW areaH
    }
    newNode = resultWidget $ node
      & L.widget .~ makeScroll config newState
    result = (newNode, Seq.singleton cViewport)

  renderAfter renderer wenv node = do
    when hScrollRequired $
      drawRect renderer hScrollRect barColorH Nothing

    when vScrollRequired $
      drawRect renderer vScrollRect barColorV Nothing

    when hScrollRequired $
      drawRect renderer hThumbRect thumbColorH thumbRadius

    when vScrollRequired $
      drawRect renderer vThumbRect thumbColorV thumbRadius
    where
      ScrollContext{..} = scrollStatus config wenv state node
      draggingH = _sstDragging state == Just HBar
      draggingV = _sstDragging state == Just VBar
      theme = wenv ^. L.theme
      athm = activeTheme wenv node
      tmpRad = fromMaybe (athm ^. L.scrollThumbRadius) (_scThumbRadius config)
      thumbRadius
        | tmpRad > 0 = Just (radius tmpRad)
        | otherwise = Nothing

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
  | isNodeFocused wenv child = focusedStyle wenv node
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
  mousePos = _ipsMousePos (_weInputStatus wenv)
  theme = activeTheme wenv node
  style = scrollActiveStyle wenv node
  contentArea = getContentArea style node
  barW = fromMaybe (theme ^. L.scrollBarWidth) (_scBarWidth config)
  thumbW = fromMaybe (theme ^. L.scrollThumbWidth) (_scThumbWidth config)
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
    _rY = caTop + hScrollTop + (barW - thumbW) / 2,
    _rW = hScrollRatio * caWidth,
    _rH = thumbW
  }
  vThumbRect = Rect {
    _rX = caLeft + vScrollLeft + (barW - thumbW) / 2,
    _rY = caTop - vScrollRatio * dy,
    _rW = thumbW,
    _rH = vScrollRatio * caHeight
  }
  hMouseInScroll = pointInRect mousePos hScrollRect
  vMouseInScroll = pointInRect mousePos vScrollRect
  hMouseInThumb = pointInRect mousePos hThumbRect
  vMouseInThumb = pointInRect mousePos vThumbRect
