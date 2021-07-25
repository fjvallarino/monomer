{-|
Module      : Monomer.Widgets.Containers.Scroll
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Scroll container of a single node. Assigns all the space the inner node requests
but limits itself to what its parent assigns. It allows navigating the content
of the inner node with the scroll bars. It also supports automatic focus
following.

Configs:

- wheelRate: rate at which wheel movement causes scrolling.
- barColor: the color of the bar (container of the thumb).
- barHoverColor: the color of the bar when mouse is on top.
- barWidth: the width of the bar.
- thumbColor: the color of the thumb.
- thumbHoverColor: the color of the thumb when mouse is on top.
- thumbWidth: the width of the thumb.
- thumbRadius: the radius of the corners of the thumb.
- scrollOverlay_: whether scroll bar should be on top of content or by the side.
- scrollInvisible_: shortcut for setting invisible style. Useful with scroll
overlay, since it allows scrolling without taking up space or hiding content.
- scrollFollowFocus_: whether to auto scroll when focusing a non visible item.
- scrollStyle: the base style of the scroll bar.

Messages:

- ScrollTo: Causes the scroll to update its handles to ensure rect is visible.
- ScrollReset: Sets both handle positions to zero.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Monomer.Widgets.Containers.Scroll (
  ScrollMessage(..),
  scroll,
  scroll_,
  hscroll,
  hscroll_,
  vscroll,
  vscroll_,
  scrollOverlay,
  scrollOverlay_,
  scrollFwdStyle,
  scrollFwdDefault,
  scrollInvisible,
  scrollInvisible_,
  scrollFollowFocus,
  scrollFollowFocus_,
  scrollStyle
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (.~), (^?), (^?!), (<>~), (%~), _Just, cloneLens, ix)
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Typeable (cast)
import GHC.Generics

import qualified Data.Sequence as Seq

import Monomer.Helper
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
  deriving (Eq, Show, Generic)

data ScrollCfg s e = ScrollCfg {
  _scScrollType :: Maybe ScrollType,
  _scScrollOverlay :: Maybe Bool,
  _scScrollFwdStyle :: Maybe (WidgetEnv s e -> Style -> (Style, Style)),
  _scFollowFocus :: Maybe Bool,
  _scWheelRate :: Maybe Rational,
  _scBarColor :: Maybe Color,
  _scBarHoverColor :: Maybe Color,
  _scThumbColor :: Maybe Color,
  _scThumbHoverColor :: Maybe Color,
  _scStyle :: Maybe (ALens' ThemeState StyleState),
  _scBarWidth :: Maybe Double,
  _scThumbWidth :: Maybe Double,
  _scThumbRadius :: Maybe Double
}

instance Default (ScrollCfg s e) where
  def = ScrollCfg {
    _scScrollType = Nothing,
    _scScrollOverlay = Nothing,
    _scScrollFwdStyle = Nothing,
    _scFollowFocus = Nothing,
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

instance Semigroup (ScrollCfg s e) where
  (<>) t1 t2 = ScrollCfg {
    _scScrollType = _scScrollType t2 <|> _scScrollType t1,
    _scScrollOverlay = _scScrollOverlay t2 <|> _scScrollOverlay t1,
    _scScrollFwdStyle = _scScrollFwdStyle t2 <|> _scScrollFwdStyle t1,
    _scFollowFocus = _scFollowFocus t2 <|> _scFollowFocus t1,
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

instance Monoid (ScrollCfg s e) where
  mempty = def

instance CmbWheelRate (ScrollCfg s e) Rational where
  wheelRate rate = def {
    _scWheelRate = Just rate
  }

instance CmbBarColor (ScrollCfg s e) where
  barColor col = def {
    _scBarColor = Just col
  }

instance CmbBarHoverColor (ScrollCfg s e) where
  barHoverColor col = def {
    _scBarHoverColor = Just col
  }

instance CmbBarWidth (ScrollCfg s e) where
  barWidth w = def {
    _scBarWidth = Just w
  }

-- Thumb
instance CmbThumbColor (ScrollCfg s e) where
  thumbColor col = def {
    _scThumbColor = Just col
  }

instance CmbThumbHoverColor (ScrollCfg s e) where
  thumbHoverColor col = def {
    _scThumbHoverColor = Just col
  }

instance CmbThumbWidth (ScrollCfg s e) where
  thumbWidth w = def {
    _scThumbWidth = Just w
  }

instance CmbThumbRadius (ScrollCfg s e) where
  thumbRadius r = def {
    _scThumbRadius = Just r
  }

-- | Scroll bars will be displayed on top of the content.
scrollOverlay :: ScrollCfg s e
scrollOverlay = scrollOverlay_ True

{-|
Sets whether scroll bars will be displayed on top of the content or next to it.
-}
scrollOverlay_ :: Bool -> ScrollCfg s e
scrollOverlay_ overlay = def {
  _scScrollOverlay = Just overlay
}

{-|
Sets a function that will split the node's style into one for the scroll and one
for the child node. Useful for widgets which wrap themselves in a scroll, such
as textArea, to be able to receive customizations made by the user.
-}
scrollFwdStyle :: (WidgetEnv s e -> Style -> (Style, Style)) -> ScrollCfg s e
scrollFwdStyle fwd = def {
  _scScrollFwdStyle = Just fwd
}

-- | Default style forward function, keeping standard fields for scroll.
scrollFwdDefault :: WidgetEnv s e -> Style -> (Style, Style)
scrollFwdDefault wenv style = (scrollStyle, childStyle) where
  scrollStyle = def
    & collectStyleField_ L.sizeReqW style
    & collectStyleField_ L.sizeReqH style
    & collectStyleField_ L.border style
    & collectStyleField_ L.radius style
    & collectStyleField_ L.bgColor style
  childStyle = def
    & collectStyleField_ L.padding style
    & collectStyleField_ L.fgColor style
    & collectStyleField_ L.sndColor style
    & collectStyleField_ L.hlColor style
    & collectStyleField_ L.text style
    & collectStyleField_ L.cursorIcon style

-- | Sets the style of the scroll bars to transparent.
scrollInvisible :: ScrollCfg s e
scrollInvisible = scrollInvisible_ True

-- | Whether to set the style of the scroll bars to transparent.
scrollInvisible_ :: Bool -> ScrollCfg s e
scrollInvisible_ False = def
scrollInvisible_ True = def {
  _scScrollOverlay = Just True,
  _scBarColor = Just transparent,
  _scBarHoverColor = Just transparent,
  _scThumbColor = Just transparent,
  _scThumbHoverColor = Just transparent
}

-- | Makes the scroll automatically follow focused items to make them visible.
scrollFollowFocus :: ScrollCfg s e
scrollFollowFocus = scrollFollowFocus_ True

-- | Whether to automatically follow focused items to make them visible.
scrollFollowFocus_ :: Bool -> ScrollCfg s e
scrollFollowFocus_ follow = def {
  _scFollowFocus = Just follow
}

{-|
Sets the base style of the scroll bar. Useful when creating widgets which use
scroll and may need to customize it.
-}
scrollStyle :: ALens' ThemeState StyleState -> ScrollCfg s e
scrollStyle style = def {
  _scStyle = Just style
}

-- Not exported
scrollType :: ScrollType -> ScrollCfg s e
scrollType st = def {
  _scScrollType = Just st
}

data ScrollState = ScrollState {
  _sstDragging :: Maybe ActiveBar,
  _sstDeltaX :: !Double,
  _sstDeltaY :: !Double,
  _sstVpSize :: Size,
  _sstChildSize :: Size,
  _sstScissor :: Rect
} deriving (Eq, Show, Generic)

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
} deriving (Eq, Show)

instance Default ScrollState where
  def = ScrollState {
    _sstDragging = Nothing,
    _sstDeltaX = 0,
    _sstDeltaY = 0,
    _sstVpSize = def,
    _sstChildSize = def,
    _sstScissor = def
  }

-- | Messages the scroll component supports.
data ScrollMessage
  -- | Causes the scroll to update its bars to ensure rect is visible.
  = ScrollTo Rect
  -- | Sets both bars to zero.
  | ScrollReset
  deriving (Eq, Show)

-- | Creates a scroll node that may show both bars.
scroll :: WidgetNode s e -> WidgetNode s e
scroll managedWidget = scroll_ def managedWidget

-- | Creates a scroll node that may show both bars. Accepts config.
scroll_ :: [ScrollCfg s e] -> WidgetNode s e -> WidgetNode s e
scroll_ configs managed = makeNode (makeScroll config def) managed where
  config = mconcat configs

-- | Creates a horizontal scroll node. Vertical space is equal to what the
-- | parent node assigns.
hscroll :: WidgetNode s e -> WidgetNode s e
hscroll managedWidget = hscroll_ def managedWidget

-- | Creates a horizontal scroll node. Vertical space is equal to what the
-- | parent node assigns. Accepts config.
hscroll_ :: [ScrollCfg s e] -> WidgetNode s e -> WidgetNode s e
hscroll_ configs managed = makeNode (makeScroll config def) managed where
  config = mconcat (scrollType ScrollH : configs)

-- | Creates a vertical scroll node. Vertical space is equal to what the
-- | parent node assigns.
vscroll :: WidgetNode s e -> WidgetNode s e
vscroll managedWidget = vscroll_ def managedWidget

-- | Creates a vertical scroll node. Vertical space is equal to what the
-- | parent node assigns. Accepts config.
vscroll_ :: [ScrollCfg s e] -> WidgetNode s e -> WidgetNode s e
vscroll_ configs managed = makeNode (makeScroll config def) managed where
  config = mconcat (scrollType ScrollV : configs)

makeNode :: Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode widget managedWidget = defaultWidgetNode "scroll" widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makeScroll :: ScrollCfg s e -> ScrollState -> Widget s e
makeScroll config state = widget where
  widget = createContainer state def {
    containerChildrenOffset = Just offset,
    containerChildrenScissor = Just (_sstScissor state),
    containerLayoutDirection = layoutDirection,
    containerGetBaseStyle = getBaseStyle,
    containerGetActiveStyle = scrollActiveStyle,
    containerInit = init,
    containerMerge = merge,
    containerFindByPoint = findByPoint,
    containerHandleEvent = handleEvent,
    containerHandleMessage = handleMessage,
    containerGetSizeReq = getSizeReq,
    containerResize = resize,
    containerRenderAfter = renderAfter
  }

  ScrollState dragging dx dy _ _ _ = state
  Size childWidth childHeight = _sstChildSize state
  Size maxVpW maxVpH = _sstVpSize state
  offset = Point dx dy
  scrollType = fromMaybe ScrollBoth (_scScrollType config)
  layoutDirection = case scrollType of
    ScrollH -> LayoutHorizontal
    ScrollV -> LayoutVertical
    ScrollBoth -> LayoutNone

  getBaseStyle wenv node = _scStyle config >>= handler where
    handler lstyle = Just $ collectTheme wenv (cloneLens lstyle)

  checkFwdStyle wenv node = newNode where
    fwdStyle = _scScrollFwdStyle config
    style = node ^. L.info . L.style
    (parentStyle, childStyle) = fromJust fwdStyle wenv style
    newNode
      | isJust fwdStyle = node
        & L.info . L.style .~ parentStyle
        & L.children . ix 0 . L.info . L.style .~ childStyle
      | otherwise = node

  init wenv node = resultNode newNode where
    newNode = checkFwdStyle wenv node

  merge wenv node oldNode oldState = resultNode newNode where
    newNode = checkFwdStyle wenv $ node
      & L.widget .~ makeScroll config oldState

  findByPoint wenv node start point = result where
    sctx = scrollStatus config wenv node state point
    mouseInScroll
      =  (hMouseInScroll sctx && hScrollRequired sctx)
      || (vMouseInScroll sctx && vScrollRequired sctx)
    childPoint = addPoint point offset

    child = Seq.index (node ^. L.children) 0
    childHovered = isPointInNodeVp childPoint child
    childDragged = isNodePressed wenv child
    result
      | (not mouseInScroll && childHovered) || childDragged = Just 0
      | otherwise = Nothing

  handleEvent wenv node target evt = case evt of
    Focus{} -> result where
      overlay = wenv ^. L.overlayPath
      inOverlay info
        | isJust overlay = seqStartsWith (fromJust overlay) (info ^. L.path)
        | otherwise = False
      focusPath = wenv ^. L.focusedPath
      focusInst = findInstOrScroll wenv node focusPath
      focusVp = focusInst ^? _Just . L.viewport
      focusOverlay = maybe False inOverlay focusInst

      follow = fromMaybe (theme ^. L.scrollFollowFocus) (_scFollowFocus config)
      overlayMatch = focusOverlay == inOverlay (node ^. L.info)

      result
        | follow && overlayMatch = focusVp >>= scrollTo wenv node
        | otherwise = Nothing

    ButtonAction point btn status _ -> result where
      leftPressed = status == BtnPressed && btn == wenv ^. L.mainButton
      btnReleased = status == BtnReleased && btn == wenv ^. L.mainButton

      isDragging = isJust $ _sstDragging state
      startDrag = leftPressed && not isDragging

      jumpScrollH = btnReleased && not isDragging && hMouseInScroll sctx
      jumpScrollV = btnReleased && not isDragging && vMouseInScroll sctx

      mouseInScroll = hMouseInScroll sctx || vMouseInScroll sctx
      mouseInThumb = hMouseInThumb sctx || vMouseInThumb sctx

      newState
        | startDrag && hMouseInThumb sctx = state { _sstDragging = Just HBar }
        | startDrag && vMouseInThumb sctx = state { _sstDragging = Just VBar }
        | jumpScrollH = updateScrollThumb state HBar point contentArea sctx
        | jumpScrollV = updateScrollThumb state VBar point contentArea sctx
        | btnReleased = state { _sstDragging = Nothing }
        | otherwise = state

      newRes = rebuildWidget wenv node newState
      handledResult = Just $ newRes
        & L.requests <>~ Seq.fromList scrollReqs
      result
        | leftPressed && mouseInThumb = handledResult
        | btnReleased && mouseInScroll = handledResult
        | btnReleased && isDragging = handledResult
        | otherwise = Nothing

    Move point | isJust dragging -> result where
      drag bar = updateScrollThumb state bar point contentArea sctx
      makeWidget state = rebuildWidget wenv node state
      makeResult state = makeWidget state
        & L.requests <>~ Seq.fromList (RenderOnce : scrollReqs)
      result = fmap (makeResult . drag) dragging

    Move point | isNothing dragging -> result where
      mousePosPrev = wenv ^. L.inputStatus . L.mousePosPrev
      psctx = scrollStatus config wenv node state mousePosPrev
      changed
        = hMouseInThumb sctx /= hMouseInThumb psctx
        || vMouseInThumb sctx /= vMouseInThumb psctx
        || hMouseInScroll sctx /= hMouseInScroll psctx
        || vMouseInScroll sctx /= vMouseInScroll psctx
      result
        | changed = Just $ resultReqs node [RenderOnce]
        | otherwise = Nothing

    WheelScroll _ (Point wx wy) wheelDirection -> result where
      changedX = wx /= 0 && childWidth > cw
      changedY = wy /= 0 && childHeight > ch

      needsUpdate = changedX || changedY
      makeWidget state = rebuildWidget wenv node state
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
        _sstDeltaX = scrollAxisH (stepX + dx),
        _sstDeltaY = scrollAxisV (stepY + dy)
      }

    _ -> Nothing
    where
      theme = activeTheme wenv node
      style = scrollActiveStyle wenv node
      contentArea = getContentArea style node
      mousePos = wenv ^. L.inputStatus . L.mousePos

      Rect cx cy cw ch = contentArea
      sctx = scrollStatus config wenv node state mousePos
      scrollReqs = [IgnoreParentEvents]
      wheelCfg = fromMaybe (theme ^. L.scrollWheelRate) (_scWheelRate config)
      wheelRate = fromRational wheelCfg

  scrollAxis reqDelta childLength vpLength
    | maxDelta == 0 = 0
    | reqDelta < 0 = max reqDelta (-maxDelta)
    | otherwise = min reqDelta 0
    where
      maxDelta = max 0 (childLength - vpLength)
  scrollAxisH delta = scrollAxis delta childWidth maxVpW
  scrollAxisV delta = scrollAxis delta childHeight maxVpH

  handleMessage wenv node target message = result where
    handleScrollMessage (ScrollTo rect) = scrollTo wenv node rect
    handleScrollMessage ScrollReset = scrollReset wenv node
    result = cast message >>= handleScrollMessage

  scrollTo wenv node targetRect = result where
    style = scrollActiveStyle wenv node
    contentArea = getContentArea style node

    rect = moveRect offset targetRect
    Rect rx ry rw rh = rect
    Rect cx cy _ _ = contentArea

    diffL = cx - rx
    diffR = cx + maxVpW - (rx + rw)
    diffT = cy - ry
    diffB = cy + maxVpH - (ry + rh)

    stepX
      | rectInRectH rect contentArea = dx
      | abs diffL <= abs diffR = diffL + dx
      | otherwise = diffR + dx
    stepY
      | rectInRectV rect contentArea = dy
      | abs diffT <= abs diffB = diffT + dy
      | otherwise = diffB + dy

    newState = state {
      _sstDeltaX = scrollAxisH stepX,
      _sstDeltaY = scrollAxisV stepY
    }
    result
      | rectInRect rect contentArea = Nothing
      | otherwise = Just $ rebuildWidget wenv node newState

  scrollReset wenv node = result where
    newState = state {
      _sstDeltaX = 0,
      _sstDeltaY = 0
    }
    result = Just $ rebuildWidget wenv node newState

  updateScrollThumb state activeBar point contentArea sctx = newState where
    Point px py = point
    ScrollContext{..} = sctx
    Rect cx cy _ _ = contentArea

    hMid = _rW hThumbRect / 2
    vMid = _rH vThumbRect / 2

    hDelta = (cx - px + hMid) / hScrollRatio
    vDelta = (cy - py + vMid) / vScrollRatio

    newDeltaX
      | activeBar == HBar = scrollAxisH hDelta
      | otherwise = dx
    newDeltaY
      | activeBar == VBar = scrollAxisV vDelta
      | otherwise = dy

    newState = state {
      _sstDeltaX = newDeltaX,
      _sstDeltaY = newDeltaY
    }

  rebuildWidget wenv node newState = result where
    newNode = node
      & L.widget .~ makeScroll config newState
    result = resultNode newNode

  getSizeReq :: ContainerGetSizeReqHandler s e
  getSizeReq wenv node children = sizeReq where
    style = scrollActiveStyle wenv node
    child = Seq.index children 0

    tw = sizeReqMaxBounded $ child ^. L.info . L.sizeReqW
    th = sizeReqMaxBounded $ child ^. L.info . L.sizeReqH

    Size w h = fromMaybe def (addOuterSize style (Size tw th))
    factor = 1

    sizeReq = (expandSize w factor, expandSize h factor)

  resize wenv node viewport children = result where
    theme = activeTheme wenv node
    style = scrollActiveStyle wenv node

    Rect cl ct cw ch = fromMaybe def (removeOuterBounds style viewport)
    dx = _sstDeltaX state
    dy = _sstDeltaY state

    child = Seq.index (node ^. L.children) 0
    childW = sizeReqMaxBounded $ child ^. L.info . L.sizeReqW
    childH = sizeReqMaxBounded $ child ^. L.info . L.sizeReqH

    barW = fromMaybe (theme ^. L.scrollBarWidth) (_scBarWidth config)
    overlay = fromMaybe (theme ^. L.scrollOverlay) (_scScrollOverlay config)

    (ncw, nch)
      | not overlay = (cw - barW, ch - barW)
      | otherwise = (cw, ch)
    (maxW, areaW)
      | scrollType == ScrollV && childH > ch = (ncw, ncw)
      | scrollType == ScrollV = (cw, cw)
      | scrollType == ScrollH = (cw, max cw childW)
      | childH <= ch && childW <= cw = (cw, cw)
      | childH <= ch = (cw, max cw childW)
      | otherwise = (ncw, max ncw childW)
    (maxH, areaH)
      | scrollType == ScrollH && childW > cw = (nch, nch)
      | scrollType == ScrollH = (ch, ch)
      | scrollType == ScrollV = (ch, max ch childH)
      | childW <= cw && childH <= ch = (ch, ch)
      | childW <= cw = (ch, max ch childH)
      | otherwise = (nch, max nch childH)

    newDx = scrollAxis dx areaW maxW
    newDy = scrollAxis dy areaH maxH

    scissor = Rect cl ct maxW maxH
    cViewport = Rect cl ct areaW areaH

    newState = state {
      _sstDeltaX = newDx,
      _sstDeltaY = newDy,
      _sstVpSize = Size maxW maxH,
      _sstChildSize = Size areaW areaH,
      _sstScissor = scissor
    }
    newNode = resultNode $ node
      & L.widget .~ makeScroll config newState
    result = (newNode, Seq.singleton cViewport)

  renderAfter wenv node renderer = do
    when hScrollRequired $
      drawRect renderer hScrollRect barColorH Nothing

    when vScrollRequired $
      drawRect renderer vScrollRect barColorV Nothing

    when hScrollRequired $
      drawRect renderer hThumbRect thumbColorH thumbRadius

    when vScrollRequired $
      drawRect renderer vThumbRect thumbColorV thumbRadius
    where
      ScrollContext{..} = scrollStatus config wenv node state mousePos
      mousePos = wenv ^. L.inputStatus . L.mousePos

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
  :: ScrollCfg s e
  -> WidgetEnv s e
  -> WidgetNode s e
  -> ScrollState
  -> Point
  -> ScrollContext
scrollStatus config wenv node scrollState mousePos = ScrollContext{..} where
  ScrollState _ dx dy _ _ _ = scrollState
  Size childWidth childHeight = _sstChildSize scrollState
  Size vpWidth vpHeight = _sstVpSize scrollState
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
    _rW = vpWidth,
    _rH = barW
  }
  vScrollRect = Rect {
    _rX = caLeft + vScrollLeft,
    _rY = caTop,
    _rW = barW,
    _rH = vpHeight
  }
  hThumbRect = Rect {
    _rX = caLeft - hScrollRatio * dx,
    _rY = caTop + hScrollTop + (barW - thumbW) / 2,
    _rW = hScrollRatio * vpWidth,
    _rH = thumbW
  }
  vThumbRect = Rect {
    _rX = caLeft + vScrollLeft + (barW - thumbW) / 2,
    _rY = caTop - vScrollRatio * dy,
    _rW = thumbW,
    _rH = vScrollRatio * vpHeight
  }

  hMouseInScroll = pointInRect mousePos hScrollRect
  vMouseInScroll = pointInRect mousePos vScrollRect

  hMouseInThumb = pointInRect mousePos hThumbRect
  vMouseInThumb = pointInRect mousePos vThumbRect

findInstOrScroll
  :: WidgetEnv s e -> WidgetNode s e -> Seq.Seq PathStep -> Maybe WidgetNodeInfo
findInstOrScroll wenv node target = wniScroll <|> wniTarget where
  child = Seq.index (node ^. L.children) 0
  isScroll wni = wni ^. L.widgetType == "scroll"
  branch = widgetFindBranchByPath (child ^. L.widget) wenv child target
  scrolls = Seq.filter isScroll branch
  wniTarget = Seq.lookup (length branch - 1) branch
  wniScroll = Seq.lookup (length scrolls - 1) scrolls
