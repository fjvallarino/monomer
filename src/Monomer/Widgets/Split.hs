{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Monomer.Widgets.Split (
  hsplit,
  hsplit_,
  vsplit,
  vsplit_,
  splitHandleSize
) where

import Codec.Serialise
import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~))
import Data.Default
import Data.Maybe
import Data.Tuple (swap)
import GHC.Generics

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container
import Monomer.Widgets.Stack (assignStackAreas)

import qualified Monomer.Lens as L

newtype SplitCfg = SplitCfg {
  _spcHandleSize :: Maybe Double
}

instance Default SplitCfg where
  def = SplitCfg {
    _spcHandleSize = Nothing
  }

instance Semigroup SplitCfg where
  (<>) s1 s2 = SplitCfg {
    _spcHandleSize = _spcHandleSize s2 <|> _spcHandleSize s1
  }

instance Monoid SplitCfg where
  mempty = def

splitHandleSize :: Double -> SplitCfg
splitHandleSize w = def {
  _spcHandleSize = Just w
}

data SplitState = SplitState {
  _spsHandleDragged :: Bool,
  _spsHandlePos :: Double,
  _spsHandleRect :: Rect,
  _spsMaxDim :: Double
} deriving (Eq, Show, Generic, Serialise)

hsplit :: (WidgetNode s e, WidgetNode s e) -> WidgetNode s e
hsplit nodes = hsplit_ nodes def

hsplit_ :: (WidgetNode s e, WidgetNode s e) -> [SplitCfg] -> WidgetNode s e
hsplit_ nodes configs = split_ True nodes configs

vsplit :: (WidgetNode s e, WidgetNode s e) -> WidgetNode s e
vsplit nodes = vsplit_ nodes def

vsplit_ :: (WidgetNode s e, WidgetNode s e) -> [SplitCfg] -> WidgetNode s e
vsplit_ nodes configs = split_ False nodes configs

split_
  :: Bool -> (WidgetNode s e, WidgetNode s e) -> [SplitCfg] -> WidgetNode s e
split_ isHorizontal (node1, node2) configs = newNode where
  config = mconcat configs
  state = SplitState False 0 def 0
  widget = makeSplit isHorizontal config state
  widgetName = if isHorizontal then "hsplit" else "vsplit"
  newNode = defaultWidgetNode widgetName widget
    & L.children .~ Seq.fromList [node1, node2]

makeSplit :: Bool -> SplitCfg -> SplitState -> Widget s e
makeSplit isHorizontal config state = widget where
  widget = createContainer state def {
    containerRestore = restore,
    containerHandleEvent = handleEvent,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  handleW = fromMaybe 5 (_spcHandleSize config)

  restore wenv oldState oldNode newNode = result where
    result = resultWidget $ newNode
      & L.widget .~ makeSplit isHorizontal config oldState

  handleEvent wenv target evt node = case evt of
    Move p
      | isTarget && isDragging -> Just resultDrag
      | isTarget && isHandle p -> Just resultHover
      where
        Point px py = validHandlePos maxDim ra p (node ^. L.children)
        newHandlePos
          | isHorizontal = (px - ra ^. L.x) / maxDim
          | otherwise = (py - ra ^. L.y) / maxDim
        newState = state {
          _spsHandleDragged = True,
          _spsHandlePos = newHandlePos
        }
        tmpNode = node
          & L.widget .~ makeSplit isHorizontal config newState
        newNode = widgetResize (tmpNode ^. L.widget) wenv vp ra tmpNode
        resultDrag = resultReqs newNode [cursorIconReq, RenderOnce]
        resultHover = resultReqs node [cursorIconReq]
    _ -> Nothing
    where
      SplitState _ _ handleRect maxDim = state
      vp = node ^. L.info . L.viewport
      ra = node ^. L.info . L.renderArea
      isTarget = target == node ^. L.info . L.path
      isDragging = isNodePressed wenv node
      isHandle p = pointInRect p handleRect
      cursorIconReq
        | isHorizontal = SetCursorIcon CursorSizeH
        | otherwise = SetCursorIcon CursorSizeV

  getSizeReq :: ContainerGetSizeReqHandler s e a
  getSizeReq wenv currState node children = (reqW, reqH) where
    node1 = Seq.index children 0
    node2 = Seq.index children 1
    reqW1 = node1 ^. L.info . L.sizeReqW
    reqH1 = node1 ^. L.info . L.sizeReqH
    reqW2 = node2 ^. L.info . L.sizeReqW
    reqH2 = node2 ^. L.info . L.sizeReqH
    reqOrder = if isHorizontal then id else swap
    (reqWS, reqHS) = reqOrder (FixedSize handleW, FixedSize 0)
    reqW = sizeReqMergeSum reqWS $ sizeReqMergeSum reqW1 reqW2
    reqH = sizeReqMergeSum reqHS $ sizeReqMergeSum reqH1 reqH2

  -- Consider contentRect
  resize wenv viewport renderArea children node = resized where
    style = activeStyle wenv node
    contentArea = fromMaybe def (removeOuterBounds style renderArea)
    Rect rx ry rw rh = contentArea
    (areas, newDim) = assignStackAreas isHorizontal contentArea children
    handlePos
      | _spsHandleDragged state = _spsHandlePos state
      | otherwise = calcHandlePos areas newDim
    (w1, h1)
      | isHorizontal = ((newDim - handleW) * handlePos, rh)
      | otherwise = (rw, (newDim - handleW) * handlePos)
    (w2, h2)
      | isHorizontal = (newDim - w1 - handleW, rh)
      | otherwise = (rw, newDim - h1 - handleW)
    rect1 = Rect rx ry w1 h1
    rect2
      | isHorizontal = Rect (rx + w1 + handleW) ry w2 h2
      | otherwise = Rect rx (ry + h1 + handleW) w2 h2
    newHandleRect
      | isHorizontal = Rect (rx + w1) ry handleW h1
      | otherwise = Rect rx (ry + h1) w1 handleW
    newState = state {
      _spsHandlePos = handlePos,
      _spsHandleRect = newHandleRect,
      _spsMaxDim = newDim
    }
    newNode = node
      & L.widget .~ makeSplit isHorizontal config newState
    newRas = Seq.fromList [rect1, rect2]
    assignedArea = Seq.zip newRas newRas
    resized = (newNode, assignedArea)

  calcHandlePos areas newDim = newPos where
    selector
      | isHorizontal = _rW
      | otherwise = _rH
    childSize = selector $ Seq.index areas 0
    newPos = childSize / newDim

  validHandlePos maxDim rect point children = addPoint origin newPoint where
    Rect rx ry rw rh = rect
    Point vx vy = rectBoundedPoint rect point
    origin = Point rx ry
    isVertical = not isHorizontal
    child1 = Seq.index children 0
    child2 = Seq.index children 1
    sizeReq
      | isHorizontal = (^. L.info . L.sizeReqW)
      | otherwise = (^. L.info . L.sizeReqH)
    minSize1 = sizeReqMin (sizeReq child1)
    maxSize1 = sizeReqMax (sizeReq child1)
    minSize2 = sizeReqMin (sizeReq child2)
    maxSize2 = sizeReqMax (sizeReq child2)
    Point tx ty
      | isHorizontal = Point (max minSize1 (min maxSize1 $ vx - rx)) 0
      | otherwise = Point 0 (max minSize1 (min maxSize1 $ vy - ry))
    newPoint
      | isHorizontal && tx + minSize2 > maxDim = Point (maxDim - minSize2) ty
      | isHorizontal && maxDim - tx > maxSize2 = Point (maxDim - maxSize2) ty
      | isVertical && ty + minSize2 > maxDim = Point tx (maxDim - minSize2)
      | isVertical && maxDim - ty > maxSize2 = Point tx (maxDim - maxSize2)
      | otherwise = Point tx ty
