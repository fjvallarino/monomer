{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Monomer.Widgets.Split (
  hsplit,
  hsplit_,
  handleWidth
) where

import Debug.Trace

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
  _spcHandleWidth :: Maybe Double
}

instance Default SplitCfg where
  def = SplitCfg {
    _spcHandleWidth = Nothing
  }

instance Semigroup SplitCfg where
  (<>) s1 s2 = SplitCfg {
    _spcHandleWidth = _spcHandleWidth s2 <|> _spcHandleWidth s1
  }

instance Monoid SplitCfg where
  mempty = def

handleWidth :: Double -> SplitCfg
handleWidth w = def {
  _spcHandleWidth = Just w
}

data SplitState = SplitState {
  _spsHandleDragged :: Bool,
  _spsHandlePos :: Double,
  _spsHandleRect :: Rect
} deriving (Eq, Show, Generic, Serialise)

hsplit :: (WidgetNode s e, WidgetNode s e) -> WidgetNode s e
hsplit nodes = hsplit_ nodes def

hsplit_ :: (WidgetNode s e, WidgetNode s e) -> [SplitCfg] -> WidgetNode s e
hsplit_ (node1, node2) configs = newNode where
  config = mconcat configs
  state = SplitState False 0 def
  widget = makeSplit True config state
  newNode = defaultWidgetNode "hsplit" widget
    & L.children .~ Seq.fromList [node1, node2]

makeSplit :: Bool -> SplitCfg -> SplitState -> Widget s e
makeSplit isHorizontal config state = widget where
  widget = createContainer state def {
    containerRestore = restore,
    containerHandleEvent = handleEvent,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  handleW = fromMaybe 5 (_spcHandleWidth config)

  restore wenv oldState oldNode newNode = result where
    result = resultWidget $ newNode
      & L.widget .~ makeSplit isHorizontal config oldState

  handleEvent wenv target evt node = case evt of
    Move p
      | isTarget && isDragging -> Just resultDrag
      | isTarget && isHandle p -> Just resultHover
      where
        Point px py = validHandlePos ra p (node ^. L.children)
        newHandlePos
          | isHorizontal = (px - ra ^. L.x) / ra ^. L.w
          | otherwise = (py - ra ^. L.y) / ra ^. L.h
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
      SplitState _ _ handleRect = state
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
    reqW = sizeReqMergeSum reqWS $ sizeReqMergeSum reqW1 reqW1
    reqH = sizeReqMergeSum reqHS $ sizeReqMergeSum reqH1 reqH1

  -- Consider contentRect
  resize wenv viewport renderArea children node = resized where
    style = activeStyle wenv node
    contentArea = fromMaybe def (removeOuterBounds style renderArea)
    Rect rx ry rw rh = contentArea
    handlePos
      | _spsHandleDragged state = _spsHandlePos state
      | otherwise = calcHandlePos contentArea children
    (w1, h1)
      | isHorizontal = ((rw - handleW) * handlePos, rh)
      | otherwise = (rw, (rh - handleW) * handlePos)
    (w2, h2)
      | isHorizontal = (rw - w1 - handleW, rh)
      | otherwise = (rw, rh - h1 - handleW)
    rect1 = Rect rx ry w1 h1
    rect2
      | isHorizontal = Rect (rx + w1 + handleW) ry w2 h2
      | otherwise = Rect rx (ry + h1 + handleW) w2 h2
    newHandleRect
      | isHorizontal = Rect (rx + w1) ry handleW h1
      | otherwise = Rect rx (ry + h1) w1 handleW
    newState = state {
      _spsHandlePos = handlePos,
      _spsHandleRect = newHandleRect
    }
    newNode = node
      & L.widget .~ makeSplit isHorizontal config newState
    newRas = Seq.fromList [rect1, rect2]
    assignedArea = Seq.zip newRas newRas
    resized = (newNode, assignedArea)

  calcHandlePos contentArea children = newPos where
    (areas, _) = assignStackAreas isHorizontal contentArea children
    selector
      | isHorizontal = _rW
      | otherwise = _rH
    childSize = selector $ Seq.index areas 0
    newPos = childSize / selector contentArea

  validHandlePos rect point children = traceShowId newPoint where
    Rect rx ry rw rh = rect
    Point px py = point
    Point vx vy = rectBoundedPoint rect point
    child1 = Seq.index children 0
    sizeReq
      | isHorizontal = child1 ^. L.info . L.sizeReqW
      | otherwise = child1 ^. L.info . L.sizeReqH
    (minSize, maxSize) = (rx + sizeReqMin sizeReq, rx + sizeReqMax sizeReq)
    newPoint
      | isHorizontal = Point (max minSize . min maxSize $ vx) vy
      | otherwise = Point vx (max minSize . min maxSize $ vy)
