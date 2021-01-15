{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Monomer.Widgets.Split (
  hsplit,
  hsplit_,
  handleWidth
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
  _spsHandlePos :: Double,
  _spsHandleRect :: Rect
} deriving (Eq, Show, Generic, Serialise)

hsplit :: (WidgetNode s e, WidgetNode s e) -> WidgetNode s e
hsplit nodes = hsplit_ nodes def

hsplit_ :: (WidgetNode s e, WidgetNode s e) -> [SplitCfg] -> WidgetNode s e
hsplit_ (node1, node2) configs = newNode where
  config = mconcat configs
  state = SplitState 0.5 def
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

  SplitState handlePos handleRect = state
  handleW = fromMaybe 5 (_spcHandleWidth config)

  restore wenv oldState oldNode newNode = result where
    result = resultWidget $ newNode
      & L.widget .~ makeSplit isHorizontal config oldState

  handleEvent wenv target evt node = case evt of
    Move p
      | isTarget && isDragging -> Just resultDrag
      | isTarget && isHandle p -> Just resultHover
      where
        Point px py = rectBoundedPoint ra p
        newHandlePos
          | isHorizontal = (px - ra ^. L.x) / ra ^. L.w
          | otherwise = (py - ra ^. L.y) / ra ^. L.h
        newState = state { _spsHandlePos = newHandlePos }
        tmpNode = node
          & L.widget .~ makeSplit isHorizontal config newState
        newNode = widgetResize (tmpNode ^. L.widget) wenv vp ra tmpNode
        resultDrag = resultReqs newNode [cursorIconReq, RenderOnce]
        resultHover = resultReqs node [cursorIconReq]
    _ -> Nothing
    where
      vp = node ^. L.info . L.viewport
      ra = node ^. L.info . L.renderArea
      isTarget = target == node ^. L.info . L.path
      isDragging = isPressed wenv node
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
    Rect rx ry rw rh = renderArea
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
    newState = SplitState handlePos newHandleRect
    newNode = node
      & L.widget .~ makeSplit isHorizontal config newState
    newRas = Seq.fromList [rect1, rect2]
    assignedArea = Seq.zip newRas newRas
    resized = (newNode, assignedArea)
