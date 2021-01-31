{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Split (
  hsplit,
  hsplit_,
  vsplit,
  vsplit_,
  splitHandlePos,
  splitHandlePosV,
  splitHandleSize,
  splitIgnoreChildResize
) where

import Codec.Serialise
import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (.~), (<>~))
import Data.Default
import Data.Maybe
import Data.Tuple (swap)
import GHC.Generics

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container
import Monomer.Widgets.Stack (assignStackAreas)

import qualified Monomer.Lens as L

data SplitCfg s e = SplitCfg {
  _spcHandlePos :: Maybe (WidgetData s Double),
  _spcHandleSize :: Maybe Double,
  _spcIgnoreChildResize :: Maybe Bool,
  _spcOnChange :: [Double -> e],
  _spcOnChangeReq :: [WidgetRequest s]
}

instance Default (SplitCfg s e) where
  def = SplitCfg {
    _spcHandlePos = Nothing,
    _spcHandleSize = Nothing,
    _spcIgnoreChildResize = Nothing,
    _spcOnChange = [],
    _spcOnChangeReq = []
  }

instance Semigroup (SplitCfg s e) where
  (<>) s1 s2 = SplitCfg {
    _spcHandlePos = _spcHandlePos s2 <|> _spcHandlePos s1,
    _spcHandleSize = _spcHandleSize s2 <|> _spcHandleSize s1,
    _spcIgnoreChildResize = _spcIgnoreChildResize s2 <|> _spcIgnoreChildResize s1,
    _spcOnChange = _spcOnChange s2 <|> _spcOnChange s1,
    _spcOnChangeReq = _spcOnChangeReq s2 <|> _spcOnChangeReq s1
  }

instance Monoid (SplitCfg s e) where
  mempty = def

instance CmbOnChange (SplitCfg s e) Double e where
  onChange fn = def {
    _spcOnChange = [fn]
  }

instance CmbOnChangeReq (SplitCfg s e) s where
  onChangeReq req = def {
    _spcOnChangeReq = [req]
  }

splitHandlePos :: ALens' s Double -> SplitCfg s e
splitHandlePos field = def {
  _spcHandlePos = Just (WidgetLens field)
}

splitHandlePosV :: Double -> SplitCfg s e
splitHandlePosV value = def {
  _spcHandlePos = Just (WidgetValue value)
}

splitHandleSize :: Double -> SplitCfg s e
splitHandleSize w = def {
  _spcHandleSize = Just w
}

splitIgnoreChildResize :: Bool -> SplitCfg s e
splitIgnoreChildResize ignore = def {
  _spcIgnoreChildResize = Just ignore
}

data SplitState = SplitState {
  _spsPrevReqs :: (SizeReq, SizeReq),
  _spsMaxSize :: Double,
  _spsHandlePosUserSet :: Bool,
  _spsHandlePos :: Double,
  _spsHandleRect :: Rect
} deriving (Eq, Show, Generic, Serialise)

hsplit :: (WidgetNode s e, WidgetNode s e) -> WidgetNode s e
hsplit nodes = hsplit_ def nodes

hsplit_ :: [SplitCfg s e] -> (WidgetNode s e, WidgetNode s e) -> WidgetNode s e
hsplit_ configs nodes = split_ True nodes configs

vsplit :: (WidgetNode s e, WidgetNode s e) -> WidgetNode s e
vsplit nodes = vsplit_ def nodes

vsplit_ :: [SplitCfg s e] -> (WidgetNode s e, WidgetNode s e) -> WidgetNode s e
vsplit_ configs nodes = split_ False nodes configs

split_
  :: Bool
  -> (WidgetNode s e, WidgetNode s e)
  -> [SplitCfg s e]
  -> WidgetNode s e
split_ isHorizontal (node1, node2) configs = newNode where
  config = mconcat configs
  state = SplitState {
    _spsPrevReqs = def,
    _spsMaxSize = 0,
    _spsHandlePosUserSet = False,
    _spsHandlePos = 0.5,
    _spsHandleRect = def
  }
  widget = makeSplit isHorizontal config state
  widgetName = if isHorizontal then "hsplit" else "vsplit"
  newNode = defaultWidgetNode widgetName widget
    & L.children .~ Seq.fromList [node1, node2]

makeSplit :: Bool -> SplitCfg s e -> SplitState -> Widget s e
makeSplit isHorizontal config state = widget where
  widget = createContainer state def {
    containerInit = init,
    containerRestore = restore,
    containerHandleEvent = handleEvent,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  handleW = fromMaybe 5 (_spcHandleSize config)

  init wenv node = result where
    useModelValue value = resultWidget newNode where
      newState = state {
        _spsHandlePosUserSet = True,
        _spsHandlePos = value
      }
      newNode = node
        & L.widget .~ makeSplit isHorizontal config newState
    result = case getModelPos wenv config of
      Just val
        | val >= 0 && val <= 1 -> useModelValue val
      _ -> resultWidget node

  restore wenv oldState oldNode newNode = result where
    oldHandlePos = _spsHandlePos oldState
    modelPos = getModelPos wenv config
    newState = oldState {
      _spsHandlePos = fromMaybe oldHandlePos modelPos
    }
    result = resultWidget $ newNode
      & L.widget .~ makeSplit isHorizontal config newState

  handleEvent wenv target evt node = case evt of
    Move point
      | isTarget && isDragging -> Just resultDrag
      | isTarget && isInHandle point -> Just resultHover
      where
        Point px py = getValidHandlePos maxSize ra point children
        newHandlePos
          | isHorizontal = (px - ra ^. L.x) / maxSize
          | otherwise = (py - ra ^. L.y) / maxSize
        newState = state {
          _spsHandlePosUserSet = True,
          _spsHandlePos = newHandlePos
        }
        tmpNode = node
          & L.widget .~ makeSplit isHorizontal config newState
        newNode = widgetResize (tmpNode ^. L.widget) wenv vp ra tmpNode
        resultDrag
          | handlePos /= newHandlePos = newNode
              & L.requests <>~ Seq.fromList [cursorIconReq, RenderOnce]
          | otherwise = resultReqs node [cursorIconReq]
        resultHover = resultReqs node [cursorIconReq]
    _ -> Nothing
    where
      maxSize = _spsMaxSize state
      handlePos = _spsHandlePos state
      handleRect = _spsHandleRect state
      vp = node ^. L.info . L.viewport
      ra = node ^. L.info . L.renderArea
      children = node ^. L.children
      isTarget = target == node ^. L.info . L.path
      isDragging = isNodePressed wenv node
      isInHandle p = pointInRect p handleRect
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
    reqWS = FixedSize handleW
    reqW
      | isHorizontal = foldl1 sizeReqMergeSum [reqWS, reqW1, reqW2]
      | otherwise = foldl1 sizeReqMergeMax [reqW1, reqW2]
    reqH
      | isHorizontal = foldl1 sizeReqMergeMax [reqH1, reqH2]
      | otherwise = foldl1 sizeReqMergeSum [reqWS, reqH1, reqH2]

  resize wenv viewport renderArea children node = resized where
    style = activeStyle wenv node
    contentArea = fromMaybe def (removeOuterBounds style renderArea)
    Rect rx ry rw rh = contentArea
    (areas, newSize) = assignStackAreas isHorizontal contentArea children
    oldHandlePos = _spsHandlePos state
    sizeReq1 = sizeReq $ Seq.index children 0
    sizeReq2 = sizeReq $ Seq.index children 1
    valid1 = sizeReqValid sizeReq1 0 (newSize * oldHandlePos)
    valid2 = sizeReqValid sizeReq2 0 (newSize * (1 - oldHandlePos))
    validSize = valid1 && valid2
    handlePosUserSet = _spsHandlePosUserSet state
    customPos = isJust (_spcHandlePos config)
    ignoreSizeReq = Just True == _spcIgnoreChildResize config
    sizeReqEquals = (sizeReq1, sizeReq2) == _spsPrevReqs state
    useOldPos = customPos || ignoreSizeReq || sizeReqEquals
    handlePos
      | useOldPos && handlePosUserSet && validSize = oldHandlePos
      | otherwise = calcHandlePos newSize oldHandlePos renderArea children
    (w1, h1)
      | isHorizontal = ((newSize - handleW) * handlePos, rh)
      | otherwise = (rw, (newSize - handleW) * handlePos)
    (w2, h2)
      | isHorizontal = (newSize - w1 - handleW, rh)
      | otherwise = (rw, newSize - h1 - handleW)
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
      _spsMaxSize = newSize,
      _spsPrevReqs = (sizeReq1, sizeReq2)
    }
    events = fmap ($ handlePos) (_spcOnChange config)
    reqOnChange = _spcOnChangeReq config
    requestPos = setModelPos config handlePos
    result = resultWidget node
      & L.node . L.widget .~ makeSplit isHorizontal config newState
      & L.events .~ Seq.fromList events
      & L.requests .~ Seq.fromList (requestPos ++ reqOnChange)
    newRas = Seq.fromList [rect1, rect2]
    assignedArea = Seq.zip newRas newRas
    resized = (result, assignedArea)

  getValidHandlePos maxDim rect point children = addPoint origin newPoint where
    Rect rx ry _ _ = rect
    Point vx vy = rectBoundedPoint rect point
    origin = Point rx ry
    isVertical = not isHorizontal
    child1 = Seq.index children 0
    child2 = Seq.index children 1
    minSize1 = sizeReqMin (sizeReq child1)
    maxSize1 = sizeReqMax (sizeReq child1)
    minSize2 = sizeReqMin (sizeReq child2)
    maxSize2 = sizeReqMax (sizeReq child2)
    (tw, th)
      | isHorizontal = (max minSize1 (min maxSize1 $ abs (vx - rx)), 0)
      | otherwise = (0, max minSize1 (min maxSize1 $ abs (vy - ry)))
    newPoint
      | isHorizontal && tw + minSize2 > maxDim = Point (maxDim - minSize2) th
      | isHorizontal && maxDim - tw > maxSize2 = Point (maxDim - maxSize2) th
      | isVertical && th + minSize2 > maxDim = Point tw (maxDim - minSize2)
      | isVertical && maxDim - th > maxSize2 = Point tw (maxDim - maxSize2)
      | otherwise = Point tw th

  calcHandlePos maxDim handlePos rect children = newPos where
    Rect rx ry _ _ = rect
    point
      | isHorizontal = Point (rx + maxDim * handlePos) 0
      | otherwise = Point 0 (ry + maxDim * handlePos)
    Point px py = getValidHandlePos maxDim rect point children
    newPos
      | isHorizontal = (px - rx) / maxDim
      | otherwise = (py - ry) / maxDim

  selector
    | isHorizontal = _rW
    | otherwise = _rH

  sizeReq
    | isHorizontal = (^. L.info . L.sizeReqW)
    | otherwise = (^. L.info . L.sizeReqH)

setModelPos :: SplitCfg s e -> Double -> [WidgetRequest s]
setModelPos cfg
  | isJust (_spcHandlePos cfg) = widgetDataSet (fromJust $ _spcHandlePos cfg)
  | otherwise = const []

getModelPos :: WidgetEnv s e -> SplitCfg s e -> Maybe Double
getModelPos wenv cfg
  | isJust handlePosL = Just $ widgetDataGet model (fromJust handlePosL)
  | otherwise = Nothing
  where
    model = wenv ^. L.model
    handlePosL = _spcHandlePos cfg
