{-|
Module      : Monomer.Widgets.Containers.Split
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Splits the assigned space into two parts, vertically or horizontally, which are
assigned to its two child nodes. The space assigned depends on the style and
size requirements of each child node.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Monomer.Widgets.Containers.Split (
  -- * Configuration
  SplitCfg,
  splitHandlePos,
  splitHandlePosV,
  splitHandleSize,
  splitIgnoreChildResize,
  -- * Constructors
  hsplit,
  hsplit_,
  vsplit,
  vsplit_
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (.~), (<>~))
import Data.Default
import Data.Maybe
import Data.Tuple (swap)
import GHC.Generics

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container
import Monomer.Widgets.Containers.Stack (assignStackAreas)

import qualified Monomer.Lens as L

{-|
Configuration options for split:

- 'splitHandlePos': lens to a model field which provides the handle position.
- 'splitHandlePosV': value which provides the handle position.
- 'splitHandleSize': width of the handle.
- 'splitIgnoreChildResize': whether to ignore changes in size to its children
  (otherwise, the handle position may change because of this).
- 'onChange': raises an event when the handle is moved.
- 'onChangeReq': generates a WidgetReqest when the handle is moved.
-}
data SplitCfg s e = SplitCfg {
  _spcHandlePos :: Maybe (WidgetData s Double),
  _spcHandleSize :: Maybe Double,
  _spcIgnoreChildResize :: Maybe Bool,
  _spcOnChangeReq :: [Double -> WidgetRequest s e]
}

instance Default (SplitCfg s e) where
  def = SplitCfg {
    _spcHandlePos = Nothing,
    _spcHandleSize = Nothing,
    _spcIgnoreChildResize = Nothing,
    _spcOnChangeReq = []
  }

instance Semigroup (SplitCfg s e) where
  (<>) s1 s2 = SplitCfg {
    _spcHandlePos = _spcHandlePos s2 <|> _spcHandlePos s1,
    _spcHandleSize = _spcHandleSize s2 <|> _spcHandleSize s1,
    _spcIgnoreChildResize = _spcIgnoreChildResize s2 <|> _spcIgnoreChildResize s1,
    _spcOnChangeReq = _spcOnChangeReq s2 <|> _spcOnChangeReq s1
  }

instance Monoid (SplitCfg s e) where
  mempty = def

instance WidgetEvent e => CmbOnChange (SplitCfg s e) Double e where
  onChange fn = def {
    _spcOnChangeReq = [RaiseEvent . fn]
  }

instance CmbOnChangeReq (SplitCfg s e) s e Double where
  onChangeReq req = def {
    _spcOnChangeReq = [req]
  }

-- | Lens to a model field which provides the handle position.
splitHandlePos :: ALens' s Double -> SplitCfg s e
splitHandlePos field = def {
  _spcHandlePos = Just (WidgetLens field)
}

-- | Value which provides the handle position.
splitHandlePosV :: Double -> SplitCfg s e
splitHandlePosV value = def {
  _spcHandlePos = Just (WidgetValue value)
}

-- | Width of the handle.
splitHandleSize :: Double -> SplitCfg s e
splitHandleSize w = def {
  _spcHandleSize = Just w
}

-- | Whether to ignore changes in size to its children.
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
} deriving (Eq, Show, Generic)

-- | Creates a horizontal split between the two provided nodes.
hsplit :: WidgetEvent e => (WidgetNode s e, WidgetNode s e) -> WidgetNode s e
hsplit nodes = hsplit_ def nodes

-- | Creates a horizontal split between the two provided nodes. Accepts config.
hsplit_
  :: WidgetEvent e
  => [SplitCfg s e] -> (WidgetNode s e, WidgetNode s e) -> WidgetNode s e
hsplit_ configs nodes = split_ True nodes configs

-- | Creates a vertical split between the two provided nodes.
vsplit :: WidgetEvent e => (WidgetNode s e, WidgetNode s e) -> WidgetNode s e
vsplit nodes = vsplit_ def nodes

-- | Creates a vertical split between the two provided nodes. Accepts config.
vsplit_
  :: WidgetEvent e
  => [SplitCfg s e]
  -> (WidgetNode s e, WidgetNode s e)
  -> WidgetNode s e
vsplit_ configs nodes = split_ False nodes configs

split_
  :: WidgetEvent e
  => Bool
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

makeSplit :: WidgetEvent e => Bool -> SplitCfg s e -> SplitState -> Widget s e
makeSplit isHorizontal config state = widget where
  widget = createContainer state def {
    containerUseCustomCursor = True,
    containerLayoutDirection = getLayoutDirection isHorizontal,
    containerInit = init,
    containerMerge = merge,
    containerHandleEvent = handleEvent,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  handleW = fromMaybe 5 (_spcHandleSize config)

  init wenv node = result where
    useModelValue value = resultNode newNode where
      newState = state {
        _spsHandlePosUserSet = True,
        _spsHandlePos = value
      }
      newNode = node
        & L.widget .~ makeSplit isHorizontal config newState
    result = case getModelPos wenv config of
      Just val
        | val >= 0 && val <= 1 -> useModelValue val
      _ -> resultNode node

  merge wenv newNode oldNode oldState = result where
    oldHandlePos = _spsHandlePos oldState
    modelPos = getModelPos wenv config
    newState = oldState {
      _spsHandlePos = fromMaybe oldHandlePos modelPos
    }
    result = resultNode $ newNode
      & L.widget .~ makeSplit isHorizontal config newState

  handleEvent wenv node target evt = case evt of
    Move point
      | isTarget && isDragging -> Just resultDrag
      | isInHandle point && curIcon /= dragIcon -> Just resultHover
      | not (isInHandle point) && curPath == path -> Just resultReset
      where
        Point px py = getValidHandlePos maxSize vp point children
        newHandlePos
          | isHorizontal = (px - vp ^. L.x) / maxSize
          | otherwise = (py - vp ^. L.y) / maxSize
        newState = state {
          _spsHandlePosUserSet = True,
          _spsHandlePos = newHandlePos
        }

        resizeReq = const True
        tmpNode = node
          & L.widget .~ makeSplit isHorizontal config newState
        newNode = widgetResize (tmpNode ^. L.widget) wenv tmpNode vp resizeReq

        resultDrag
          | handlePos /= newHandlePos = newNode
              & L.requests <>~ Seq.fromList [cursorIconReq, RenderOnce]
          | otherwise = resultReqs node [cursorIconReq]

        resultHover = resultReqs node [cursorIconReq]
        resultReset = resultReqs node [ResetCursorIcon widgetId]
    _ -> Nothing
    where
      maxSize = _spsMaxSize state
      handlePos = _spsHandlePos state
      handleRect = _spsHandleRect state

      widgetId = node ^. L.info . L.widgetId
      path = node ^. L.info . L.path
      vp = node ^. L.info . L.viewport
      children = node ^. L.children

      isTarget = target == node ^. L.info . L.path
      (curPath, curIcon) = fromMaybe def (wenv ^. L.cursor)
      isDragging = isNodePressed wenv node
      isInHandle p = pointInRect p handleRect
      dragIcon
        | isHorizontal = CursorSizeH
        | otherwise = CursorSizeV
      cursorIconReq = SetCursorIcon widgetId dragIcon

  getSizeReq :: ContainerGetSizeReqHandler s e
  getSizeReq wenv node children = (reqW, reqH) where
    node1 = Seq.index children 0
    node2 = Seq.index children 1

    reqW1 = node1 ^. L.info . L.sizeReqW
    reqH1 = node1 ^. L.info . L.sizeReqH
    reqW2 = node2 ^. L.info . L.sizeReqW
    reqH2 = node2 ^. L.info . L.sizeReqH

    reqWS = fixedSize handleW
    reqW
      | isHorizontal = foldl1 sizeReqMergeSum [reqWS, reqW1, reqW2]
      | otherwise = foldl1 sizeReqMergeMax [reqW1, reqW2]
    reqH
      | isHorizontal = foldl1 sizeReqMergeMax [reqH1, reqH2]
      | otherwise = foldl1 sizeReqMergeSum [reqWS, reqH1, reqH2]

  resize wenv node viewport children = resized where
    style = currentStyle wenv node
    contentArea = fromMaybe def (removeOuterBounds style viewport)
    Rect rx ry rw rh = contentArea
    (areas, newSize) = assignStackAreas isHorizontal contentArea children
    oldHandlePos = _spsHandlePos state

    sizeReq1 = sizeReq $ Seq.index children 0
    sizeReq2 = sizeReq $ Seq.index children 1
    valid1 = sizeReqValid sizeReq1 0 (newSize * oldHandlePos)
    valid2 = sizeReqValid sizeReq2 0 (newSize * (1 - oldHandlePos))
    validSize = valid1 && valid2

    handlePosUserSet = _spsHandlePosUserSet state
    ignoreSizeReq = Just True == _spcIgnoreChildResize config
    sizeReqEquals = (sizeReq1, sizeReq2) == _spsPrevReqs state
    resizeNeeded = not (sizeReqEquals && handlePosUserSet)

    customPos = isJust (_spcHandlePos config)
    useOldPos = customPos || ignoreSizeReq || sizeReqEquals
    initialPos = initialHandlePos children

    handlePos
      | useOldPos && handlePosUserSet && validSize = oldHandlePos
      | resizeNeeded = calcHandlePos newSize initialPos viewport children
      | otherwise = calcHandlePos newSize oldHandlePos viewport children
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

    reqOnChange = fmap ($ handlePos) (_spcOnChangeReq config)
    requestPos = setModelPos config handlePos
    result = resultNode node
      & L.node . L.widget .~ makeSplit isHorizontal config newState
      & L.requests .~ Seq.fromList (requestPos ++ reqOnChange)
    newVps = Seq.fromList [rect1, rect2]
    resized
      | node ^. L.info . L.visible = (result, newVps)
      | otherwise = (resultNode node, newVps)

  getValidHandlePos maxDim vp handleXY children = addPoint origin newPoint where
    Rect rx ry _ _ = vp
    Point vx vy = rectBoundedPoint vp handleXY

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

  calcHandlePos maxDim handlePos vp children = newPos where
    Rect rx ry _ _ = vp
    handleXY
      | isHorizontal = Point (rx + maxDim * handlePos) 0
      | otherwise = Point 0 (ry + maxDim * handlePos)
    Point px py = getValidHandlePos maxDim vp handleXY children
    newPos
      | isHorizontal = (px - rx) / maxDim
      | otherwise = (py - ry) / maxDim

  initialHandlePos children = handlePos where
    child1 = Seq.index children 0
    child2 = Seq.index children 1
    maxSize1 = sizeReqMaxBounded (sizeReq child1)
    maxSize2 = sizeReqMaxBounded (sizeReq child2)
    handlePos = maxSize1 / (maxSize1 + maxSize2)

  selector
    | isHorizontal = _rW
    | otherwise = _rH

  sizeReq
    | isHorizontal = (^. L.info . L.sizeReqW)
    | otherwise = (^. L.info . L.sizeReqH)

setModelPos :: SplitCfg s e -> Double -> [WidgetRequest s e]
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
