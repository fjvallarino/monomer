{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widgets.Dial (
  DialCfg,
  dial,
  dial_,
  dialV,
  dialV_,
  dialD_,
  dialWidth
) where

import Codec.Serialise
import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (.~), (<>~))
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)
import GHC.Generics

import qualified Data.Sequence as Seq

import Monomer.Core.NumUtil
import Monomer.Widgets.Single

import qualified Monomer.Lens as L

type DialValue a = (Eq a, Show a, NumRangeable a)

data DialCfg s e a = DialCfg {
  _dlcWidth :: Maybe Double,
  _dlcDragRate :: Maybe Double,
  _dlcOnFocus :: [e],
  _dlcOnFocusReq :: [WidgetRequest s],
  _dlcOnBlur :: [e],
  _dlcOnBlurReq :: [WidgetRequest s],
  _dlcOnChange :: [a -> e],
  _dlcOnChangeReq :: [WidgetRequest s]
}

instance Default (DialCfg s e a) where
  def = DialCfg {
    _dlcWidth = Nothing,
    _dlcDragRate = Nothing,
    _dlcOnFocus = [],
    _dlcOnFocusReq = [],
    _dlcOnBlur = [],
    _dlcOnBlurReq = [],
    _dlcOnChange = [],
    _dlcOnChangeReq = []
  }

instance Semigroup (DialCfg s e a) where
  (<>) t1 t2 = DialCfg {
    _dlcWidth = _dlcWidth t2 <|> _dlcWidth t1,
    _dlcDragRate = _dlcDragRate t2 <|> _dlcDragRate t1,
    _dlcOnFocus = _dlcOnFocus t1 <> _dlcOnFocus t2,
    _dlcOnFocusReq = _dlcOnFocusReq t1 <> _dlcOnFocusReq t2,
    _dlcOnBlur = _dlcOnBlur t1 <> _dlcOnBlur t2,
    _dlcOnBlurReq = _dlcOnBlurReq t1 <> _dlcOnBlurReq t2,
    _dlcOnChange = _dlcOnChange t1 <> _dlcOnChange t2,
    _dlcOnChangeReq = _dlcOnChangeReq t1 <> _dlcOnChangeReq t2
  }

instance Monoid (DialCfg s e a) where
  mempty = def

instance CmbDragRate (DialCfg s e a) Double where
  dragRate rate = def {
    _dlcDragRate = Just rate
  }

instance CmbOnFocus (DialCfg s e a) e where
  onFocus fn = def {
    _dlcOnFocus = [fn]
  }

instance CmbOnFocusReq (DialCfg s e a) s where
  onFocusReq req = def {
    _dlcOnFocusReq = [req]
  }

instance CmbOnBlur (DialCfg s e a) e where
  onBlur fn = def {
    _dlcOnBlur = [fn]
  }

instance CmbOnBlurReq (DialCfg s e a) s where
  onBlurReq req = def {
    _dlcOnBlurReq = [req]
  }

instance CmbOnChange (DialCfg s e a) a e where
  onChange fn = def {
    _dlcOnChange = [fn]
  }

instance CmbOnChangeReq (DialCfg s e a) s where
  onChangeReq req = def {
    _dlcOnChangeReq = [req]
  }

dialWidth :: Double -> DialCfg s e a
dialWidth w = def {
  _dlcWidth = Just w
}

dial :: DialValue a => ALens' s a -> a -> a -> WidgetNode s e
dial field minVal maxVal = dial_ field minVal maxVal def

dial_
  :: DialValue a => ALens' s a -> a -> a -> [DialCfg s e a] -> WidgetNode s e
dial_ field minVal maxVal cfgs = dialD_ (WidgetLens field) minVal maxVal cfgs

dialV :: DialValue a => a -> (a -> e) -> a -> a -> WidgetNode s e
dialV value handler minVal maxVal = dialV_ value handler minVal maxVal def

dialV_
  :: DialValue a => a -> (a -> e) -> a -> a -> [DialCfg s e a] -> WidgetNode s e
dialV_ value handler minVal maxVal configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newNode = dialD_ widgetData minVal maxVal newConfigs

dialD_
  :: DialValue a
  => WidgetData s a
  -> a
  -> a
  -> [DialCfg s e a]
  -> WidgetNode s e
dialD_ widgetData minVal maxVal configs = dialNode where
  config = mconcat configs
  state = DialState 0 0
  widget = makeDial widgetData minVal maxVal config state
  dialNode = defaultWidgetNode "dial" widget
    & L.info . L.focusable .~ True

data DialState = DialState {
  _dlsMaxPos :: Integer,
  _dlsPos :: Integer
} deriving (Eq, Show, Generic, Serialise)

makeDial
  :: DialValue a
  => WidgetData s a
  -> a
  -> a
  -> DialCfg s e a
  -> DialState
  -> Widget s e
makeDial field minVal maxVal config state = widget where
  widget = createSingle state def {
    singleStyleChangeCfg = def & L.cursorEvt .~ isOnMove,
    singleGetBaseStyle = getBaseStyle,
    singleGetActiveStyle = getActiveStyle,
    singleInit = init,
    singleRestore = restore,
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  dragRate
    | isJust (_dlcDragRate config) = fromJust (_dlcDragRate config)
    | otherwise = numToFrac (maxVal - minVal) / 1000

  getBaseStyle wenv node = Just style where
    style = collectTheme wenv L.dialStyle

  getActiveStyle wenv node = style where
    (_, dialArea) = getDialInfo wenv node config
    style = activeStyle_ (isNodeHoveredEllipse_ dialArea) wenv node

  init wenv node = resultWidget resNode where
    newState = newStateFromModel wenv node state
    resNode = node
      & L.widget .~ makeDial field minVal maxVal config newState

  restore wenv oldState oldNode newNode = resultWidget resNode where
    newState
      | isNodePressed wenv newNode = oldState
      | otherwise = newStateFromModel wenv newNode oldState
    resNode = newNode
      & L.widget .~ makeDial field minVal maxVal config newState

  newStateFromModel wenv node oldState = newState where
    currVal = widgetDataGet (wenv ^. L.model) field
    newMaxPos = numToIntegral (numMulFrac (maxVal - minVal) (1.0 / dragRate))
    newPos = numToIntegral $ numMulFrac (currVal - minVal) (1.0 / dragRate)
    newState = oldState {
      _dlsMaxPos = newMaxPos,
      _dlsPos = newPos
    }

  handleEvent wenv target evt node = case evt of
    Focus -> handleFocusChange _dlcOnFocus _dlcOnFocusReq config node
    Blur -> handleFocusChange _dlcOnBlur _dlcOnBlurReq config node
    KeyAction mod code KeyPressed
      | isCtrl && isKeyUp code -> handleNewPos (pos + warpSpeed)
      | isCtrl && isKeyDown code -> handleNewPos (pos - warpSpeed)
      | isShiftPressed evt && isKeyUp code -> handleNewPos (pos + baseSpeed)
      | isShiftPressed evt && isKeyDown code -> handleNewPos (pos - baseSpeed)
      | isKeyUp code -> handleNewPos (pos + fastSpeed)
      | isKeyDown code -> handleNewPos (pos - fastSpeed)
      where
        DialState maxPos pos = state
        isCtrl = isShortCutControl wenv mod
        baseSpeed = max 1 $ round (fromIntegral maxPos / 1000)
        fastSpeed = max 1 $ round (fromIntegral maxPos / 100)
        warpSpeed = max 1 $ round (fromIntegral maxPos / 10)
        vPos pos = restrictValue 0 maxPos pos
        newResult newPos = addReqsEvts (resultWidget newNode) newVal where
          newVal = valueFromPos minVal dragRate newPos
          newState = state { _dlsPos = newPos }
          newNode = node
            & L.widget .~ makeDial field minVal maxVal config newState
        handleNewPos newPos
          | vPos newPos /= pos = Just $ newResult (vPos newPos)
          | otherwise = Nothing
    Move point
      | isNodePressed wenv node -> Just result where
        (_, start) = fromJust $ wenv ^. L.mainBtnPress
        (_, newVal) = posFromPoint minVal maxVal state dragRate start point
        result = addReqsEvts (resultReqs node [RenderOnce]) newVal
    ButtonAction point btn ReleasedBtn clicks
      | clicks == 0 -> Just result where
        reqs = [RenderOnce]
        newState = newStateFromModel wenv node state
        newNode = node
          & L.widget .~ makeDial field minVal maxVal config newState
        result = resultReqs newNode reqs
    _ -> Nothing
    where
      (_, dialArea) = getDialInfo wenv node config
      path = node ^. L.info . L.path
      isSelectKey code = isKeyReturn code || isKeySpace code
      addReqsEvts result newVal = newResult where
        evts = fmap ($ newVal) (_dlcOnChange config)
        reqs = widgetDataSet field newVal ++ _dlcOnChangeReq config
        newResult = result
          & L.events .~ Seq.fromList evts
          & L.requests <>~ Seq.fromList reqs

  getSizeReq wenv currState node = req where
    theme = activeTheme wenv node
    width = fromMaybe (theme ^. L.dialWidth) (_dlcWidth config)
    req = (FixedSize width, FixedSize width)

  render renderer wenv node = do
    drawArcBorder renderer dialArea start endFg CW (Just fgColor) dialBW
    drawArcBorder renderer dialArea start endHl CW (Just hlColor) dialBW
    where
      model = _weModel wenv
      value = widgetDataGet model field
      (dialCenter, dialArea) = getDialInfo wenv node config
      DialState maxPos pos = newStateFromModel wenv node state
      posPct = fromIntegral pos / fromIntegral maxPos
      dialBW = max 1 (_rW dialArea * 0.1)
      style = getActiveStyle wenv node
      fgColor = styleFgColor style
      hlColor = styleHlColor style
      start = 90 + 45
      endFg = 45
      endHl = start + 270 * posPct

posFromPoint
  :: DialValue a
  => a
  -> a
  -> DialState
  -> Double
  -> Point
  -> Point
  -> (Integer, a)
posFromPoint minVal maxVal state dragRate stPoint point = (newPos, newVal) where
  DialState maxPos pos = state
  Point _ dy = subPoint stPoint point
  tmpPos = pos + round dy
  newPos = restrictValue 0 maxPos tmpPos
  newVal = valueFromPos minVal dragRate newPos

valueFromPos :: DialValue a => a -> Double -> Integer -> a
valueFromPos minVal dragRate newPos = newVal where
  newVal = numAddFrac minVal (dragRate * fromIntegral newPos)

getDialInfo :: WidgetEnv s e -> WidgetNode s e -> DialCfg s e a -> (Point, Rect)
getDialInfo wenv node config = (dialCenter, dialArea) where
  theme = activeTheme wenv node
  style = activeStyle wenv node
  rarea = getContentArea style node
  dialW = fromMaybe (theme ^. L.dialWidth) (_dlcWidth config)
  dialL = _rX rarea + (_rW rarea - dialW) / 2
  dialT = _rY rarea + (_rH rarea - dialW) / 2
  dialCenter = Point (dialL + dialW / 2) (dialT + dialW / 2)
  dialArea = Rect dialL dialT dialW dialW
