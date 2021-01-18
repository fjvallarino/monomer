{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Debug.Trace

import Codec.Serialise
import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (.~))
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)
import GHC.Generics

import Monomer.Core.NumUtil
import Monomer.Widgets.Single

import qualified Monomer.Lens as L

type DialValue a = (Eq a, Show a, NumRangeable a)

data DialCfg s e a = DialCfg {
  _rdcWidth :: Maybe Double,
  _rdcDragRate :: Maybe Double,
  _rdcOnFocus :: [e],
  _rdcOnFocusReq :: [WidgetRequest s],
  _rdcOnBlur :: [e],
  _rdcOnBlurReq :: [WidgetRequest s],
  _rdcOnChange :: [a -> e],
  _rdcOnChangeReq :: [WidgetRequest s]
}

instance Default (DialCfg s e a) where
  def = DialCfg {
    _rdcWidth = Nothing,
    _rdcDragRate = Nothing,
    _rdcOnFocus = [],
    _rdcOnFocusReq = [],
    _rdcOnBlur = [],
    _rdcOnBlurReq = [],
    _rdcOnChange = [],
    _rdcOnChangeReq = []
  }

instance Semigroup (DialCfg s e a) where
  (<>) t1 t2 = DialCfg {
    _rdcWidth = _rdcWidth t2 <|> _rdcWidth t1,
    _rdcDragRate = _rdcDragRate t2 <|> _rdcDragRate t1,
    _rdcOnFocus = _rdcOnFocus t1 <> _rdcOnFocus t2,
    _rdcOnFocusReq = _rdcOnFocusReq t1 <> _rdcOnFocusReq t2,
    _rdcOnBlur = _rdcOnBlur t1 <> _rdcOnBlur t2,
    _rdcOnBlurReq = _rdcOnBlurReq t1 <> _rdcOnBlurReq t2,
    _rdcOnChange = _rdcOnChange t1 <> _rdcOnChange t2,
    _rdcOnChangeReq = _rdcOnChangeReq t1 <> _rdcOnChangeReq t2
  }

instance Monoid (DialCfg s e a) where
  mempty = def

instance CmbDragRate (DialCfg s e a) Double where
  dragRate rate = def {
    _rdcDragRate = Just rate
  }

instance CmbOnFocus (DialCfg s e a) e where
  onFocus fn = def {
    _rdcOnFocus = [fn]
  }

instance CmbOnFocusReq (DialCfg s e a) s where
  onFocusReq req = def {
    _rdcOnFocusReq = [req]
  }

instance CmbOnBlur (DialCfg s e a) e where
  onBlur fn = def {
    _rdcOnBlur = [fn]
  }

instance CmbOnBlurReq (DialCfg s e a) s where
  onBlurReq req = def {
    _rdcOnBlurReq = [req]
  }

instance CmbOnChange (DialCfg s e a) a e where
  onChange fn = def {
    _rdcOnChange = [fn]
  }

instance CmbOnChangeReq (DialCfg s e a) s where
  onChangeReq req = def {
    _rdcOnChangeReq = [req]
  }

dialWidth :: Double -> DialCfg s e a
dialWidth w = def {
  _rdcWidth = Just w
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
    | isJust (_rdcDragRate config) = fromJust (_rdcDragRate config)
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
    Focus -> handleFocusChange _rdcOnFocus _rdcOnFocusReq config node
    Blur -> handleFocusChange _rdcOnBlur _rdcOnBlurReq config node
    Move point
      | isNodePressed wenv node -> Just result where
        (_, start) = fromJust $ wenv ^. L.mainBtnPress
        (_, newVal) = posFromPoint minVal maxVal state dragRate start point
        setValueReq = widgetDataSet field newVal
        reqs = RenderOnce : setValueReq
        result = resultReqs node reqs
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

  getSizeReq wenv currState node = req where
    theme = activeTheme wenv node
    width = fromMaybe (theme ^. L.dialWidth) (_rdcWidth config)
    req = (FixedSize width, FixedSize width)

  render renderer wenv node = do
    drawArcBorder renderer dialArea start endBg CW (Just fgColor) dialBW
    drawArcBorder renderer dialArea start endVal CW (Just hlColor) dialBW
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
      endBg = 45
      endVal = start + 270 * posPct

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
  newVal = numAddFrac minVal (dragRate * fromIntegral newPos)

getDialInfo :: WidgetEnv s e -> WidgetNode s e -> DialCfg s e a -> (Point, Rect)
getDialInfo wenv node config = (dialCenter, dialArea) where
  theme = activeTheme wenv node
  style = activeStyle wenv node
  rarea = getContentArea style node
  dialW = fromMaybe (theme ^. L.dialWidth) (_rdcWidth config)
  dialL = _rX rarea + (_rW rarea - dialW) / 2
  dialT = _rY rarea + (_rH rarea - dialW) / 2
  dialCenter = Point (dialL + dialW / 2) (dialT + dialW / 2)
  dialArea = Rect dialL dialT dialW dialW
