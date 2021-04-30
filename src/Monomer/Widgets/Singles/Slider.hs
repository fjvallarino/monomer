{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widgets.Singles.Slider (
  hslider,
  hslider_,
  vslider,
  vslider_,
  hsliderV,
  hsliderV_,
  vsliderV,
  vsliderV_,
  sliderRadius,
  sliderWidth
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (.~), (%~), (<>~))
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics

import qualified Data.Sequence as Seq

import Monomer.Widgets.Single

import qualified Monomer.Lens as L

type SliderValue a = (Eq a, Show a, Real a, FromFractional a, Typeable a)

data SliderCfg s e a = SliderCfg {
  _slcRadius :: Maybe Double,
  _slcWidth :: Maybe Double,
  _slcDragRate :: Maybe Rational,
  _slcOnFocus :: [e],
  _slcOnFocusReq :: [WidgetRequest s e],
  _slcOnBlur :: [e],
  _slcOnBlurReq :: [WidgetRequest s e],
  _slcOnChange :: [a -> e],
  _slcOnChangeReq :: [WidgetRequest s e]
}

instance Default (SliderCfg s e a) where
  def = SliderCfg {
    _slcRadius = Nothing,
    _slcWidth = Nothing,
    _slcDragRate = Nothing,
    _slcOnFocus = [],
    _slcOnFocusReq = [],
    _slcOnBlur = [],
    _slcOnBlurReq = [],
    _slcOnChange = [],
    _slcOnChangeReq = []
  }

instance Semigroup (SliderCfg s e a) where
  (<>) t1 t2 = SliderCfg {
    _slcRadius = _slcRadius t2 <|> _slcRadius t1,
    _slcWidth = _slcWidth t2 <|> _slcWidth t1,
    _slcDragRate = _slcDragRate t2 <|> _slcDragRate t1,
    _slcOnFocus = _slcOnFocus t1 <> _slcOnFocus t2,
    _slcOnFocusReq = _slcOnFocusReq t1 <> _slcOnFocusReq t2,
    _slcOnBlur = _slcOnBlur t1 <> _slcOnBlur t2,
    _slcOnBlurReq = _slcOnBlurReq t1 <> _slcOnBlurReq t2,
    _slcOnChange = _slcOnChange t1 <> _slcOnChange t2,
    _slcOnChangeReq = _slcOnChangeReq t1 <> _slcOnChangeReq t2
  }

instance Monoid (SliderCfg s e a) where
  mempty = def

instance CmbDragRate (SliderCfg s e a) Rational where
  dragRate rate = def {
    _slcDragRate = Just rate
  }

instance CmbOnFocus (SliderCfg s e a) e where
  onFocus fn = def {
    _slcOnFocus = [fn]
  }

instance CmbOnFocusReq (SliderCfg s e a) s e where
  onFocusReq req = def {
    _slcOnFocusReq = [req]
  }

instance CmbOnBlur (SliderCfg s e a) e where
  onBlur fn = def {
    _slcOnBlur = [fn]
  }

instance CmbOnBlurReq (SliderCfg s e a) s e where
  onBlurReq req = def {
    _slcOnBlurReq = [req]
  }

instance CmbOnChange (SliderCfg s e a) a e where
  onChange fn = def {
    _slcOnChange = [fn]
  }

instance CmbOnChangeReq (SliderCfg s e a) s e where
  onChangeReq req = def {
    _slcOnChangeReq = [req]
  }

sliderRadius :: Double -> SliderCfg s e a
sliderRadius w = def {
  _slcRadius = Just w
}

sliderWidth :: Double -> SliderCfg s e a
sliderWidth w = def {
  _slcWidth = Just w
}

data SliderState = SliderState {
  _slsMaxPos :: Integer,
  _slsPos :: Integer
} deriving (Eq, Show, Generic)

hslider
  :: (SliderValue a, WidgetEvent e) => ALens' s a -> a -> a -> WidgetNode s e
hslider field minVal maxVal = hslider_ field minVal maxVal def

hslider_
  :: (SliderValue a, WidgetEvent e)
  => ALens' s a
  -> a
  -> a
  -> [SliderCfg s e a]
  -> WidgetNode s e
hslider_ field minVal maxVal cfg = sliderD_ True wlens minVal maxVal cfg where
  wlens = WidgetLens field

vslider
  :: (SliderValue a, WidgetEvent e) => ALens' s a -> a -> a -> WidgetNode s e
vslider field minVal maxVal = vslider_ field minVal maxVal def

vslider_
  :: (SliderValue a, WidgetEvent e)
  => ALens' s a
  -> a
  -> a
  -> [SliderCfg s e a]
  -> WidgetNode s e
vslider_ field minVal maxVal cfg = sliderD_ False wlens minVal maxVal cfg where
  wlens = WidgetLens field

hsliderV
  :: (SliderValue a, WidgetEvent e) => a -> (a -> e) -> a -> a -> WidgetNode s e
hsliderV value handler minVal maxVal = hsliderV_ value handler minVal maxVal def

hsliderV_
  :: (SliderValue a, WidgetEvent e)
  => a
  -> (a -> e)
  -> a
  -> a
  -> [SliderCfg s e a]
  -> WidgetNode s e
hsliderV_ value handler minVal maxVal configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newNode = sliderD_ True widgetData minVal maxVal newConfigs

vsliderV
  :: (SliderValue a, WidgetEvent e) => a -> (a -> e) -> a -> a -> WidgetNode s e
vsliderV value handler minVal maxVal = vsliderV_ value handler minVal maxVal def

vsliderV_
  :: (SliderValue a, WidgetEvent e)
  => a
  -> (a -> e)
  -> a
  -> a
  -> [SliderCfg s e a]
  -> WidgetNode s e
vsliderV_ value handler minVal maxVal configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newNode = sliderD_ False widgetData minVal maxVal newConfigs

sliderD_
  :: (SliderValue a, WidgetEvent e)
  => Bool
  -> WidgetData s a
  -> a
  -> a
  -> [SliderCfg s e a]
  -> WidgetNode s e
sliderD_ isHz widgetData minVal maxVal configs = sliderNode where
  config = mconcat configs
  state = SliderState 0 0
  widget = makeSlider isHz widgetData minVal maxVal config state
  sliderNode = defaultWidgetNode "slider" widget
    & L.info . L.focusable .~ True

makeSlider
  :: (SliderValue a, WidgetEvent e)
  => Bool
  -> WidgetData s a
  -> a
  -> a
  -> SliderCfg s e a
  -> SliderState
  -> Widget s e
makeSlider isHz field minVal maxVal config state = widget where
  widget = createSingle state def {
    singleGetBaseStyle = getBaseStyle,
    singleInit = init,
    singleMerge = merge,
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  dragRate
    | isJust (_slcDragRate config) = fromJust (_slcDragRate config)
    | otherwise = toRational (maxVal - minVal) / 1000

  getBaseStyle wenv node = Just style where
    style = collectTheme wenv L.sliderStyle

  init wenv node = resultWidget resNode where
    newState = newStateFromModel wenv node state
    resNode = node
      & L.widget .~ makeSlider isHz field minVal maxVal config newState

  merge wenv newNode oldNode oldState = resultWidget resNode where
    newState
      | isNodePressed wenv newNode = oldState
      | otherwise = newStateFromModel wenv newNode oldState
    resNode = newNode
      & L.widget .~ makeSlider isHz field minVal maxVal config newState

  handleEvent wenv node target evt = case evt of
    Focus -> handleFocusChange _slcOnFocus _slcOnFocusReq config node
    Blur -> handleFocusChange _slcOnBlur _slcOnBlurReq config node
    KeyAction mod code KeyPressed
      | isCtrl && isKeyUp code -> handleNewPos (pos + warpSpeed)
      | isCtrl && isKeyDown code -> handleNewPos (pos - warpSpeed)
      | isShiftPressed mod && isKeyUp code -> handleNewPos (pos + baseSpeed)
      | isShiftPressed mod && isKeyDown code -> handleNewPos (pos - baseSpeed)
      | isKeyUp code -> handleNewPos (pos + fastSpeed)
      | isKeyDown code -> handleNewPos (pos - fastSpeed)
      where
        SliderState maxPos pos = state
        isCtrl = isShortCutControl wenv mod
        baseSpeed = max 1 $ round (fromIntegral maxPos / 1000)
        fastSpeed = max 1 $ round (fromIntegral maxPos / 100)
        warpSpeed = max 1 $ round (fromIntegral maxPos / 10)
        vPos pos = restrictValue 0 maxPos pos
        newResult newPos = addReqsEvts (resultWidget newNode) newVal where
          newVal = valueFromPos minVal dragRate newPos
          newState = state { _slsPos = newPos }
          newWidget = makeSlider isHz field minVal maxVal config newState
          newNode = node
            & L.widget .~ newWidget
        handleNewPos newPos
          | vPos newPos /= pos = Just $ newResult (vPos newPos)
          | otherwise = Nothing
    Move point
      | isNodePressed wenv node -> Just result where
        (_, newVal) = posFromMouse isHz minVal maxVal state dragRate vp point
        result = addReqsEvts (resultReqs node [RenderOnce]) newVal
    ButtonAction point btn PressedBtn clicks
      | clicks == 1 -> Just result where
        (_, newVal) = posFromMouse isHz minVal maxVal state dragRate vp point
        result = addReqsEvts (resultReqs node [RenderOnce]) newVal
    ButtonAction point btn ReleasedBtn clicks
      | clicks <= 1 -> Just result where
        reqs = [RenderOnce]
        newState = newStateFromModel wenv node state
        newNode = node
          & L.widget .~ makeSlider isHz field minVal maxVal config newState
        result = resultReqs newNode reqs
    _ -> Nothing
    where
      style = activeStyle wenv node
      sliderArea = getContentArea style node
      path = node ^. L.info . L.path
      vp = getContentArea style node
      isSelectKey code = isKeyReturn code || isKeySpace code
      addReqsEvts result newVal = newResult where
        currVal = widgetDataGet (wenv ^. L.model) field
        evts = RaiseEvent <$> fmap ($ newVal) (_slcOnChange config)
        reqs = widgetDataSet field newVal ++ _slcOnChangeReq config
        newResult
          | currVal /= newVal = result
              & L.requests <>~ Seq.fromList (reqs <> evts)
          | otherwise = result

  getSizeReq wenv node = req where
    theme = activeTheme wenv node
    maxPos = realToFrac (toRational (maxVal - minVal) / dragRate)
    width = fromMaybe (theme ^. L.sliderWidth) (_slcWidth config)
    req
      | isHz = (flexSize maxPos 1, fixedSize width)
      | otherwise = (fixedSize width, flexSize maxPos 1)

  render wenv node renderer = do
    drawRect renderer sliderBgArea (Just sndColor) sliderRadius
    drawRect renderer sliderFgArea (Just fgColor) sliderRadius
    where
      theme = activeTheme wenv node
      style = activeStyle wenv node
      fgColor = styleFgColor style
      sndColor = styleSndColor style
      sliderRadiusVal = _slcRadius config <|> theme ^. L.sliderRadius
      sliderRadius = radius <$> sliderRadiusVal
      SliderState maxPos pos = newStateFromModel wenv node state
      posPct = fromIntegral pos / fromIntegral maxPos
      sliderBgArea = getContentArea style node
      sliderFgArea
        | isHz = sliderBgArea & L.w %~ (*posPct)
        | otherwise = sliderBgArea
            & L.y %~ (+ (sliderBgArea ^. L.h * posPct))
            & L.h %~ (* (1 - posPct))

  newStateFromModel wenv node oldState = newState where
    currVal = widgetDataGet (wenv ^. L.model) field
    newMaxPos = round (toRational (maxVal - minVal) / dragRate)
    newPos = round (toRational (currVal - minVal) / dragRate)
    newState = oldState {
      _slsMaxPos = newMaxPos,
      _slsPos = newPos
    }

posFromMouse
  :: SliderValue a
  => Bool
  -> a
  -> a
  -> SliderState
  -> Rational
  -> Rect
  -> Point
  -> (Integer, a)
posFromMouse isHz minVal maxVal state dragRate vp point = result where
  SliderState maxPos _ = state
  sliderStart = Point (vp ^. L.x) (vp ^. L.y)
  Point dx dy = subPoint point sliderStart
  tmpPos
    | isHz = round (dx * fromIntegral maxPos / vp ^. L.w)
    | otherwise = round (dy * fromIntegral maxPos / vp ^. L.h)
  newPos = restrictValue 0 maxPos tmpPos
  newVal = valueFromPos minVal dragRate newPos
  result = (newPos, newVal)

valueFromPos :: SliderValue a => a -> Rational -> Integer -> a
valueFromPos minVal dragRate newPos = newVal where
  newVal = minVal + fromFractional (dragRate * fromIntegral newPos)
