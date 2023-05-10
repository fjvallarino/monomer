{-|
Module      : Monomer.Widgets.Singles.Slider
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Slider widget, used for interacting with numeric values. It allows changing the
value using the keyboard arrows, dragging the mouse or using the wheel.

@
hslider numericLens 0 100
@

Similar in objective to "Monomer.Widgets.Singles.Dial", but more convenient in
some layouts.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Monomer.Widgets.Singles.Slider (
  -- * Configuration
  SliderValue,
  SliderCfg,
  -- * Constructors
  hslider,
  hslider_,
  vslider,
  vslider_,
  hsliderV,
  hsliderV_,
  vsliderV,
  vsliderV_,
  sliderD_
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (.~), (%~), (<>~))
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Typeable (Typeable, typeOf)
import GHC.Generics
import TextShow

import qualified Data.Sequence as Seq

import Monomer.Helper
import Monomer.Widgets.Single

import qualified Monomer.Lens as L

-- | Constraints for numeric types accepted by the slider widget.
type SliderValue a = (Eq a, Show a, Real a, FromFractional a, Typeable a)

{-|
Configuration options for slider:

- 'width': sets the size of the secondary axis of the Slider.
- 'radius': the radius of the corners of the Slider.
- 'wheelRate': The rate at which wheel movement affects the number.
- 'dragRate': The rate at which drag movement affects the number.
- 'thumbVisible': whether a thumb should be visible or not.
- 'thumbFactor': the size of the thumb relative to width.
- 'onFocus': event to raise when focus is received.
- 'onFocusReq': 'WidgetRequest' to generate when focus is received.
- 'onBlur': event to raise when focus is lost.
- 'onBlurReq': 'WidgetRequest' to generate when focus is lost.
- 'onChange': event to raise when the value changes.
- 'onChangeReq': 'WidgetRequest' to generate when the value changes.
-}
data SliderCfg s e a = SliderCfg {
  _slcRadius :: Maybe Double,
  _slcWidth :: Maybe Double,
  _slcWheelRate :: Maybe Rational,
  _slcDragRate :: Maybe Rational,
  _slcThumbVisible :: Maybe Bool,
  _slcThumbFactor :: Maybe Double,
  _slcOnFocusReq :: [Path -> WidgetRequest s e],
  _slcOnBlurReq :: [Path -> WidgetRequest s e],
  _slcOnChangeReq :: [a -> WidgetRequest s e]
}

instance Default (SliderCfg s e a) where
  def = SliderCfg {
    _slcRadius = Nothing,
    _slcWidth = Nothing,
    _slcWheelRate = Nothing,
    _slcDragRate = Nothing,
    _slcThumbVisible = Nothing,
    _slcThumbFactor = Nothing,
    _slcOnFocusReq = [],
    _slcOnBlurReq = [],
    _slcOnChangeReq = []
  }

instance Semigroup (SliderCfg s e a) where
  (<>) t1 t2 = SliderCfg {
    _slcRadius = _slcRadius t2 <|> _slcRadius t1,
    _slcWidth = _slcWidth t2 <|> _slcWidth t1,
    _slcWheelRate = _slcWheelRate t2 <|> _slcWheelRate t1,
    _slcDragRate = _slcDragRate t2 <|> _slcDragRate t1,
    _slcThumbVisible = _slcThumbVisible t2 <|> _slcThumbVisible t1,
    _slcThumbFactor = _slcThumbFactor t2 <|> _slcThumbFactor t1,
    _slcOnFocusReq = _slcOnFocusReq t1 <> _slcOnFocusReq t2,
    _slcOnBlurReq = _slcOnBlurReq t1 <> _slcOnBlurReq t2,
    _slcOnChangeReq = _slcOnChangeReq t1 <> _slcOnChangeReq t2
  }

instance Monoid (SliderCfg s e a) where
  mempty = def

instance CmbWidth (SliderCfg s e a) where
  width w = def {
    _slcWidth = Just w
}

instance CmbRadius (SliderCfg s e a) where
  radius w = def {
    _slcRadius = Just w
  }

instance CmbWheelRate (SliderCfg s e a) Rational where
  wheelRate rate = def {
    _slcWheelRate = Just rate
  }

instance CmbDragRate (SliderCfg s e a) Rational where
  dragRate rate = def {
    _slcDragRate = Just rate
  }

instance CmbThumbFactor (SliderCfg s e a) where
  thumbFactor w = def {
    _slcThumbFactor = Just w
  }

instance CmbThumbVisible (SliderCfg s e a) where
  thumbVisible_ w = def {
    _slcThumbVisible = Just w
  }

instance WidgetEvent e => CmbOnFocus (SliderCfg s e a) e Path where
  onFocus fn = def {
    _slcOnFocusReq = [RaiseEvent . fn]
  }

instance CmbOnFocusReq (SliderCfg s e a) s e Path where
  onFocusReq req = def {
    _slcOnFocusReq = [req]
  }

instance WidgetEvent e => CmbOnBlur (SliderCfg s e a) e Path where
  onBlur fn = def {
    _slcOnBlurReq = [RaiseEvent . fn]
  }

instance CmbOnBlurReq (SliderCfg s e a) s e Path where
  onBlurReq req = def {
    _slcOnBlurReq = [req]
  }

instance WidgetEvent e => CmbOnChange (SliderCfg s e a) a e where
  onChange fn = def {
    _slcOnChangeReq = [RaiseEvent . fn]
  }

instance CmbOnChangeReq (SliderCfg s e a) s e a where
  onChangeReq req = def {
    _slcOnChangeReq = [req]
  }

data SliderState = SliderState {
  _slsMaxPos :: Integer,
  _slsPos :: Integer
} deriving (Eq, Show, Generic)


{-|
Creates a horizontal slider using the given lens, providing minimum and maximum
values.
-}
hslider
  :: (SliderValue a, WidgetEvent e)
  => ALens' s a      -- ^ The lens into the model.
  -> a               -- ^ Minimum value.
  -> a               -- ^ Maximum value.
  -> WidgetNode s e  -- ^ The created slider.
hslider field minVal maxVal = hslider_ field minVal maxVal def

{-|
Creates a horizontal slider using the given lens, providing minimum and maximum
values. Accepts config.
-}
hslider_
  :: (SliderValue a, WidgetEvent e)
  => ALens' s a         -- ^ The lens into the model.
  -> a                  -- ^ Minimum value.
  -> a                  -- ^ Maximum value.
  -> [SliderCfg s e a]  -- ^ The config options.
  -> WidgetNode s e     -- ^ The created slider.
hslider_ field minVal maxVal cfg = sliderD_ True wlens minVal maxVal cfg where
  wlens = WidgetLens field

{-|
Creates a vertical slider using the given lens, providing minimum and maximum
values.
-}
vslider
  :: (SliderValue a, WidgetEvent e)
  => ALens' s a      -- ^ The lens into the model.
  -> a               -- ^ Minimum value.
  -> a               -- ^ Maximum value.
  -> WidgetNode s e  -- ^ The created slider.
vslider field minVal maxVal = vslider_ field minVal maxVal def

{-|
Creates a vertical slider using the given lens, providing minimum and maximum
values. Accepts config.
-}
vslider_
  :: (SliderValue a, WidgetEvent e)
  => ALens' s a         -- ^ The lens into the model.
  -> a                  -- ^ Minimum value.
  -> a                  -- ^ Maximum value.
  -> [SliderCfg s e a]  -- ^ The config options.
  -> WidgetNode s e     -- ^ The created slider.
vslider_ field minVal maxVal cfg = sliderD_ False wlens minVal maxVal cfg where
  wlens = WidgetLens field

{-|
Creates a horizontal slider using the given value and 'onChange' event handler,
providing minimum and maximum values.
-}
hsliderV
  :: (SliderValue a, WidgetEvent e)
  => a               -- ^ The current value.
  -> (a -> e)        -- ^ The event to raise on change.
  -> a               -- ^ Minimum value.
  -> a               -- ^ Maximum value.
  -> WidgetNode s e  -- ^ The created slider.
hsliderV value handler minVal maxVal = hsliderV_ value handler minVal maxVal def

{-|
Creates a horizontal slider using the given value and 'onChange' event handler,
providing minimum and maximum values. Accepts config.
-}
hsliderV_
  :: (SliderValue a, WidgetEvent e)
  => a                  -- ^ The current value.
  -> (a -> e)           -- ^ The event to raise on change.
  -> a                  -- ^ Minimum value.
  -> a                  -- ^ Maximum value.
  -> [SliderCfg s e a]  -- ^ The config options.
  -> WidgetNode s e     -- ^ The created slider.
hsliderV_ value handler minVal maxVal configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newNode = sliderD_ True widgetData minVal maxVal newConfigs

{-|
Creates a vertical slider using the given value and 'onChange' event handler,
providing minimum and maximum values.
-}
vsliderV
  :: (SliderValue a, WidgetEvent e)
  => a               -- ^ The current value.
  -> (a -> e)        -- ^ The event to raise on change.
  -> a               -- ^ Minimum value.
  -> a               -- ^ Maximum value.
  -> WidgetNode s e  -- ^ The created slider.
vsliderV value handler minVal maxVal = vsliderV_ value handler minVal maxVal def

{-|
Creates a vertical slider using the given value and 'onChange' event handler,
providing minimum and maximum values. Accepts config.
-}
vsliderV_
  :: (SliderValue a, WidgetEvent e)
  => a                  -- ^ The current value.
  -> (a -> e)           -- ^ The event to raise on change.
  -> a                  -- ^ Minimum value.
  -> a                  -- ^ Maximum value.
  -> [SliderCfg s e a]  -- ^ The config options.
  -> WidgetNode s e     -- ^ The created slider.
vsliderV_ value handler minVal maxVal configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newNode = sliderD_ False widgetData minVal maxVal newConfigs

{-|
Creates a slider providing direction, a 'WidgetData' instance, minimum and
maximum values and config.
-}
sliderD_
  :: (SliderValue a, WidgetEvent e)
  => Bool               -- ^ True if horizontal, False if vertical
  -> WidgetData s a     -- ^ The 'WidgetData' to retrieve the value from.
  -> a                  -- ^ Minimum value.
  -> a                  -- ^ Maximum value.
  -> [SliderCfg s e a]  -- ^ The config options.
  -> WidgetNode s e     -- ^ The created slider.
sliderD_ isHz widgetData minVal maxVal configs = sliderNode where
  config = mconcat configs
  state = SliderState 0 0
  wtype = WidgetType ("slider-" <> showt (typeOf minVal))
  widget = makeSlider isHz widgetData minVal maxVal config state
  sliderNode = defaultWidgetNode wtype widget
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
makeSlider !isHz !field !minVal !maxVal !config !state = widget where
  widget = createSingle state def {
    singleFocusOnBtnPressed = False,
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

  init wenv node = resultNode resNode where
    currVal = widgetDataGet (wenv ^. L.model) field
    newState = newStateFromValue currVal
    resNode = node
      & L.widget .~ makeSlider isHz field minVal maxVal config newState

  merge wenv newNode oldNode oldState = resultNode resNode where
    stateVal = valueFromPos (_slsPos oldState)
    modelVal = widgetDataGet (wenv ^. L.model) field
    newState
      | isNodePressed wenv newNode = oldState
      | stateVal == modelVal = oldState
      | otherwise = newStateFromValue modelVal
    resNode = newNode
      & L.widget .~ makeSlider isHz field minVal maxVal config newState

  handleEvent wenv node target evt = case evt of
    Focus prev -> handleFocusChange node prev (_slcOnFocusReq config)

    Blur next -> handleFocusChange node next (_slcOnBlurReq config)

    KeyAction mod code KeyPressed
      | ctrlPressed && isInc code -> handleNewPos (pos + warpSpeed)
      | ctrlPressed && isDec code -> handleNewPos (pos - warpSpeed)
      | shiftPressed && isInc code -> handleNewPos (pos + baseSpeed)
      | shiftPressed && isDec code -> handleNewPos (pos - baseSpeed)
      | isInc code -> handleNewPos (pos + fastSpeed)
      | isDec code -> handleNewPos (pos - fastSpeed)
      where
        ctrlPressed = isShortCutControl wenv mod
        (isDec, isInc)
          | isHz = (isKeyLeft, isKeyRight)
          | otherwise = (isKeyDown, isKeyUp)

        baseSpeed = max 1 $ round (fromIntegral maxPos / 1000)
        fastSpeed = max 1 $ round (fromIntegral maxPos / 100)
        warpSpeed = max 1 $ round (fromIntegral maxPos / 10)

        handleNewPos !newPos
          | validPos /= pos = resultFromPos validPos []
          | otherwise = Nothing
          where
            validPos = clamp 0 maxPos newPos

    Move point
      | isNodePressed wenv node -> resultFromPoint point []

    ButtonAction point btn BtnPressed clicks -> resultFromPoint point reqs where
      reqs
        | shiftPressed = []
        | otherwise = [SetFocus widgetId]

    ButtonAction point btn BtnReleased clicks  -> resultFromPoint point []

    WheelScroll _ (Point _ wy) wheelDirection -> resultFromPos newPos reqs where
      wheelCfg = fromMaybe (theme ^. L.sliderWheelRate) (_slcWheelRate config)
      wheelRate = fromRational wheelCfg
      tmpPos = pos + round (wy * wheelRate)
      newPos = clamp 0 maxPos tmpPos
      reqs = [IgnoreParentEvents]
    _ -> Nothing
    where
      theme = currentTheme wenv node
      style = currentStyle wenv node
      vp = getContentArea node style
      widgetId = node ^. L.info . L.widgetId
      shiftPressed = wenv ^. L.inputStatus . L.keyMod . L.leftShift
      SliderState maxPos pos = state

      resultFromPoint !point !reqs = resultFromPos newPos reqs where
        !newPos = posFromMouse isHz vp point

      resultFromPos !newPos !extraReqs = Just newResult where
        !newState = state {
          _slsPos = newPos
        }
        !newNode = node
          & L.widget .~ makeSlider isHz field minVal maxVal config newState
        !result = resultReqs newNode [RenderOnce]
        !newVal = valueFromPos newPos

        !reqs = widgetDataSet field newVal
          ++ fmap ($ newVal) (_slcOnChangeReq config)
          ++ extraReqs
        !newResult
          | pos /= newPos = result
              & L.requests <>~ Seq.fromList reqs
          | otherwise = result

  getSizeReq wenv node = req where
    theme = currentTheme wenv node
    maxPos = realToFrac (toRational (maxVal - minVal) / dragRate)
    width = fromMaybe (theme ^. L.sliderWidth) (_slcWidth config)
    req
      | isHz = (expandSize maxPos 1, fixedSize width)
      | otherwise = (fixedSize width, expandSize maxPos 1)

  render wenv node renderer = do
    drawRect renderer sliderBgArea (Just sndColor) sliderRadius

    drawInScissor renderer True sliderFgArea $
      drawRect renderer sliderBgArea (Just fgColor) sliderRadius

    when thbVisible $
      drawEllipse renderer thbArea (Just hlColor)
    where
      theme = currentTheme wenv node
      style = currentStyle wenv node

      fgColor = styleFgColor style
      hlColor = styleHlColor style
      sndColor = styleSndColor style

      radiusW = _slcRadius config <|> theme ^. L.sliderRadius
      sliderRadius = radius <$> radiusW
      SliderState maxPos pos = state
      posPct = fromIntegral pos / fromIntegral maxPos
      carea = getContentArea node style
      Rect cx cy cw ch = carea
      barW
        | isHz = ch
        | otherwise = cw
      -- Thumb
      thbVisible = fromMaybe False (_slcThumbVisible config)
      thbF = fromMaybe (theme ^. L.sliderThumbFactor) (_slcThumbFactor config)
      thbW = thbF * barW
      thbPos dim = (dim - thbW) * posPct
      thbDif = (thbW - barW) / 2
      thbArea
        | isHz = Rect (cx + thbPos cw) (cy - thbDif) thbW thbW
        | otherwise = Rect (cx - thbDif) (cy + ch - thbW - thbPos ch) thbW thbW
      -- Bar
      tw2 = thbW / 2
      sliderBgArea
        | not thbVisible = carea
        | isHz = fromMaybe def (subtractFromRect carea tw2 tw2 0 0)
        | otherwise = fromMaybe def (subtractFromRect carea 0 0 tw2 tw2)
      sliderFgArea
        | isHz = sliderBgArea & L.w %~ (*posPct)
        | otherwise = sliderBgArea
            & L.y %~ (+ (sliderBgArea ^. L.h * (1 - posPct)))
            & L.h %~ (*posPct)

  newStateFromValue currVal = newState where
    newMaxPos = round (toRational (maxVal - minVal) / dragRate)
    newPos = round (toRational (currVal - minVal) / dragRate)
    newState = SliderState {
      _slsMaxPos = newMaxPos,
      _slsPos = newPos
    }

  posFromMouse isHz vp point = newPos where
    SliderState maxPos _ = state
    dv
      | isHz = point ^. L.x - vp ^. L.x
      | otherwise = vp ^. L.y + vp ^. L.h - point ^. L.y
    tmpPos
      | isHz = round (dv * fromIntegral maxPos / vp ^. L.w)
      | otherwise = round (dv * fromIntegral maxPos / vp ^. L.h)
    newPos = clamp 0 maxPos tmpPos

  valueFromPos newPos = newVal where
    newVal = minVal + fromFractional (dragRate * fromIntegral newPos)
