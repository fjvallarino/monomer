{-|
Module      : Monomer.Widgets.Singles.Radio
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Radio widget, used for interacting with a fixed set of values. Each instance of
the radio is associated with a single value. It does not include text, which can
be added with a label in the desired position (usually with
[hstack/vstack]('Monomer.Widgets.Containers.Stack')). Alternatively,
'Monomer.Widgets.Singles.LabeledRadio' provides this functionality out of the
box.

@
radio Option1 optionLens
@

'Monomer.Widgets.Singles.OptionButton' provides similar functionality but with
the look of a regular button.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Monomer.Widgets.Singles.Radio (
  -- * Configuration
  RadioValue,
  RadioCfg,
  -- * Constructors
  radio,
  radio_,
  radioV,
  radioV_,
  radioD_
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (.~))
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Data.Typeable (Typeable, typeOf)
import TextShow

import Monomer.Widgets.Single

import qualified Monomer.Lens as L

-- | Constraints for numeric types accepted by the radio widget.
type RadioValue a = (Eq a, Typeable a)

{-|
Configuration options for radio:

- 'width': sets the max width/height of the radio.
- 'onFocus': event to raise when focus is received.
- 'onFocusReq': 'WidgetRequest' to generate when focus is received.
- 'onBlur': event to raise when focus is lost.
- 'onBlurReq': 'WidgetRequest' to generate when focus is lost.
- 'onChange': event to raise when the value changes/is clicked.
- 'onChangeReq': 'WidgetRequest' to generate when the value changes/is clicked.
-}
data RadioCfg s e a = RadioCfg {
  _rdcWidth :: Maybe Double,
  _rdcOnFocusReq :: [Path -> WidgetRequest s e],
  _rdcOnBlurReq :: [Path -> WidgetRequest s e],
  _rdcOnChangeReq :: [a -> WidgetRequest s e]
}

instance Default (RadioCfg s e a) where
  def = RadioCfg {
    _rdcWidth = Nothing,
    _rdcOnFocusReq = [],
    _rdcOnBlurReq = [],
    _rdcOnChangeReq = []
  }

instance Semigroup (RadioCfg s e a) where
  (<>) t1 t2 = RadioCfg {
    _rdcWidth = _rdcWidth t2 <|> _rdcWidth t1,
    _rdcOnFocusReq = _rdcOnFocusReq t1 <> _rdcOnFocusReq t2,
    _rdcOnBlurReq = _rdcOnBlurReq t1 <> _rdcOnBlurReq t2,
    _rdcOnChangeReq = _rdcOnChangeReq t1 <> _rdcOnChangeReq t2
  }

instance Monoid (RadioCfg s e a) where
  mempty = def

instance CmbWidth (RadioCfg s e a) where
  width w = def {
    _rdcWidth = Just w
  }

instance WidgetEvent e => CmbOnFocus (RadioCfg s e a) e Path where
  onFocus fn = def {
    _rdcOnFocusReq = [RaiseEvent . fn]
  }

instance CmbOnFocusReq (RadioCfg s e a) s e Path where
  onFocusReq req = def {
    _rdcOnFocusReq = [req]
  }

instance WidgetEvent e => CmbOnBlur (RadioCfg s e a) e Path where
  onBlur fn = def {
    _rdcOnBlurReq = [RaiseEvent . fn]
  }

instance CmbOnBlurReq (RadioCfg s e a) s e Path where
  onBlurReq req = def {
    _rdcOnBlurReq = [req]
  }

instance WidgetEvent e => CmbOnChange (RadioCfg s e a) a e where
  onChange fn = def {
    _rdcOnChangeReq = [RaiseEvent . fn]
  }

instance CmbOnChangeReq (RadioCfg s e a) s e a where
  onChangeReq req = def {
    _rdcOnChangeReq = [req]
  }

-- | Creates a radio using the given lens.
radio :: (RadioValue a, WidgetEvent e) => a -> ALens' s a -> WidgetNode s e
radio option field = radio_ option field def

-- | Creates a radio using the given lens. Accepts config.
radio_
  :: (RadioValue a, WidgetEvent e)
  => a
  -> ALens' s a
  -> [RadioCfg s e a]
  -> WidgetNode s e
radio_ option field configs = radioD_ option (WidgetLens field) configs

-- | Creates a radio using the given value and 'onChange' event handler.
radioV
  :: (RadioValue a, WidgetEvent e)
  => a
  -> a
  -> (a -> e)
  -> WidgetNode s e
radioV option value handler = radioV_ option value handler def

-- | Creates a radio using the given value and 'onChange' event handler.
--   Accepts config.
radioV_
  :: (RadioValue a, WidgetEvent e)
  => a
  -> a
  -> (a -> e)
  -> [RadioCfg s e a]
  -> WidgetNode s e
radioV_ option value handler configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newNode = radioD_ option widgetData newConfigs

-- | Creates a radio providing a 'WidgetData' instance and config.
radioD_
  :: (RadioValue a, WidgetEvent e)
  => a
  -> WidgetData s a
  -> [RadioCfg s e a]
  -> WidgetNode s e
radioD_ option widgetData configs = radioNode where
  config = mconcat configs
  wtype = WidgetType ("radio-" <> showt (typeOf option))
  widget = makeRadio widgetData option config
  radioNode = defaultWidgetNode wtype widget
    & L.info . L.focusable .~ True

makeRadio
  :: (RadioValue a, WidgetEvent e)
  => WidgetData s a
  -> a
  -> RadioCfg s e a
  -> Widget s e
makeRadio !field !option !config = widget where
  widget = createSingle () def {
    singleGetBaseStyle = getBaseStyle,
    singleGetCurrentStyle = getCurrentStyle,
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  getBaseStyle wenv node = Just style where
    style = collectTheme wenv L.radioStyle

  getCurrentStyle wenv node = style where
    radioArea = getRadioArea wenv node config
    style = currentStyle_ (currentStyleConfig radioArea) wenv node

  handleEvent wenv node target evt = case evt of
    Focus prev -> handleFocusChange node prev (_rdcOnFocusReq config)

    Blur next -> handleFocusChange node next (_rdcOnBlurReq config)

    Click p _ _
      | pointInEllipse p rdArea -> Just $ resultReqs node reqs

    KeyAction mod code KeyPressed
      | isSelectKey code -> Just $ resultReqs node reqs
    _ -> Nothing
    where
      rdArea = getRadioArea wenv node config
      path = node ^. L.info . L.path
      isSelectKey code = isKeyReturn code || isKeySpace code
      setValueReq = widgetDataSet field option
      reqs = setValueReq ++ fmap ($ option) (_rdcOnChangeReq config)

  getSizeReq wenv node = req where
    theme = currentTheme wenv node
    width = fromMaybe (theme ^. L.radioWidth) (_rdcWidth config)
    req = (fixedSize width, fixedSize width)

  render wenv node renderer = do
    renderRadio renderer radioBW radioArea fgColor

    when (value == option) $
      renderMark renderer radioBW radioArea hlColor
    where
      model = _weModel wenv
      value = widgetDataGet model field
      radioArea = getRadioArea wenv node config
      radioBW = max 1 (_rW radioArea * 0.1)

      style_ = currentStyle_ (currentStyleConfig radioArea) wenv node
      fgColor = styleFgColor style_
      hlColor = styleHlColor style_

getRadioArea :: WidgetEnv s e -> WidgetNode s e -> RadioCfg s e a -> Rect
getRadioArea wenv node config = radioArea where
  theme = currentTheme wenv node
  style = currentStyle wenv node
  rarea = getContentArea node style

  radioW = fromMaybe (theme ^. L.radioWidth) (_rdcWidth config)
  radioL = _rX rarea + (_rW rarea - radioW) / 2
  radioT = _rY rarea + (_rH rarea - radioW) / 2
  radioArea = Rect radioL radioT radioW radioW

renderRadio :: Renderer -> Double -> Rect -> Color -> IO ()
renderRadio renderer radioBW rect color = action where
  action = drawEllipseBorder renderer rect (Just color) radioBW

renderMark :: Renderer -> Double -> Rect -> Color -> IO ()
renderMark renderer radioBW rect color = action where
  w = radioBW * 2
  newRect = fromMaybe def (subtractFromRect rect w w w w)
  action = drawEllipse renderer newRect (Just color)

currentStyleConfig :: Rect -> CurrentStyleCfg s e
currentStyleConfig radioArea = def &
  L.isHovered .~ isNodeHoveredEllipse_ radioArea
