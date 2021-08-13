{-|
Module      : Monomer.Widgets.Singles.LabeledRadio
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Radio widget, used for interacting with a fixed set of values with an associated
clickable label. Each instance of the radio will be associated with a single
value.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Singles.LabeledRadio (
  -- * Configuration
  LabeledRadioCfg,
  -- * Constructors
  labeledRadio,
  labeledRadio_,
  labeledRadioV,
  labeledRadioV_,
  labeledRadioD_
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (.~))
import Data.Default
import Data.Maybe
import Data.Text (Text)

import qualified Data.Sequence as Seq

import Monomer.Widgets.Containers.Base.LabeledItem
import Monomer.Widgets.Single
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.Radio

import qualified Monomer.Lens as L

{-|
Configuration options for labeledRadio:

- Text related

    - 'textLeft': places the label to the left of the radio.
    - 'textRight': places the label to the right of the radio.
    - 'textTop': places the label to the top of the radio.
    - 'textBottom': places the label to the bottom of the radio.
    - 'trimSpaces': whether to remove leading/trailing spaces in the caption.
    - 'ellipsis': if ellipsis should be used for overflown text.
    - 'multiline': if text may be split in multiple lines.
    - 'maxLines': maximum number of text lines to show.
    - 'resizeFactor': flexibility to have more or less spaced assigned.
    - 'resizeFactorW': flexibility for more or less horizontal spaced assigned.
    - 'resizeFactorH': flexibility for more or less vertical spaced assigned.

- Radio related

    - 'width': sets the max width/height of the radio.
    - 'onFocus': event to raise when focus is received.
    - 'onFocusReq': 'WidgetRequest' to generate when focus is received.
    - 'onBlur': event to raise when focus is lost.
    - 'onBlurReq': 'WidgetRequest' to generate when focus is lost.
    - 'onChange': event to raise when the value changes/is clicked.
    - 'onChangeReq': 'WidgetRequest' to generate when the value changes/is
      clicked.
-}
data LabeledRadioCfg s e a = LabeledRadioCfg {
  _lchTextSide :: Maybe RectSide,
  _lchLabelCfg :: LabelCfg s e,
  _lchRadioCfg :: RadioCfg s e a
}

instance Default (LabeledRadioCfg s e a) where
  def = LabeledRadioCfg {
    _lchTextSide = Nothing,
    _lchLabelCfg = def,
    _lchRadioCfg = def
  }

instance Semigroup (LabeledRadioCfg s e a) where
  (<>) t1 t2 = LabeledRadioCfg {
    _lchTextSide = _lchTextSide t2 <|> _lchTextSide t1,
    _lchLabelCfg = _lchLabelCfg t1 <> _lchLabelCfg t2,
    _lchRadioCfg = _lchRadioCfg t1 <> _lchRadioCfg t2
  }

instance Monoid (LabeledRadioCfg s e a) where
  mempty = def

instance CmbTextLeft (LabeledRadioCfg s e a) where
  textLeft_ False = def
  textLeft_ True = def {
    _lchTextSide = Just SideLeft
  }

instance CmbTextRight (LabeledRadioCfg s e a) where
  textRight_ False = def
  textRight_ True = def {
    _lchTextSide = Just SideRight
  }

instance CmbTextTop (LabeledRadioCfg s e a) where
  textTop_ False = def
  textTop_ True = def {
    _lchTextSide = Just SideTop
  }

instance CmbTextBottom (LabeledRadioCfg s e a) where
  textBottom_ False = def
  textBottom_ True = def {
    _lchTextSide = Just SideBottom
  }

instance CmbTrimSpaces (LabeledRadioCfg s e a) where
  trimSpaces_ trim = def {
    _lchLabelCfg = trimSpaces_ trim
  }

instance CmbEllipsis (LabeledRadioCfg s e a) where
  ellipsis_ ellipsis = def {
    _lchLabelCfg = ellipsis_ ellipsis
  }

instance CmbMultiline (LabeledRadioCfg s e a) where
  multiline_ multi = def {
    _lchLabelCfg = multiline_ multi
  }

instance CmbMaxLines (LabeledRadioCfg s e a) where
  maxLines count = def {
    _lchLabelCfg = maxLines count
  }

instance CmbResizeFactor (LabeledRadioCfg s e a) where
  resizeFactor s = def {
    _lchLabelCfg = resizeFactor s
  }

instance CmbResizeFactorDim (LabeledRadioCfg s e a) where
  resizeFactorW w = def {
    _lchLabelCfg = resizeFactorW w
  }
  resizeFactorH h = def {
    _lchLabelCfg = resizeFactorH h
  }

instance CmbWidth (LabeledRadioCfg s e a) where
  width w = def {
    _lchRadioCfg = width w
  }

instance WidgetEvent e => CmbOnFocus (LabeledRadioCfg s e a) e Path where
  onFocus fn = def {
    _lchRadioCfg = onFocus fn
  }

instance CmbOnFocusReq (LabeledRadioCfg s e a) s e Path where
  onFocusReq req = def {
    _lchRadioCfg = onFocusReq req
  }

instance WidgetEvent e => CmbOnBlur (LabeledRadioCfg s e a) e Path where
  onBlur fn = def {
    _lchRadioCfg = onBlur fn
  }

instance CmbOnBlurReq (LabeledRadioCfg s e a) s e Path where
  onBlurReq req = def {
    _lchRadioCfg = onBlurReq req
  }

instance WidgetEvent e => CmbOnChange (LabeledRadioCfg s e a) a e where
  onChange fn = def {
    _lchRadioCfg = onChange fn
  }

instance CmbOnChangeReq (LabeledRadioCfg s e a) s e a where
  onChangeReq req = def {
    _lchRadioCfg = onChangeReq req
  }

-- | Creates a labeled radio using the given lens.
labeledRadio
  :: (Eq a, WidgetEvent e)
  => Text
  -> a
  -> ALens' s a
  -> WidgetNode s e
labeledRadio caption option field = labeledRadio_ caption option field def

-- | Creates a labeled radio using the given lens. Accepts config.
labeledRadio_
  :: (Eq a, WidgetEvent e)
  => Text
  -> a
  -> ALens' s a
  -> [LabeledRadioCfg s e a]
  -> WidgetNode s e
labeledRadio_ caption option field config = newNode where
  newNode = labeledRadioD_ caption option (WidgetLens field) config

-- | Creates a labeled radio using the given value and 'onChange' event handler.
labeledRadioV
  :: (Eq a, WidgetEvent e)
  => Text
  -> a
  -> a
  -> (a -> e)
  -> WidgetNode s e
labeledRadioV caption option value handler = newNode where
  newNode = labeledRadioV_ caption option value handler def

{-|
Creates a labeled radio using the given value and 'onChange' event handler.
Accepts config.
-}
labeledRadioV_
  :: (Eq a, WidgetEvent e)
  => Text
  -> a
  -> a
  -> (a -> e)
  -> [LabeledRadioCfg s e a]
  -> WidgetNode s e
labeledRadioV_ caption option value handler config = newNode where
  newConfig = onChange handler : config
  newNode = labeledRadioD_ caption option (WidgetValue value) newConfig

-- | Creates a labeled radio providing a 'WidgetData' instance and config.
labeledRadioD_
  :: (Eq a, WidgetEvent e)
  => Text
  -> a
  -> WidgetData s a
  -> [LabeledRadioCfg s e a]
  -> WidgetNode s e
labeledRadioD_ caption option widgetData configs = newNode where
  config = mconcat configs
  labelSide = fromMaybe SideLeft (_lchTextSide config)
  labelCfg = _lchLabelCfg config
  widget = radioD_ option widgetData [_lchRadioCfg config]
  newNode = labeledItem "labeledRadio" labelSide caption labelCfg widget
