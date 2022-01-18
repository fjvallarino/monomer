{-|
Module      : Monomer.Widgets.Singles.LabeledCheckbox
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Labeled checkbox, used for interacting with boolean values with an associated
clickable label.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Singles.LabeledCheckbox (
  -- * Configuration
  LabeledCheckboxCfg,
  -- * Constructors
  labeledCheckbox,
  labeledCheckbox_,
  labeledCheckboxV,
  labeledCheckboxV_,
  labeledCheckboxD_
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (.~))
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)

import qualified Data.Sequence as Seq

import Monomer.Widgets.Containers.Base.LabeledItem
import Monomer.Widgets.Single
import Monomer.Widgets.Singles.Checkbox
import Monomer.Widgets.Singles.Label

import qualified Monomer.Lens as L

{-|
Configuration options for labeledCheckbox:

- General

    - 'childSpacing': the spacing between the checkbox and the text.

- Text related

    - 'textLeft': places the label to the left of the checkbox.
    - 'textRight': places the label to the right of the checkbox.
    - 'textTop': places the label to the top of the checkbox.
    - 'textBottom': places the label to the bottom of the checkbox.
    - 'trimSpaces': whether to remove leading/trailing spaces in the caption.
    - 'ellipsis': if ellipsis should be used for overflown text.
    - 'multiline': if text may be split in multiple lines.
    - 'maxLines': maximum number of text lines to show.
    - 'resizeFactor': flexibility to have more or less spaced assigned.
    - 'resizeFactorW': flexibility for more or less horizontal spaced assigned.
    - 'resizeFactorH': flexibility for more or less vertical spaced assigned.

- Checkbox related

    - 'checkboxMark': the type of checkbox mark.
    - 'width': sets the max width/height of the checkbox.
    - 'onFocus': event to raise when focus is received.
    - 'onFocusReq': 'WidgetRequest' to generate when focus is received.
    - 'onBlur': event to raise when focus is lost.
    - 'onBlurReq': 'WidgetRequest' to generate when focus is lost.
    - 'onChange': event to raise when the value changes/is clicked.
    - 'onChangeReq': 'WidgetRequest' to generate when the value changes/is clicked.
-}
data LabeledCheckboxCfg s e = LabeledCheckboxCfg {
  _lchTextSide :: Maybe RectSide,
  _lchChildSpacing :: Maybe Double,
  _lchLabelCfg :: LabelCfg s e,
  _lchCheckboxCfg :: CheckboxCfg s e
}

instance Default (LabeledCheckboxCfg s e) where
  def = LabeledCheckboxCfg {
    _lchTextSide = Nothing,
    _lchChildSpacing = Nothing,
    _lchLabelCfg = def,
    _lchCheckboxCfg = def
  }

instance Semigroup (LabeledCheckboxCfg s e) where
  (<>) t1 t2 = LabeledCheckboxCfg {
    _lchTextSide = _lchTextSide t2 <|> _lchTextSide t1,
    _lchChildSpacing = _lchChildSpacing t2 <|> _lchChildSpacing t1,
    _lchLabelCfg = _lchLabelCfg t1 <> _lchLabelCfg t2,
    _lchCheckboxCfg = _lchCheckboxCfg t1 <> _lchCheckboxCfg t2
  }

instance Monoid (LabeledCheckboxCfg s e) where
  mempty = def

instance CmbChildSpacing (LabeledCheckboxCfg s e) where
  childSpacing_ spacing = def {
    _lchChildSpacing = Just spacing
  }

instance CmbTextLeft (LabeledCheckboxCfg s e) where
  textLeft_ False = def
  textLeft_ True = def {
    _lchTextSide = Just SideLeft
  }

instance CmbTextRight (LabeledCheckboxCfg s e) where
  textRight_ False = def
  textRight_ True = def {
    _lchTextSide = Just SideRight
  }

instance CmbTextTop (LabeledCheckboxCfg s e) where
  textTop_ False = def
  textTop_ True = def {
    _lchTextSide = Just SideTop
  }

instance CmbTextBottom (LabeledCheckboxCfg s e) where
  textBottom_ False = def
  textBottom_ True = def {
    _lchTextSide = Just SideBottom
  }

instance CmbTrimSpaces (LabeledCheckboxCfg s e) where
  trimSpaces_ trim = def {
    _lchLabelCfg = trimSpaces_ trim
  }

instance CmbEllipsis (LabeledCheckboxCfg s e) where
  ellipsis_ ellipsis = def {
    _lchLabelCfg = ellipsis_ ellipsis
  }

instance CmbMultiline (LabeledCheckboxCfg s e) where
  multiline_ multi = def {
    _lchLabelCfg = multiline_ multi
  }

instance CmbMaxLines (LabeledCheckboxCfg s e) where
  maxLines count = def {
    _lchLabelCfg = maxLines count
  }

instance CmbResizeFactor (LabeledCheckboxCfg s e) where
  resizeFactor s = def {
    _lchLabelCfg = resizeFactor s
  }

instance CmbResizeFactorDim (LabeledCheckboxCfg s e) where
  resizeFactorW w = def {
    _lchLabelCfg = resizeFactorW w
  }
  resizeFactorH h = def {
    _lchLabelCfg = resizeFactorH h
  }

instance CmbCheckboxMark (LabeledCheckboxCfg s e) where
  checkboxMark mark = def {
    _lchCheckboxCfg = checkboxMark mark
  }
  checkboxSquare = checkboxMark CheckboxSquare
  checkboxTimes = checkboxMark CheckboxTimes

instance CmbWidth (LabeledCheckboxCfg s e) where
  width w = def {
    _lchCheckboxCfg = width w
  }

instance WidgetEvent e => CmbOnFocus (LabeledCheckboxCfg s e) e Path where
  onFocus fn = def {
    _lchCheckboxCfg = onFocus fn
  }

instance CmbOnFocusReq (LabeledCheckboxCfg s e) s e Path where
  onFocusReq req = def {
    _lchCheckboxCfg = onFocusReq req
  }

instance WidgetEvent e => CmbOnBlur (LabeledCheckboxCfg s e) e Path where
  onBlur fn = def {
    _lchCheckboxCfg = onBlur fn
  }

instance CmbOnBlurReq (LabeledCheckboxCfg s e) s e Path where
  onBlurReq req = def {
    _lchCheckboxCfg = onBlurReq req
  }

instance WidgetEvent e => CmbOnChange (LabeledCheckboxCfg s e) Bool e where
  onChange fn = def {
    _lchCheckboxCfg = onChange fn
  }

instance CmbOnChangeReq (LabeledCheckboxCfg s e) s e Bool where
  onChangeReq req = def {
    _lchCheckboxCfg = onChangeReq req
  }

-- | Creates a labeled checkbox using the given lens.
labeledCheckbox :: WidgetEvent e => Text -> ALens' s Bool -> WidgetNode s e
labeledCheckbox caption field = labeledCheckbox_ caption field def

-- | Creates a labeled checkbox using the given lens. Accepts config.
labeledCheckbox_
  :: WidgetEvent e
  => Text
  -> ALens' s Bool
  -> [LabeledCheckboxCfg s e]
  -> WidgetNode s e
labeledCheckbox_ caption field config = newNode where
  newNode = labeledCheckboxD_ caption (WidgetLens field) config

-- | Creates a labeled checkbox using the given value and 'onChange' event
--   handler.
labeledCheckboxV
  :: WidgetEvent e
  => Text
  -> Bool
  -> (Bool -> e)
  -> WidgetNode s e
labeledCheckboxV caption value handler = newNode where
  newNode = labeledCheckboxV_ caption value handler def

{-|
Creates a labeled checkbox using the given value and 'onChange' event handler.
Accepts config.
-}
labeledCheckboxV_
  :: WidgetEvent e
  => Text
  -> Bool
  -> (Bool -> e)
  -> [LabeledCheckboxCfg s e]
  -> WidgetNode s e
labeledCheckboxV_ caption value handler config = newNode where
  newConfig = onChange handler : config
  newNode = labeledCheckboxD_ caption (WidgetValue value) newConfig

-- | Creates a labeled checkbox providing a 'WidgetData' instance and config.
labeledCheckboxD_
  :: WidgetEvent e
  => Text
  -> WidgetData s Bool
  -> [LabeledCheckboxCfg s e]
  -> WidgetNode s e
labeledCheckboxD_ caption widgetData configs = newNode where
  config = mconcat configs
  labelSide = fromMaybe SideLeft (_lchTextSide config)
  childSpacing = _lchChildSpacing config
  labelCfg = _lchLabelCfg config
  widget = checkboxD_ widgetData [_lchCheckboxCfg config]
  newNode = labeledItem "labeledCheckbox" labelSide childSpacing caption labelCfg widget
