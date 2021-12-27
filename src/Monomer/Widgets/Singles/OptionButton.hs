{-|
Module      : Monomer.Widgets.Singles.OptionButton
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Option button widget, used for choosing one value from a fixed set. Each
instance of optionButton will be associated with a single value.

Its behavior is equivalent to 'Monomer.Widgets.Singles.Radio' and
'Monomer.Widgets.Singles.LabeledRadio', with a different visual representation.

This widget, and the associated 'ToggleButton', uses two separate styles for the
On and Off states which can be modified individually for the theme. If you use
any of the the standard style functions (styleBasic, styleHover, etc) in an
optionButton/toggleButton these changes will apply to both On and Off states,
except for the color related styles. The reason for this is that, in general,
you will want to use the same font and padding for both states, but colors will
usually differ. For changing the colors of the Off state you can use
'optionButtonOffStyle', that receives a 'Style' instance. The values set here
are higher priority than any inherited style from the theme or node text style.

'Style' instances can be created this way:

@
newStyle :: Style = def
  `styleBasic` [textSize 20]
  `styleHover` [textColor white]
@
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Monomer.Widgets.Singles.OptionButton (
  -- * Configuration
  OptionButtonValue,
  OptionButtonCfg,
  optionButtonOffStyle,
  -- * Constructors
  optionButton,
  optionButton_,
  optionButtonV,
  optionButtonV_,
  optionButtonD_,
  -- * Internal
  makeOptionButton
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', Lens', (&), (^.), (^?), (.~), (?~), _Just)
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Data.Typeable (Typeable, typeOf)
import TextShow

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container
import Monomer.Widgets.Singles.Label

import qualified Monomer.Lens as L

-- | Constraints for numeric types accepted by the optionButton widget.
type OptionButtonValue a = (Eq a, Typeable a)

{-|
Configuration options for optionButton:

- 'ignoreTheme': whether to load default style from theme or start empty.
- 'optionButtonOffStyle': style to use when the option is not active.
- 'trimSpaces': whether to remove leading/trailing spaces in the caption.
- 'ellipsis': if ellipsis should be used for overflown text.
- 'multiline': if text may be split in multiple lines.
- 'maxLines': maximum number of text lines to show.
- 'resizeFactor': flexibility to have more or less spaced assigned.
- 'resizeFactorW': flexibility to have more or less horizontal spaced assigned.
- 'resizeFactorH': flexibility to have more or less vertical spaced assigned.
- 'onFocus': event to raise when focus is received.
- 'onFocusReq': 'WidgetRequest' to generate when focus is received.
- 'onBlur': event to raise when focus is lost.
- 'onBlurReq': 'WidgetRequest' to generate when focus is lost.
- 'onChange': event to raise when the value changes/is clicked.
- 'onChangeReq': 'WidgetRequest' to generate when the value changes/is clicked.
-}
data OptionButtonCfg s e a = OptionButtonCfg {
  _obcIgnoreTheme :: Maybe Bool,
  _obcOffStyle :: Maybe Style,
  _obcLabelCfg :: LabelCfg s e,
  _obcOnFocusReq :: [Path -> WidgetRequest s e],
  _obcOnBlurReq :: [Path -> WidgetRequest s e],
  _obcOnChangeReq :: [a -> WidgetRequest s e]
}

instance Default (OptionButtonCfg s e a) where
  def = OptionButtonCfg {
    _obcIgnoreTheme = Nothing,
    _obcOffStyle = Nothing,
    _obcLabelCfg = def,
    _obcOnFocusReq = [],
    _obcOnBlurReq = [],
    _obcOnChangeReq = []
  }

instance Semigroup (OptionButtonCfg s e a) where
  (<>) t1 t2 = OptionButtonCfg {
    _obcIgnoreTheme = _obcIgnoreTheme t2 <|> _obcIgnoreTheme t1,
    _obcOffStyle = _obcOffStyle t1 <> _obcOffStyle t2,
    _obcLabelCfg = _obcLabelCfg t1 <> _obcLabelCfg t2,
    _obcOnFocusReq = _obcOnFocusReq t1 <> _obcOnFocusReq t2,
    _obcOnBlurReq = _obcOnBlurReq t1 <> _obcOnBlurReq t2,
    _obcOnChangeReq = _obcOnChangeReq t1 <> _obcOnChangeReq t2
  }

instance Monoid (OptionButtonCfg s e a) where
  mempty = def

instance CmbIgnoreTheme (OptionButtonCfg s e a) where
  ignoreTheme_ ignore = def {
    _obcIgnoreTheme = Just ignore
  }

instance CmbTrimSpaces (OptionButtonCfg s e a) where
  trimSpaces_ trim = def {
    _obcLabelCfg = trimSpaces_ trim
  }

instance CmbEllipsis (OptionButtonCfg s e a) where
  ellipsis_ ellipsis = def {
    _obcLabelCfg = ellipsis_ ellipsis
  }

instance CmbMultiline (OptionButtonCfg s e a) where
  multiline_ multi = def {
    _obcLabelCfg = multiline_ multi
  }

instance CmbMaxLines (OptionButtonCfg s e a) where
  maxLines count = def {
    _obcLabelCfg = maxLines count
  }

instance CmbResizeFactor (OptionButtonCfg s e a) where
  resizeFactor s = def {
    _obcLabelCfg = resizeFactor s
  }

instance CmbResizeFactorDim (OptionButtonCfg s e a) where
  resizeFactorW w = def {
    _obcLabelCfg = resizeFactorW w
  }
  resizeFactorH h = def {
    _obcLabelCfg = resizeFactorH h
  }

instance WidgetEvent e => CmbOnFocus (OptionButtonCfg s e a) e Path where
  onFocus fn = def {
    _obcOnFocusReq = [RaiseEvent . fn]
  }

instance CmbOnFocusReq (OptionButtonCfg s e a) s e Path where
  onFocusReq req = def {
    _obcOnFocusReq = [req]
  }

instance WidgetEvent e => CmbOnBlur (OptionButtonCfg s e a) e Path where
  onBlur fn = def {
    _obcOnBlurReq = [RaiseEvent . fn]
  }

instance CmbOnBlurReq (OptionButtonCfg s e a) s e Path where
  onBlurReq req = def {
    _obcOnBlurReq = [req]
  }

instance WidgetEvent e => CmbOnChange (OptionButtonCfg s e a) a e where
  onChange fn = def {
    _obcOnChangeReq = [RaiseEvent . fn]
  }

instance CmbOnChangeReq (OptionButtonCfg s e a) s e a where
  onChangeReq req = def {
    _obcOnChangeReq = [req]
  }

-- | Sets the style for the Off state of the option button.
optionButtonOffStyle :: Style -> OptionButtonCfg s e a
optionButtonOffStyle style = def {
  _obcOffStyle = Just style
}

-- | Creates an optionButton using the given lens.
optionButton
  :: OptionButtonValue a
  => Text
  -> a
  -> ALens' s a
  -> WidgetNode s e
optionButton caption option field = optionButton_ caption option field def

-- | Creates an optionButton using the given lens. Accepts config.
optionButton_
  :: OptionButtonValue a
  => Text
  -> a
  -> ALens' s a
  -> [OptionButtonCfg s e a]
  -> WidgetNode s e
optionButton_ caption option field cfgs = newNode where
  newNode = optionButtonD_ caption option (WidgetLens field) cfgs

-- | Creates an optionButton using the given value and 'onChange' event handler.
optionButtonV
  :: (OptionButtonValue a, WidgetEvent e)
  => Text
  -> a
  -> a
  -> (a -> e)
  -> WidgetNode s e
optionButtonV caption option value handler = newNode where
  newNode = optionButtonV_ caption option value handler def

-- | Creates an optionButton using the given value and 'onChange' event handler.
--   Accepts config.
optionButtonV_
  :: (OptionButtonValue a, WidgetEvent e)
  => Text
  -> a
  -> a
  -> (a -> e)
  -> [OptionButtonCfg s e a]
  -> WidgetNode s e
optionButtonV_ caption option value handler configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newNode = optionButtonD_ caption option widgetData newConfigs

-- | Creates an optionButton providing a 'WidgetData' instance and config.
optionButtonD_
  :: OptionButtonValue a
  => Text
  -> a
  -> WidgetData s a
  -> [OptionButtonCfg s e a]
  -> WidgetNode s e
optionButtonD_ caption option widgetData configs = optionButtonNode where
  config = mconcat configs
  makeWithStyle = makeOptionButton L.optionBtnOnStyle L.optionBtnOffStyle
  wtype = WidgetType ("optionButton-" <> showt (typeOf option))
  widget = makeWithStyle widgetData caption (== option) (const option) config
  optionButtonNode = defaultWidgetNode wtype widget
    & L.info . L.focusable .~ True

makeOptionButton
  :: OptionButtonValue a
  => Lens' ThemeState StyleState
  -> Lens' ThemeState StyleState
  -> WidgetData s a
  -> Text
  -> (a -> Bool)
  -> (a -> a)
  -> OptionButtonCfg s e a
  -> Widget s e
makeOptionButton styleOn styleOff !field !caption !isSelVal !getNextVal !config = widget where
  widget = createContainer () def {
    containerAddStyleReq = False,
    containerDrawDecorations = False,
    containerUseScissor = True,
    containerInit = init,
    containerMerge = merge,
    containerHandleEvent = handleEvent,
    containerResize = resize
  }

  createChildNode wenv node = newNode where
    currValue = widgetDataGet (wenv ^. L.model) field
    isSelected = isSelVal currValue
    useBaseTheme = _obcIgnoreTheme config /= Just True

    baseOffStyle
      | useBaseTheme = Just (collectTheme wenv styleOff)
      | otherwise = Nothing

    baseOnStyle
      | useBaseTheme = Just (collectTheme wenv styleOn)
      | otherwise = Nothing

    nodeStyle = node ^. L.info . L.style
    colorlessStyle = mapStyleStates resetColor nodeStyle
    customOffStyle = mergeBasicStyle <$> _obcOffStyle config

    labelNodeStyle
      | isSelected = fromJust (baseOnStyle <> Just nodeStyle)
      | otherwise = fromJust (baseOffStyle <> Just colorlessStyle <> customOffStyle)

    labelCfg = _obcLabelCfg config
    labelCurrStyle = labelCurrentStyle childOfFocusedStyle
    labelNode = label_ caption [ignoreTheme, labelCfg, labelCurrStyle]
      & L.info . L.style .~ labelNodeStyle

    !newNode = node
      & L.children .~ Seq.singleton labelNode

  init wenv node = result where
    result = resultNode (createChildNode wenv node)

  merge wenv node oldNode oldState = result where
    result = resultNode (createChildNode wenv node)

  handleEvent wenv node target evt = case evt of
    Focus prev -> handleFocusChange node prev (_obcOnFocusReq config)
    Blur next -> handleFocusChange node next (_obcOnBlurReq config)

    KeyAction mode code status
      | isSelectKey code && status == KeyPressed -> Just result
      where
        isSelectKey code = isKeyReturn code || isKeySpace code

    Click p _ _
      | isPointInNodeVp node p -> Just result

    ButtonAction p btn BtnPressed 1 -- Set focus on click
      | mainBtn btn && pointInVp p && not focused -> Just resultFocus

    _ -> Nothing
    where
      mainBtn btn = btn == wenv ^. L.mainButton
      focused = isNodeFocused wenv node
      pointInVp p = isPointInNodeVp node p

      currValue = widgetDataGet (wenv ^. L.model) field
      nextValue = getNextVal currValue
      setValueReq = widgetDataSet field nextValue
      reqs = setValueReq ++ fmap ($ nextValue) (_obcOnChangeReq config)
      result = resultReqs node reqs
      resultFocus = resultReqs node [SetFocus (node ^. L.info . L.widgetId)]

  resize wenv node viewport children = resized where
    assignedAreas = Seq.fromList [viewport]
    resized = (resultNode node, assignedAreas)

resetColor :: StyleState -> StyleState
resetColor st = st
  & L.bgColor .~ Nothing
  & L.fgColor .~ Nothing
  & L.text . _Just . L.fontColor .~ Nothing
