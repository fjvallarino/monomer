{-|
Module      : Monomer.Widgets.Singles.TextField
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Input field for single line 'Text'. Allows setting the maximum number of
characters and a replacement character for password.

@
textField shortTextLens
@

With configuration options:

@
textField_ shortTextLens [maxLength 100, selectOnFocus_ False]
@

-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Singles.TextField (
  -- * Configuration
  TextFieldCfg,
  textFieldDisplayChar,
  -- * Constructors
  textField,
  textField_,
  textFieldV,
  textFieldV_,
  textFieldD_
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens')
import Data.Default
import Data.Maybe
import Data.Text (Text)

import qualified Data.Text as T

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Widgets.Singles.Base.InputField

import qualified Monomer.Lens as L

{-|
Configuration options for textField:

- 'caretWidth': the width of the caret.
- 'caretMs': the blink period of the caret.
- 'placeholder': the placeholder to use when main value is empty.
- 'validInput': field indicating if the current input is valid. Useful to show
  warnings in the UI, or disable buttons if needed.
- 'resizeOnChange': Whether input causes ResizeWidgets requests.
- 'selectOnFocus': Whether all input should be selected when focus is received.
- 'readOnly': Whether to prevent the user from changing the input text.
- 'maxLength': the maximum length of input text.
- 'textFieldDisplayChar': the character that will be displayed as replacement of
  the real text. Useful for password fields.
- 'onFocus': event to raise when focus is received.
- 'onFocusReq': 'WidgetRequest' to generate when focus is received.
- 'onBlur': event to raise when focus is lost.
- 'onBlurReq': 'WidgetRequest' to generate when focus is lost.
- 'onChange': event to raise when the value changes.
- 'onChangeReq': 'WidgetRequest' to generate when the value changes.
-}
data TextFieldCfg s e = TextFieldCfg {
  _tfcCaretWidth :: Maybe Double,
  _tfcCaretMs :: Maybe Millisecond,
  _tfcDisplayChar :: Maybe Char,
  _tfcPlaceholder :: Maybe Text,
  _tfcValid :: Maybe (WidgetData s Bool),
  _tfcValidV :: [Bool -> e],
  _tfcMaxLength :: Maybe Int,
  _tfcResizeOnChange :: Maybe Bool,
  _tfcSelectOnFocus :: Maybe Bool,
  _tfcReadOnly :: Maybe Bool,
  _tfcOnFocusReq :: [Path -> WidgetRequest s e],
  _tfcOnBlurReq :: [Path -> WidgetRequest s e],
  _tfcOnChangeReq :: [Text -> WidgetRequest s e]
}

instance Default (TextFieldCfg s e) where
  def = TextFieldCfg {
    _tfcCaretWidth = Nothing,
    _tfcCaretMs = Nothing,
    _tfcDisplayChar = Nothing,
    _tfcPlaceholder = Nothing,
    _tfcValid = Nothing,
    _tfcValidV = [],
    _tfcMaxLength = Nothing,
    _tfcResizeOnChange = Nothing,
    _tfcSelectOnFocus = Nothing,
    _tfcReadOnly = Nothing,
    _tfcOnFocusReq = [],
    _tfcOnBlurReq = [],
    _tfcOnChangeReq = []
  }

instance Semigroup (TextFieldCfg s e) where
  (<>) t1 t2 = TextFieldCfg {
    _tfcCaretWidth = _tfcCaretWidth t2 <|> _tfcCaretWidth t1,
    _tfcCaretMs = _tfcCaretMs t2 <|> _tfcCaretMs t1,
    _tfcDisplayChar = _tfcDisplayChar t2 <|> _tfcDisplayChar t1,
    _tfcPlaceholder = _tfcPlaceholder t2 <|> _tfcPlaceholder t1,
    _tfcValid = _tfcValid t2 <|> _tfcValid t1,
    _tfcValidV = _tfcValidV t1 <> _tfcValidV t2,
    _tfcMaxLength = _tfcMaxLength t2 <|> _tfcMaxLength t1,
    _tfcResizeOnChange = _tfcResizeOnChange t2 <|> _tfcResizeOnChange t1,
    _tfcSelectOnFocus = _tfcSelectOnFocus t2 <|> _tfcSelectOnFocus t1,
    _tfcReadOnly = _tfcReadOnly t2 <|> _tfcReadOnly t1,
    _tfcOnFocusReq = _tfcOnFocusReq t1 <> _tfcOnFocusReq t2,
    _tfcOnBlurReq = _tfcOnBlurReq t1 <> _tfcOnBlurReq t2,
    _tfcOnChangeReq = _tfcOnChangeReq t1 <> _tfcOnChangeReq t2
  }

instance Monoid (TextFieldCfg s e) where
  mempty = def

instance CmbCaretWidth (TextFieldCfg s e) Double where
  caretWidth w = def {
    _tfcCaretWidth = Just w
  }

instance CmbCaretMs (TextFieldCfg s e) Millisecond where
  caretMs ms = def {
    _tfcCaretMs = Just ms
  }

instance CmbPlaceholder (TextFieldCfg s e) Text where
  placeholder value = def {
    _tfcPlaceholder = Just value
  }

instance CmbValidInput (TextFieldCfg s e) s where
  validInput field = def {
    _tfcValid = Just (WidgetLens field)
  }

instance CmbValidInputV (TextFieldCfg s e) e where
  validInputV fn = def {
    _tfcValidV = [fn]
  }

instance CmbResizeOnChange (TextFieldCfg s e) where
  resizeOnChange_ resize = def {
    _tfcResizeOnChange = Just resize
  }

instance CmbSelectOnFocus (TextFieldCfg s e) where
  selectOnFocus_ sel = def {
    _tfcSelectOnFocus = Just sel
  }

instance CmbReadOnly (TextFieldCfg s e) where
  readOnly_ ro = def {
    _tfcReadOnly = Just ro
  }

instance CmbMaxLength (TextFieldCfg s e) where
  maxLength len = def {
    _tfcMaxLength = Just len
  }

instance WidgetEvent e => CmbOnFocus (TextFieldCfg s e) e Path where
  onFocus fn = def {
    _tfcOnFocusReq = [RaiseEvent . fn]
  }

instance CmbOnFocusReq (TextFieldCfg s e) s e Path where
  onFocusReq req = def {
    _tfcOnFocusReq = [req]
  }

instance WidgetEvent e => CmbOnBlur (TextFieldCfg s e) e Path where
  onBlur fn = def {
    _tfcOnBlurReq = [RaiseEvent . fn]
  }

instance CmbOnBlurReq (TextFieldCfg s e) s e Path where
  onBlurReq req = def {
    _tfcOnBlurReq = [req]
  }

instance WidgetEvent e => CmbOnChange (TextFieldCfg s e) Text e where
  onChange fn = def {
    _tfcOnChangeReq = [RaiseEvent . fn]
  }

instance CmbOnChangeReq (TextFieldCfg s e) s e Text where
  onChangeReq req = def {
    _tfcOnChangeReq = [req]
  }

-- | Replacement character to show instead of real text. Useful for passwords.
textFieldDisplayChar :: Char -> TextFieldCfg s e
textFieldDisplayChar char = def {
    _tfcDisplayChar = Just char
  }

-- | Creates a text field using the given lens.
textField
  :: WidgetEvent e
  => ALens' s Text   -- ^ The lens into the model.
  -> WidgetNode s e  -- ^ The created text field.
textField field = textField_ field def

-- | Creates a text field using the given lens. Accepts config.
textField_
  :: WidgetEvent e
  => ALens' s Text       -- ^ The lens into the model.
  -> [TextFieldCfg s e]  -- ^ The config options.
  -> WidgetNode s e      -- ^ The created text field.
textField_ field configs = textFieldD_ (WidgetLens field) configs

-- | Creates a text field using the given value and 'onChange' event handler.
textFieldV
  :: WidgetEvent e
  => Text            -- ^ The current value.
  -> (Text -> e)     -- ^ The event to raise on change.
  -> WidgetNode s e  -- ^ The created text field.
textFieldV value handler = textFieldV_ value handler def

-- | Creates a text field using the given value and 'onChange' event handler.
--   Accepts config.
textFieldV_
  :: WidgetEvent e
  => Text                -- ^ The current value.
  -> (Text -> e)         -- ^ The event to raise on change.
  -> [TextFieldCfg s e]  -- ^ The config options.
  -> WidgetNode s e      -- ^ The created text field.
textFieldV_ value handler configs = textFieldD_ widgetData newConfig where
  widgetData = WidgetValue value
  newConfig = onChange handler : configs

-- | Creates a text field providing a 'WidgetData' instance and config.
textFieldD_
  :: WidgetEvent e
  => WidgetData s Text   -- ^ The 'WidgetData' to retrieve the value from.
  -> [TextFieldCfg s e]  -- ^ The config options.
  -> WidgetNode s e      -- ^ The created text field.
textFieldD_ widgetData configs = inputField where
  config = mconcat configs
  fromText = textToText (_tfcMaxLength config)
  inputConfig = InputFieldCfg {
    _ifcPlaceholder = _tfcPlaceholder config,
    _ifcInitialValue = "",
    _ifcValue = widgetData,
    _ifcValid = _tfcValid config,
    _ifcValidV = _tfcValidV config,
    _ifcFromText = fromText,
    _ifcToText = id,
    _ifcAcceptInput = acceptInput (_tfcMaxLength config),
    _ifcIsValidInput = acceptInput (_tfcMaxLength config),
    _ifcDefCursorEnd = True,
    _ifcDefWidth = 100,
    _ifcCaretWidth = _tfcCaretWidth config,
    _ifcCaretMs = _tfcCaretMs config,
    _ifcDisplayChar = _tfcDisplayChar config,
    _ifcResizeOnChange = fromMaybe False (_tfcResizeOnChange config),
    _ifcSelectOnFocus = fromMaybe False (_tfcSelectOnFocus config),
    _ifcReadOnly = fromMaybe False (_tfcReadOnly config),
    _ifcStyle = Just L.textFieldStyle,
    _ifcWheelHandler = Nothing,
    _ifcDragHandler = Nothing,
    _ifcDragCursor = Nothing,
    _ifcOnFocusReq = _tfcOnFocusReq config,
    _ifcOnBlurReq = _tfcOnBlurReq config,
    _ifcOnChangeReq = _tfcOnChangeReq config
  }
  inputField = inputField_ "textField" inputConfig

textToText :: Maybe Int -> Text -> Maybe Text
textToText Nothing text = Just text
textToText (Just len) text
  | T.length text <= len = Just text
  | otherwise = Nothing

acceptInput :: Maybe Int -> Text -> Bool
acceptInput Nothing _ = True
acceptInput (Just len) text = T.length text <= len
