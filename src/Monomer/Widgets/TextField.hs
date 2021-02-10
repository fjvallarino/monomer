{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.TextField (
  TextFieldCfg,
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
import Monomer.Widgets.InputField

import qualified Monomer.Lens as L

data TextFieldCfg s e = TextFieldCfg {
  _tfcValid :: Maybe (WidgetData s Bool),
  _tfcMaxLength :: Maybe Int,
  _tfcResizeOnChange :: Maybe Bool,
  _tfcSelectOnFocus :: Maybe Bool,
  _tfcOnFocus :: [e],
  _tfcOnFocusReq :: [WidgetRequest s],
  _tfcOnBlur :: [e],
  _tfcOnBlurReq :: [WidgetRequest s],
  _tfcOnChange :: [Text -> e],
  _tfcOnChangeReq :: [WidgetRequest s]
}

instance Default (TextFieldCfg s e) where
  def = TextFieldCfg {
    _tfcValid = Nothing,
    _tfcMaxLength = Nothing,
    _tfcResizeOnChange = Nothing,
    _tfcSelectOnFocus = Nothing,
    _tfcOnFocus = [],
    _tfcOnFocusReq = [],
    _tfcOnBlur = [],
    _tfcOnBlurReq = [],
    _tfcOnChange = [],
    _tfcOnChangeReq = []
  }

instance Semigroup (TextFieldCfg s e) where
  (<>) t1 t2 = TextFieldCfg {
    _tfcValid = _tfcValid t2 <|> _tfcValid t1,
    _tfcMaxLength = _tfcMaxLength t2 <|> _tfcMaxLength t1,
    _tfcResizeOnChange = _tfcResizeOnChange t2 <|> _tfcResizeOnChange t1,
    _tfcSelectOnFocus = _tfcSelectOnFocus t2 <|> _tfcSelectOnFocus t1,
    _tfcOnFocus = _tfcOnFocus t1 <> _tfcOnFocus t2,
    _tfcOnFocusReq = _tfcOnFocusReq t1 <> _tfcOnFocusReq t2,
    _tfcOnBlur = _tfcOnBlur t1 <> _tfcOnBlur t2,
    _tfcOnBlurReq = _tfcOnBlurReq t1 <> _tfcOnBlurReq t2,
    _tfcOnChange = _tfcOnChange t1 <> _tfcOnChange t2,
    _tfcOnChangeReq = _tfcOnChangeReq t1 <> _tfcOnChangeReq t2
  }

instance Monoid (TextFieldCfg s e) where
  mempty = def

instance CmbValidInput (TextFieldCfg s e) s where
  validInput field = def {
    _tfcValid = Just (WidgetLens field)
  }

instance CmbResizeOnChange (TextFieldCfg s e) where
  resizeOnChange resize = def {
    _tfcResizeOnChange = Just resize
  }

instance CmbSelectOnFocus (TextFieldCfg s e) where
  selectOnFocus sel = def {
    _tfcSelectOnFocus = Just sel
  }

instance CmbMaxLength (TextFieldCfg s e) where
  maxLength len = def {
    _tfcMaxLength = Just len
  }

instance CmbOnFocus (TextFieldCfg s e) e where
  onFocus fn = def {
    _tfcOnFocus = [fn]
  }

instance CmbOnFocusReq (TextFieldCfg s e) s where
  onFocusReq req = def {
    _tfcOnFocusReq = [req]
  }

instance CmbOnBlur (TextFieldCfg s e) e where
  onBlur fn = def {
    _tfcOnBlur = [fn]
  }

instance CmbOnBlurReq (TextFieldCfg s e) s where
  onBlurReq req = def {
    _tfcOnBlurReq = [req]
  }

instance CmbOnChange (TextFieldCfg s e) Text e where
  onChange fn = def {
    _tfcOnChange = [fn]
  }

instance CmbOnChangeReq (TextFieldCfg s e) s where
  onChangeReq req = def {
    _tfcOnChangeReq = [req]
  }

instance Default Text where
  def = T.empty

textField :: ALens' s Text -> WidgetNode s e
textField field = textField_ field def

textField_ :: ALens' s Text -> [TextFieldCfg s e] -> WidgetNode s e
textField_ field configs = textFieldD_ (WidgetLens field) configs

textFieldV :: Text -> (Text -> e) -> WidgetNode s e
textFieldV value handler = textFieldV_ value handler def

textFieldV_ :: Text -> (Text -> e) -> [TextFieldCfg s e] -> WidgetNode s e
textFieldV_ value handler configs = textFieldD_ widgetData newConfig where
  widgetData = WidgetValue value
  newConfig = onChange handler : configs

textFieldD_ :: WidgetData s Text -> [TextFieldCfg s e] -> WidgetNode s e
textFieldD_ widgetData configs = inputField where
  config = mconcat configs
  fromText = textToText (_tfcMaxLength config)
  inputConfig = InputFieldCfg {
    _ifcInitialValue = "",
    _ifcValue = widgetData,
    _ifcValid = _tfcValid config,
    _ifcFromText = fromText,
    _ifcToText = id,
    _ifcAcceptInput = acceptInput (_tfcMaxLength config),
    _ifcDefCursorEnd = True,
    _ifcDefWidth = 100,
    _ifcResizeOnChange = fromMaybe False (_tfcResizeOnChange config),
    _ifcSelectOnFocus = fromMaybe False (_tfcSelectOnFocus config),
    _ifcSelectDragOnlyFocused = False,
    _ifcStyle = Just L.inputTextStyle,
    _ifcDragHandler = Nothing,
    _ifcDragCursor = Nothing,
    _ifcOnFocus = _tfcOnFocus config,
    _ifcOnFocusReq = _tfcOnFocusReq config,
    _ifcOnBlur = _tfcOnBlur config,
    _ifcOnBlurReq = _tfcOnBlurReq config,
    _ifcOnChange = _tfcOnChange config,
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
