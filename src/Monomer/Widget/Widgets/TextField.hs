{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{- HLINT ignore "Use foldr" -}

module Monomer.Widget.Widgets.TextField where

import Control.Applicative ((<|>))
import Control.Lens (ALens')
import Data.Char
import Data.Default
import Data.Either
import Data.Maybe
import Data.Text (Text)
import Data.Text.Read (decimal, rational)
import Data.Typeable (Typeable)

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Formatting as F

import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.Widgets.InputField

data TextFieldCfg s e = TextFieldCfg {
  _tfcValue :: WidgetValue s Text,
  _tfcValid :: Maybe (WidgetValue s Bool),
  _tfcMaxLength :: Maybe Int,
  _tfcOnChange :: [Text -> e],
  _tfcOnChangeReq :: [WidgetRequest s]
}

textFieldCfg :: WidgetValue s Text -> TextFieldCfg s e
textFieldCfg value = TextFieldCfg {
  _tfcValue = value,
  _tfcValid = Nothing,
  _tfcMaxLength = Nothing,
  _tfcOnChange = [],
  _tfcOnChangeReq = []
}

instance Default Text where
  def = T.empty

textField :: ALens' s Text -> WidgetInstance s e
textField field = textField_ config where
  config = textFieldCfg (WidgetLens field)

textField_ :: TextFieldCfg s e -> WidgetInstance s e
textField_ config = inputField where
  inputConfig = inputFieldCfg (_tfcValue config) Just id
  inputField = inputField_ "textField" inputConfig {
    _ifcAcceptInput = acceptInput (_tfcMaxLength config),
    _ifcOnChange = _tfcOnChange config,
    _ifcOnChangeReq = _tfcOnChangeReq config
  }

acceptInput :: Maybe Int -> Text -> Bool
acceptInput Nothing _ = True
acceptInput (Just len) text = T.length text <= len
