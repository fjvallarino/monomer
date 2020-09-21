{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Monomer.Widget.Widgets.WidgetCombinators

data TextFieldCfg s e = TextFieldCfg {
  _tfcValid :: Maybe (WidgetValue s Bool),
  _tfcMaxLength :: Maybe Int,
  _tfcOnChange :: [Text -> e],
  _tfcOnChangeReq :: [WidgetRequest s]
}

instance Default (TextFieldCfg s e) where
  def = TextFieldCfg {
    _tfcValid = Nothing,
    _tfcMaxLength = Nothing,
    _tfcOnChange = [],
    _tfcOnChangeReq = []
  }

instance Semigroup (TextFieldCfg s e) where
  (<>) t1 t2 = TextFieldCfg {
    _tfcValid = _tfcValid t2 <|> _tfcValid t1,
    _tfcMaxLength = _tfcMaxLength t2 <|> _tfcMaxLength t1,
    _tfcOnChange = _tfcOnChange t2 <> _tfcOnChange t1,
    _tfcOnChangeReq = _tfcOnChangeReq t2 <> _tfcOnChangeReq t1
  }

instance Monoid (TextFieldCfg s e) where
  mempty = def

instance ValidInput (TextFieldCfg s e) s where
  validInput field = def {
    _tfcValid = Just (WidgetLens field)
  }

instance MaxLengthCombinator (TextFieldCfg s e) where
  maxLength len = def {
    _tfcMaxLength = Just len
  }

instance Default Text where
  def = T.empty

textField :: ALens' s Text -> WidgetInstance s e
textField field = textField_ field def

textField_ :: ALens' s Text -> TextFieldCfg s e -> WidgetInstance s e
textField_ field config = inputField where
  inputConfig = inputFieldCfg (WidgetLens field) Just id
  inputField = inputField_ "textField" inputConfig {
    _ifcAcceptInput = acceptInput (_tfcMaxLength config),
    _ifcOnChange = _tfcOnChange config,
    _ifcOnChangeReq = _tfcOnChangeReq config
  }

acceptInput :: Maybe Int -> Text -> Bool
acceptInput Nothing _ = True
acceptInput (Just len) text = T.length text <= len
