{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widget.Widgets.IntegralField where

import Control.Applicative ((<|>))
import Control.Lens (ALens')
import Data.Char
import Data.Default
import Data.Either
import Data.Maybe
import Data.Text (Text)
import Data.Text.Read (decimal)
import Data.Typeable (Typeable)

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Formatting as F

import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.Widgets.InputField
import Monomer.Widget.Widgets.WidgetCombinators

type FormattableInt a = (Eq a, Default a, Typeable a, Integral a, Real a)

data IntegralFieldCfg s e a = IntegralFieldCfg {
  _nfcValid :: Maybe (WidgetValue s Bool),
  _nfcMinValue :: Maybe a,
  _nfcMaxValue :: Maybe a,
  _nfcSelectOnFocus :: Maybe Bool,
  _nfcOnChange :: [a -> e],
  _nfcOnChangeReq :: [WidgetRequest s]
}

instance Default (IntegralFieldCfg s e a) where
  def = IntegralFieldCfg {
    _nfcValid = Nothing,
    _nfcMinValue = Nothing,
    _nfcMaxValue = Nothing,
    _nfcSelectOnFocus = Nothing,
    _nfcOnChange = [],
    _nfcOnChangeReq = []
  }

instance Semigroup (IntegralFieldCfg s e a) where
  (<>) t1 t2 = IntegralFieldCfg {
    _nfcValid = _nfcValid t2 <|> _nfcValid t1,
    _nfcMinValue = _nfcMinValue t2 <|> _nfcMinValue t1,
    _nfcMaxValue = _nfcMaxValue t2 <|> _nfcMaxValue t1,
    _nfcSelectOnFocus = _nfcSelectOnFocus t2 <|> _nfcSelectOnFocus t1,
    _nfcOnChange = _nfcOnChange t1 <> _nfcOnChange t2,
    _nfcOnChangeReq = _nfcOnChangeReq t1 <> _nfcOnChangeReq t2
  }

instance Monoid (IntegralFieldCfg s e a) where
  mempty = def

instance ValidInput (IntegralFieldCfg s e a) s where
  validInput field = def {
    _nfcValid = Just (WidgetLens field)
  }

instance SelectOnFocus (IntegralFieldCfg s e a) where
  selectOnFocus sel = def {
    _nfcSelectOnFocus = Just sel
  }

instance FormattableInt a => MinValue (IntegralFieldCfg s e a) a where
  minValue len = def {
    _nfcMinValue = Just len
  }

instance FormattableInt a => MaxValue (IntegralFieldCfg s e a) a where
  maxValue len = def {
    _nfcMaxValue = Just len
  }

instance OnChange (IntegralFieldCfg s e a) a e where
  onChange fn = def {
    _nfcOnChange = [fn]
  }

instance OnChangeReq (IntegralFieldCfg s e a) s where
  onChangeReq req = def {
    _nfcOnChangeReq = [req]
  }

integralField :: FormattableInt a => ALens' s a -> WidgetInstance s e
integralField field = integralField_ field def

integralField_
  :: FormattableInt a
  => ALens' s a
  -> IntegralFieldCfg s e a
  -> WidgetInstance s e
integralField_ field config = newInst where
  inputConfig = inputFieldCfg (WidgetLens field) fromText toText
  minVal = _nfcMinValue config
  maxVal = _nfcMaxValue config
  fromText = integralFromText minVal maxVal
  toText = integralToText
  newInst = inputField_ "integralField" inputConfig {
    _ifcValid = _nfcValid config,
    _ifcAcceptInput = acceptIntegralInput,
    _ifcSelectOnFocus = fromMaybe True (_nfcSelectOnFocus config),
    _ifcOnChange = _nfcOnChange config,
    _ifcOnChangeReq = _nfcOnChangeReq config
  }

integralFromText :: FormattableInt a => Maybe a -> Maybe a -> Text -> Maybe a
integralFromText minVal maxVal t = case decimal t of
  Right (val, _)
    | numberInBounds minVal maxVal val -> Just val
  _ -> Nothing

integralToText :: FormattableInt a => a -> Text
integralToText val = F.sformat F.int val

acceptIntegralInput :: Text -> Bool
acceptIntegralInput text = isRight (A.parseOnly parser text) where
  number = A.takeWhile isDigit
  parser = number <* A.endOfInput
