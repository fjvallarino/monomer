{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- HLINT ignore "Use foldr" -}

module Monomer.Widgets.IntegralField (
  IntegralFieldCfg,
  integralField,
  integralField_,
  integralFieldV,
  integralFieldV_,
  integralFieldD_
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens')
import Data.Char
import Data.Default
import Data.Either
import Data.Maybe
import Data.Text (Text)
import Data.Text.Read (decimal, signed)
import Data.Typeable (Typeable)

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Formatting as F

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Widgets.InputField
import Monomer.Widgets.Util

import qualified Monomer.Lens as L

type FormattableInt a = (Eq a, Default a, Typeable a, Integral a, Real a)

data IntegralFieldCfg s e a = IntegralFieldCfg {
  _nfcValid :: Maybe (WidgetData s Bool),
  _nfcMinValue :: Maybe a,
  _nfcMaxValue :: Maybe a,
  _nfcSelectOnFocus :: Maybe Bool,
  _nfcOnFocus :: [e],
  _nfcOnFocusReq :: [WidgetRequest s],
  _nfcOnBlur :: [e],
  _nfcOnBlurReq :: [WidgetRequest s],
  _nfcOnChange :: [a -> e],
  _nfcOnChangeReq :: [WidgetRequest s]
}

instance Default (IntegralFieldCfg s e a) where
  def = IntegralFieldCfg {
    _nfcValid = Nothing,
    _nfcMinValue = Nothing,
    _nfcMaxValue = Nothing,
    _nfcSelectOnFocus = Nothing,
    _nfcOnFocus = [],
    _nfcOnFocusReq = [],
    _nfcOnBlur = [],
    _nfcOnBlurReq = [],
    _nfcOnChange = [],
    _nfcOnChangeReq = []
  }

instance Semigroup (IntegralFieldCfg s e a) where
  (<>) t1 t2 = IntegralFieldCfg {
    _nfcValid = _nfcValid t2 <|> _nfcValid t1,
    _nfcMinValue = _nfcMinValue t2 <|> _nfcMinValue t1,
    _nfcMaxValue = _nfcMaxValue t2 <|> _nfcMaxValue t1,
    _nfcSelectOnFocus = _nfcSelectOnFocus t2 <|> _nfcSelectOnFocus t1,
    _nfcOnFocus = _nfcOnFocus t1 <> _nfcOnFocus t2,
    _nfcOnFocusReq = _nfcOnFocusReq t1 <> _nfcOnFocusReq t2,
    _nfcOnBlur = _nfcOnBlur t1 <> _nfcOnBlur t2,
    _nfcOnBlurReq = _nfcOnBlurReq t1 <> _nfcOnBlurReq t2,
    _nfcOnChange = _nfcOnChange t1 <> _nfcOnChange t2,
    _nfcOnChangeReq = _nfcOnChangeReq t1 <> _nfcOnChangeReq t2
  }

instance Monoid (IntegralFieldCfg s e a) where
  mempty = def

instance CmbValidInput (IntegralFieldCfg s e a) s where
  validInput field = def {
    _nfcValid = Just (WidgetLens field)
  }

instance CmbSelectOnFocus (IntegralFieldCfg s e a) where
  selectOnFocus sel = def {
    _nfcSelectOnFocus = Just sel
  }

instance FormattableInt a => CmbMinValue (IntegralFieldCfg s e a) a where
  minValue len = def {
    _nfcMinValue = Just len
  }

instance FormattableInt a => CmbMaxValue (IntegralFieldCfg s e a) a where
  maxValue len = def {
    _nfcMaxValue = Just len
  }

instance CmbOnFocus (IntegralFieldCfg s e a) e where
  onFocus fn = def {
    _nfcOnFocus = [fn]
  }

instance CmbOnFocusReq (IntegralFieldCfg s e a) s where
  onFocusReq req = def {
    _nfcOnFocusReq = [req]
  }

instance CmbOnBlur (IntegralFieldCfg s e a) e where
  onBlur fn = def {
    _nfcOnBlur = [fn]
  }

instance CmbOnBlurReq (IntegralFieldCfg s e a) s where
  onBlurReq req = def {
    _nfcOnBlurReq = [req]
  }

instance CmbOnChange (IntegralFieldCfg s e a) a e where
  onChange fn = def {
    _nfcOnChange = [fn]
  }

instance CmbOnChangeReq (IntegralFieldCfg s e a) s where
  onChangeReq req = def {
    _nfcOnChangeReq = [req]
  }

integralField :: FormattableInt a => ALens' s a -> WidgetInstance s e
integralField field = integralField_ field def

integralField_
  :: FormattableInt a
  => ALens' s a
  -> [IntegralFieldCfg s e a]
  -> WidgetInstance s e
integralField_ field configs = integralFieldD_ (WidgetLens field) configs

integralFieldV :: FormattableInt a => a -> (a -> e) -> WidgetInstance s e
integralFieldV value handler = integralFieldV_ value handler def

integralFieldV_
  :: FormattableInt a
  => a
  -> (a -> e)
  -> [IntegralFieldCfg s e a]
  -> WidgetInstance s e
integralFieldV_ value handler configs = newInst where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newInst = integralFieldD_ widgetData newConfigs

integralFieldD_
  :: FormattableInt a
  => WidgetData s a
  -> [IntegralFieldCfg s e a]
  -> WidgetInstance s e
integralFieldD_ widgetData configs = newInst where
  config = mconcat configs
  minVal = _nfcMinValue config
  maxVal = _nfcMaxValue config
  fromText = integralFromText minVal maxVal
  toText = integralToText
  inputConfig = InputFieldCfg {
    _ifcValue = widgetData,
    _ifcValid = _nfcValid config,
    _ifcFromText = fromText,
    _ifcToText = toText,
    _ifcAcceptInput = acceptIntegralInput,
    _ifcSelectOnFocus = fromMaybe True (_nfcSelectOnFocus config),
    _ifcStyle = Just L.inputIntegralStyle,
    _ifcOnFocus = _nfcOnFocus config,
    _ifcOnFocusReq = _nfcOnFocusReq config,
    _ifcOnBlur = _nfcOnBlur config,
    _ifcOnBlurReq = _nfcOnBlurReq config,
    _ifcOnChange = _nfcOnChange config,
    _ifcOnChangeReq = _nfcOnChangeReq config
  }
  newInst = inputField_ "integralField" inputConfig

integralFromText :: FormattableInt a => Maybe a -> Maybe a -> Text -> Maybe a
integralFromText minVal maxVal t = case signed decimal t of
  Right (val, _)
    | numberInBounds minVal maxVal val -> Just val
  _ -> Nothing

integralToText :: FormattableInt a => a -> Text
integralToText val = F.sformat F.int val

acceptIntegralInput :: Text -> Bool
acceptIntegralInput text = isRight (A.parseOnly parser text) where
  sign = A.option "" (single '-')
  number = A.takeWhile isDigit
  parser = join [sign, number] <* A.endOfInput

join :: [A.Parser Text] -> A.Parser Text
join [] = return T.empty
join (x:xs) = (<>) <$> x <*> join xs

single :: Char -> A.Parser Text
single c = T.singleton <$> A.char c
