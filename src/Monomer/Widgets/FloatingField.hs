{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- HLINT ignore "Use foldr" -}

module Monomer.Widgets.FloatingField (
  FloatingFieldCfg,
  floatingField,
  floatingField_,
  floatingFieldV,
  floatingFieldV_,
  floatingFieldD_
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens')
import Data.Char
import Data.Default
import Data.Either
import Data.Maybe
import Data.Text (Text)
import Data.Text.Read (signed, rational)
import Data.Typeable (Typeable)

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Formatting as F

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Widgets.InputField
import Monomer.Widgets.Util

import qualified Monomer.Lens as L

type FormattableFloat a = (Eq a, Default a, Typeable a, Fractional a, Real a)

data FloatingFieldCfg s e a = FloatingFieldCfg {
  _ffcValid :: Maybe (WidgetData s Bool),
  _ffcDecimals :: Maybe Int,
  _ffcMinValue :: Maybe a,
  _ffcMaxValue :: Maybe a,
  _ffcSelectOnFocus :: Maybe Bool,
  _ffcOnFocus :: [e],
  _ffcOnFocusReq :: [WidgetRequest s],
  _ffcOnBlur :: [e],
  _ffcOnBlurReq :: [WidgetRequest s],
  _ffcOnChange :: [a -> e],
  _ffcOnChangeReq :: [WidgetRequest s]
}

instance Default (FloatingFieldCfg s e a) where
  def = FloatingFieldCfg {
    _ffcValid = Nothing,
    _ffcDecimals = Nothing,
    _ffcMinValue = Nothing,
    _ffcMaxValue = Nothing,
    _ffcSelectOnFocus = Nothing,
    _ffcOnFocus = [],
    _ffcOnFocusReq = [],
    _ffcOnBlur = [],
    _ffcOnBlurReq = [],
    _ffcOnChange = [],
    _ffcOnChangeReq = []
  }

instance Semigroup (FloatingFieldCfg s e a) where
  (<>) t1 t2 = FloatingFieldCfg {
    _ffcValid = _ffcValid t2 <|> _ffcValid t1,
    _ffcDecimals = _ffcDecimals t2 <|> _ffcDecimals t1,
    _ffcMinValue = _ffcMinValue t2 <|> _ffcMinValue t1,
    _ffcMaxValue = _ffcMaxValue t2 <|> _ffcMaxValue t1,
    _ffcSelectOnFocus = _ffcSelectOnFocus t2 <|> _ffcSelectOnFocus t1,
    _ffcOnFocus = _ffcOnFocus t1 <> _ffcOnFocus t2,
    _ffcOnFocusReq = _ffcOnFocusReq t1 <> _ffcOnFocusReq t2,
    _ffcOnBlur = _ffcOnBlur t1 <> _ffcOnBlur t2,
    _ffcOnBlurReq = _ffcOnBlurReq t1 <> _ffcOnBlurReq t2,
    _ffcOnChange = _ffcOnChange t1 <> _ffcOnChange t2,
    _ffcOnChangeReq = _ffcOnChangeReq t1 <> _ffcOnChangeReq t2
  }

instance Monoid (FloatingFieldCfg s e a) where
  mempty = def

instance CmbValidInput (FloatingFieldCfg s e a) s where
  validInput field = def {
    _ffcValid = Just (WidgetLens field)
  }

instance CmbSelectOnFocus (FloatingFieldCfg s e a) where
  selectOnFocus sel = def {
    _ffcSelectOnFocus = Just sel
  }

instance FormattableFloat a => CmbMinValue (FloatingFieldCfg s e a) a where
  minValue len = def {
    _ffcMinValue = Just len
  }

instance FormattableFloat a => CmbMaxValue (FloatingFieldCfg s e a) a where
  maxValue len = def {
    _ffcMaxValue = Just len
  }

instance CmbDecimals (FloatingFieldCfg s e a) where
  decimals num = def {
    _ffcDecimals = Just num
  }

instance CmbOnFocus (FloatingFieldCfg s e a) e where
  onFocus fn = def {
    _ffcOnFocus = [fn]
  }

instance CmbOnFocusReq (FloatingFieldCfg s e a) s where
  onFocusReq req = def {
    _ffcOnFocusReq = [req]
  }

instance CmbOnBlur (FloatingFieldCfg s e a) e where
  onBlur fn = def {
    _ffcOnBlur = [fn]
  }

instance CmbOnBlurReq (FloatingFieldCfg s e a) s where
  onBlurReq req = def {
    _ffcOnBlurReq = [req]
  }

instance CmbOnChange (FloatingFieldCfg s e a) a e where
  onChange fn = def {
    _ffcOnChange = [fn]
  }

instance CmbOnChangeReq (FloatingFieldCfg s e a) s where
  onChangeReq req = def {
    _ffcOnChangeReq = [req]
  }

floatingField
  :: FormattableFloat a
  => ALens' s a -> WidgetInstance s e
floatingField field = floatingField_ field def

floatingField_
  :: FormattableFloat a
  => ALens' s a
  -> [FloatingFieldCfg s e a]
  -> WidgetInstance s e
floatingField_ field configs = floatingFieldD_ (WidgetLens field) configs

floatingFieldV
  :: FormattableFloat a
  => a -> (a -> e) -> WidgetInstance s e
floatingFieldV value handler = floatingFieldV_ value handler def

floatingFieldV_
  :: FormattableFloat a
  => a
  -> (a -> e)
  -> [FloatingFieldCfg s e a]
  -> WidgetInstance s e
floatingFieldV_ value handler configs = newInst where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newInst = floatingFieldD_ widgetData newConfigs

floatingFieldD_
  :: FormattableFloat a
  => WidgetData s a
  -> [FloatingFieldCfg s e a]
  -> WidgetInstance s e
floatingFieldD_ widgetData configs = newInst where
  config = mconcat configs
  minVal = _ffcMinValue config
  maxVal = _ffcMaxValue config
  decimals = max 0 $ fromMaybe 2 (_ffcDecimals config)
  fromText = floatFromText minVal maxVal
  toText = floatToText decimals
  inputConfig = InputFieldCfg {
    _ifcValue = widgetData,
    _ifcValid = _ffcValid config,
    _ifcFromText = fromText,
    _ifcToText = toText,
    _ifcAcceptInput = acceptFloatInput decimals,
    _ifcSelectOnFocus = fromMaybe True (_ffcSelectOnFocus config),
    _ifcStyle = Just L.inputFloatingStyle,
    _ifcOnFocus = _ffcOnFocus config,
    _ifcOnFocusReq = _ffcOnFocusReq config,
    _ifcOnBlur = _ffcOnBlur config,
    _ifcOnBlurReq = _ffcOnBlurReq config,
    _ifcOnChange = _ffcOnChange config,
    _ifcOnChangeReq = _ffcOnChangeReq config
  }
  newInst = inputField_ "floatingField" inputConfig

floatFromText :: FormattableFloat a => Maybe a -> Maybe a -> Text -> Maybe a
floatFromText minVal maxVal t = case signed rational t of
  Right (val, _)
    | numberInBounds minVal maxVal val -> Just val
  _ -> Nothing

floatToText :: FormattableFloat a => Int -> a -> Text
floatToText decimals val = F.sformat (F.fixed decimals) val

acceptFloatInput :: Int -> Text -> Bool
acceptFloatInput decimals text = isRight (A.parseOnly parser text) where
  sign = A.option "" (single '-')
  number = A.takeWhile isDigit
  digit = T.singleton <$> A.digit
  rest = join [single '.', upto decimals digit]
  parser = join [sign, number, A.option "" rest] <* A.endOfInput

-- Parsing helpers
join :: [A.Parser Text] -> A.Parser Text
join [] = return T.empty
join (x:xs) = (<>) <$> x <*> join xs

upto :: Int -> A.Parser Text -> A.Parser Text
upto n p
  | n > 0 = (<>) <$> A.try p <*> upto (n-1) p <|> return T.empty
  | otherwise = return T.empty

single :: Char -> A.Parser Text
single c = T.singleton <$> A.char c
