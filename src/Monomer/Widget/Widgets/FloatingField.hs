{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- HLINT ignore "Use foldr" -}

module Monomer.Widget.Widgets.FloatingField where

import Control.Applicative ((<|>))
import Control.Lens (ALens')
import Data.Char
import Data.Default
import Data.Either
import Data.Maybe
import Data.Text (Text)
import Data.Text.Read (rational)
import Data.Typeable (Typeable)

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Formatting as F

import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.Widgets.InputField
import Monomer.Widget.Widgets.WidgetCombinators

type FormattableFloat a = (Eq a, Default a, Typeable a, Fractional a, Real a)

data FloatingFieldCfg s e a = FloatingFieldCfg {
  _ffcValid :: Maybe (WidgetValue s Bool),
  _ffcDecimals :: Maybe Int,
  _ffcMinValue :: Maybe a,
  _ffcMaxValue :: Maybe a,
  _ffcOnChange :: [a -> e],
  _ffcOnChangeReq :: [WidgetRequest s]
}

instance Default (FloatingFieldCfg s e a) where
  def = FloatingFieldCfg {
    _ffcValid = Nothing,
    _ffcDecimals = Nothing,
    _ffcMinValue = Nothing,
    _ffcMaxValue = Nothing,
    _ffcOnChange = [],
    _ffcOnChangeReq = []
  }

instance Semigroup (FloatingFieldCfg s e a) where
  (<>) t1 t2 = FloatingFieldCfg {
    _ffcValid = _ffcValid t2 <|> _ffcValid t1,
    _ffcDecimals = _ffcDecimals t2 <|> _ffcDecimals t1,
    _ffcMinValue = _ffcMinValue t2 <|> _ffcMinValue t1,
    _ffcMaxValue = _ffcMaxValue t2 <|> _ffcMaxValue t1,
    _ffcOnChange = _ffcOnChange t1 <> _ffcOnChange t2,
    _ffcOnChangeReq = _ffcOnChangeReq t1 <> _ffcOnChangeReq t2
  }

instance Monoid (FloatingFieldCfg s e a) where
  mempty = def

instance ValidInput (FloatingFieldCfg s e a) s where
  validInput field = def {
    _ffcValid = Just (WidgetLens field)
  }

instance FormattableFloat a => MinValue (FloatingFieldCfg s e a) a where
  minValue len = def {
    _ffcMinValue = Just len
  }

instance FormattableFloat a => MaxValue (FloatingFieldCfg s e a) a where
  maxValue len = def {
    _ffcMaxValue = Just len
  }

instance Decimals (FloatingFieldCfg s e a) where
  decimals num = def {
    _ffcDecimals = Just num
  }

instance OnChange (FloatingFieldCfg s e a) a e where
  onChange fn = def {
    _ffcOnChange = [fn]
  }

instance OnChangeReq (FloatingFieldCfg s e a) s where
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
  -> FloatingFieldCfg s e a
  -> WidgetInstance s e
floatingField_ field config = newInst where
  inputConfig = inputFieldCfg (WidgetLens field) fromText toText
  minVal = _ffcMinValue config
  maxVal = _ffcMaxValue config
  decimals = max 0 $ fromMaybe 2 (_ffcDecimals config)
  fromText = floatFromText minVal maxVal
  toText = floatToText decimals
  newInst = inputField_ "floatingField" inputConfig {
    _ifcValid = _ffcValid config,
    _ifcAcceptInput = acceptFloatInput decimals
  }

floatFromText :: FormattableFloat a => Maybe a -> Maybe a -> Text -> Maybe a
floatFromText minVal maxVal t = case rational t of
  Right (val, _)
    | numberInBounds minVal maxVal val -> Just val
  _ -> Nothing

floatToText :: FormattableFloat a => Int -> a -> Text
floatToText decimals val = F.sformat (F.fixed decimals) val

acceptFloatInput :: Int -> Text -> Bool
acceptFloatInput decimals text = isRight (A.parseOnly parser text) where
  number = A.takeWhile isDigit
  digit = T.singleton <$> A.digit
  rest = join [single '.', upto decimals digit]
  parser = join [number, A.option "" rest] <* A.endOfInput

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
