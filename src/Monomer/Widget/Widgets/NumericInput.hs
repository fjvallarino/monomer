{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{- HLINT ignore "Use foldr" -}

module Monomer.Widget.Widgets.NumericInput where

import Control.Applicative ((<|>))
import Control.Lens (ALens')
import Data.Char
import Data.Default
import Data.Either
import Data.Maybe
import Data.Text (Text)
import Data.Text.Read (decimal, rational)
import Text.Printf

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Formatting as F

import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.Widgets.TextField

type FormattableInt a = (Eq a, Integral a, Real a)
type FormattableFloat a = (Eq a, Fractional a, Real a)

integerInput :: FormattableInt a => ALens' s a -> WidgetInstance s e
integerInput field = newInst where
  config = textFieldCfg (WidgetLens field) integerFromText integerToText
  newInst = textField_ config {
    _tfcAcceptInput = acceptIntegerInput
  }

integerFromText :: FormattableInt a => Text -> Maybe a
integerFromText t = case decimal t of
  Left _ -> Nothing
  Right (val, _) -> Just val

integerToText :: FormattableInt a => a -> Text
integerToText val = F.sformat F.int val

acceptIntegerInput :: Text -> Bool
acceptIntegerInput text = isRight (A.parseOnly parser text) where
  number = A.takeWhile isDigit
  parser = number <* A.endOfInput

floatingInput :: FormattableFloat a => ALens' s a -> WidgetInstance s e
floatingInput field = floatingInput_ field 2

floatingInput_ :: FormattableFloat a => ALens' s a -> Int -> WidgetInstance s e
floatingInput_ field decimals = newInst where
  config = textFieldCfg (WidgetLens field) floatFromText (floatToText decimals)
  newInst = textField_ config {
    _tfcAcceptInput = acceptFloatInput decimals
  }

floatFromText :: FormattableFloat a => Text -> Maybe a
floatFromText t = case rational t of
  Left _ -> Nothing
  Right (val, _) -> Just val

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
