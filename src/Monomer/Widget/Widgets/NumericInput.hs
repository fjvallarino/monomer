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
import Data.Typeable (Typeable)

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Formatting as F

import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.Widgets.InputField

type FormattableInt a = (Eq a, Default a, Typeable a, Integral a, Real a)
type FormattableFloat a = (Eq a, Default a, Typeable a, Fractional a, Real a)

integerInput :: FormattableInt a => ALens' s a -> WidgetInstance s e
integerInput field = newInst where
  config = inputFieldCfg (WidgetLens field) integerFromText integerToText
  newInst = inputField_ "integerInput" config {
    _ifcAcceptInput = acceptIntegerInput
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

floatingInput
  :: FormattableFloat a
  => ALens' s a -> ALens' s Bool -> WidgetInstance s e
floatingInput field valid = floatingInput_ field valid 2

floatingInput_
  :: FormattableFloat a
  => ALens' s a -> ALens' s Bool -> Int -> WidgetInstance s e
floatingInput_ field valid decimals = newInst where
  config = inputFieldCfg (WidgetLens field) floatFromText (floatToText decimals)
  newInst = inputField_ "floatingInput" config {
    _ifcValid = Just (WidgetLens valid),
    _ifcAcceptInput = acceptFloatInput decimals
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
