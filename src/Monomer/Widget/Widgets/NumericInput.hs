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
import Data.Text.Read (rational)
import Text.Printf

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.Widgets.TextField

numericInput :: ALens' s Double -> WidgetInstance s e
numericInput field = newInst where
  config = textFieldCfg (WidgetLens field) fromText toText
  newInst = textField_ config {
    _tfcAcceptInput = acceptInput
  }

fromText :: Fractional a => Text -> Maybe a
fromText t = case rational t of
  Left _ -> Nothing
  Right (val, _) -> Just val

toText :: (Fractional a, PrintfArg a) => a -> Text
toText val = T.pack $ printf "%0.2f" val

acceptInput :: Text -> Bool
acceptInput t = isRight (A.parseOnly parser t) where
  number = A.takeWhile isDigit
  digit = T.singleton <$> A.digit
  rest = join [single '.', upto 2 digit]
  parser = join [number, A.option "" rest] <* A.endOfInput

join :: [A.Parser Text] -> A.Parser Text
join [] = return T.empty
join (x:xs) = (<>) <$> x <*> join xs

upto :: Int -> A.Parser Text -> A.Parser Text
upto n p
  | n > 0 = (<>) <$> A.try p <*> upto (n-1) p <|> return T.empty
  | otherwise = return T.empty

single :: Char -> A.Parser Text
single c = T.singleton <$> A.char c
