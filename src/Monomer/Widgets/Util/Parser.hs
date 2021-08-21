{-|
Module      : Monomer.Widgets.Util.Parser
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Very basic parsing helpers used by numeric input fields.
-}

{-# LANGUAGE Strict #-}

module Monomer.Widgets.Util.Parser where

import Control.Applicative ((<|>))
import Data.Text (Text)

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

-- | Combines a list of text parsers.
join :: [A.Parser Text] -> A.Parser Text
join [] = return T.empty
join (x:xs) = (<>) <$> x <*> join xs

-- | Combines a parser up to a maximum of repetitions.
upto :: Int -> A.Parser Text -> A.Parser Text
upto n p
  | n > 0 = (<>) <$> A.try p <*> upto (n-1) p <|> return T.empty
  | otherwise = return T.empty

-- | Matches a single character.
single :: Char -> A.Parser Text
single c = T.singleton <$> A.char c
