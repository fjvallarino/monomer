{-|
Module      : Monomer.Helper
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions used across the library. Do not belong to any specifig module
and are not directly exported.
-}
module Monomer.Helper where

import Data.Sequence (Seq(..))

import qualified Data.Sequence as Seq

-- | Concats a list of Monoids or returns Nothing if empty.
maybeConcat :: Monoid a => [a] -> Maybe a
maybeConcat [] = Nothing
maybeConcat lst = Just (mconcat lst)

-- | Checks if the first sequence is a prefix of the second.
seqStartsWith :: Eq a => Seq a -> Seq a -> Bool
seqStartsWith prefix seq = Seq.take (length prefix) seq == prefix

-- | Filters Nothing instances out of a Seq, and removes the Just wrapper.
seqCatMaybes :: Seq (Maybe a) -> Seq a
seqCatMaybes Empty = Empty
seqCatMaybes (x :<| xs) = case x of
  Just val -> val :<| seqCatMaybes xs
  _ -> seqCatMaybes xs

-- Returns the maximum value of a given floating type.
maxNumericValue :: (RealFloat a) => a
maxNumericValue = x where
  n = floatDigits x
  b = floatRadix x
  (_, u) = floatRange x
  x = encodeFloat (b^n - 1) (u - n)

-- | Restricts a value to a given range.
clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx
