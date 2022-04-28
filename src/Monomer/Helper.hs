{-|
Module      : Monomer.Helper
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions used across the library. They do not belong to any specific
module and are not directly exported.
-}
{-# LANGUAGE BangPatterns #-}

module Monomer.Helper where

import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (MonadIO)
import Data.Sequence (Seq(..))

import qualified Data.Sequence as Seq

-- | Concats a list of Monoids or returns Nothing if empty.
maybeConcat :: Monoid a => [a] -> Maybe a
maybeConcat [] = Nothing
maybeConcat lst = Just merged where
  !merged = mconcat lst

-- | Runs an action until Nothing is returned, collecting the results in a list.
collectJustM :: MonadIO m => m (Maybe a) -> m [a]
collectJustM action = do
  x <- action
  case x of
    Nothing -> return []
    Just x -> do
      xs <- collectJustM action
      return (x : xs)

-- | Returns the last item in a sequence. Unsafe, fails if sequence is empty.
seqLast :: Seq a -> a
seqLast seq
  | not (null seq) = Seq.index seq (length seq - 1)
  | otherwise = error "Invalid sequence provided to seqLast"

-- | Checks if the first sequence is a prefix of the second.
seqStartsWith :: Eq a => Seq a -> Seq a -> Bool
seqStartsWith prefix seq = Seq.take (length prefix) seq == prefix

-- | Filters Nothing instances out of a Seq, and removes the Just wrapper.
seqCatMaybes :: Seq (Maybe a) -> Seq a
seqCatMaybes Empty = Empty
seqCatMaybes (x :<| xs) = case x of
  Just val -> val :<| seqCatMaybes xs
  _ -> seqCatMaybes xs

-- | Folds a list of functions over an initial value.
applyFnList :: [a -> a] -> a -> a
applyFnList fns initial = foldl (flip ($)) initial fns

-- | Returns the maximum value of a given floating type.
maxNumericValue :: (RealFloat a) => a
maxNumericValue = x where
  n = floatDigits x
  b = floatRadix x
  (_, u) = floatRange x
  x = encodeFloat (b^n - 1) (u - n)

-- | Restricts a value to a given range.
clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

-- | Catches any exception thrown by the provided action
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch

-- | Returns Just the first item if the list is not empty, Nothing otherwise.
headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x : _) = Just x
