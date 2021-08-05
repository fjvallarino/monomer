{-|
Module      : Monomer.Core.FromFractional
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Conversions from Fractional to several types. Used by dial, numericField,
slider and other numeric related widgets.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Core.FromFractional (
  FromFractional(..)
) where

import Data.Int
import Data.Fixed
import Data.Word
import Foreign.C.Types

fractionalToIntegral :: (Integral a, Real b, Fractional b) => b -> a
fractionalToIntegral num = round newNum where
  newNum :: Rational
  newNum = realToFrac num

-- | Converts a Fractional number to the target type.
class Real a => FromFractional a where
  fromFractional :: (Real b, Fractional b) => b -> a

instance FromFractional Integer where
  fromFractional = fractionalToIntegral

instance FromFractional Int where
  fromFractional = fractionalToIntegral

instance FromFractional Int8 where
  fromFractional = fractionalToIntegral

instance FromFractional Int16 where
  fromFractional = fractionalToIntegral

instance FromFractional Int32 where
  fromFractional = fractionalToIntegral

instance FromFractional Int64 where
  fromFractional = fractionalToIntegral

instance FromFractional Word where
  fromFractional = fractionalToIntegral

instance FromFractional Word8 where
  fromFractional = fractionalToIntegral

instance FromFractional Word16 where
  fromFractional = fractionalToIntegral

instance FromFractional Word32 where
  fromFractional = fractionalToIntegral

instance FromFractional Word64 where
  fromFractional = fractionalToIntegral

instance FromFractional Float where
  fromFractional = realToFrac

instance FromFractional Double where
  fromFractional = realToFrac

instance FromFractional CFloat where
  fromFractional = realToFrac

instance FromFractional CDouble where
  fromFractional = realToFrac

instance FromFractional Rational where
  fromFractional = realToFrac

instance HasResolution a => FromFractional (Fixed a) where
  fromFractional = realToFrac
