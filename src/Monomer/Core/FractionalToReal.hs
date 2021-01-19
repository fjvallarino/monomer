{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Monomer.Core.FractionalToReal where

import Data.Int
import Data.Word
import Foreign.C.Types

fractionalToIntegral :: (Integral a, Real b, Fractional b) => b -> a
fractionalToIntegral num = round newNum where
  newNum :: Rational
  newNum = realToFrac num

class Real a => FractionalToReal a where
  fractionalToReal :: (Real b, Fractional b) => b -> a

instance FractionalToReal Int where
  fractionalToReal = fractionalToIntegral

instance FractionalToReal Int8 where
  fractionalToReal = fractionalToIntegral

instance FractionalToReal Int16 where
  fractionalToReal = fractionalToIntegral

instance FractionalToReal Int32 where
  fractionalToReal = fractionalToIntegral

instance FractionalToReal Int64 where
  fractionalToReal = fractionalToIntegral

instance FractionalToReal Word where
  fractionalToReal = fractionalToIntegral

instance FractionalToReal Word8 where
  fractionalToReal = fractionalToIntegral

instance FractionalToReal Word16 where
  fractionalToReal = fractionalToIntegral

instance FractionalToReal Word32 where
  fractionalToReal = fractionalToIntegral

instance FractionalToReal Word64 where
  fractionalToReal = fractionalToIntegral

instance FractionalToReal Integer where
  fractionalToReal = fractionalToIntegral

instance FractionalToReal Float where
  fractionalToReal = realToFrac

instance FractionalToReal Double where
  fractionalToReal = realToFrac

instance FractionalToReal CFloat where
  fractionalToReal = realToFrac

instance FractionalToReal CDouble where
  fractionalToReal = realToFrac

instance FractionalToReal Rational where
  fractionalToReal = realToFrac
