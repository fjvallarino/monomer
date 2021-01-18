{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Monomer.Core.NumUtil where

import Data.Int
import Data.Word
import Foreign.C.Types

intAddFrac :: (Integral a, RealFrac b) => a -> b -> a
intAddFrac num frac = num + round frac

intMulFrac :: (Integral a, RealFrac b) => a -> b -> a
intMulFrac num frac = round (realToFrac num * frac)

fracAddFrac :: (Fractional a, RealFrac b) => a -> b -> a
fracAddFrac num frac = num + realToFrac frac

fracMulFrac :: (Fractional a, RealFrac b) => a -> b -> a
fracMulFrac num frac = num * realToFrac frac

class Num a => NumRangeable a where
  numAddFrac :: RealFrac b => a -> b -> a
  numMulFrac :: RealFrac b => a -> b -> a
  numToFrac :: RealFrac b => a -> b
  numToIntegral :: Integral b => a -> b

instance NumRangeable Int where
  numAddFrac = intAddFrac
  numMulFrac = intMulFrac
  numToIntegral = fromIntegral
  numToFrac = realToFrac

instance NumRangeable Int8 where
  numAddFrac = intAddFrac
  numMulFrac = intMulFrac
  numToIntegral = fromIntegral
  numToFrac = realToFrac

instance NumRangeable Int16 where
  numAddFrac = intAddFrac
  numMulFrac = intMulFrac
  numToIntegral = fromIntegral
  numToFrac = realToFrac

instance NumRangeable Int32 where
  numAddFrac = intAddFrac
  numMulFrac = intMulFrac
  numToIntegral = fromIntegral
  numToFrac = realToFrac

instance NumRangeable Int64 where
  numAddFrac = intAddFrac
  numMulFrac = intMulFrac
  numToIntegral = fromIntegral
  numToFrac = realToFrac

instance NumRangeable Word where
  numAddFrac = intAddFrac
  numMulFrac = intMulFrac
  numToIntegral = fromIntegral
  numToFrac = realToFrac

instance NumRangeable Word8 where
  numAddFrac = intAddFrac
  numMulFrac = intMulFrac
  numToIntegral = fromIntegral
  numToFrac = realToFrac

instance NumRangeable Word16 where
  numAddFrac = intAddFrac
  numMulFrac = intMulFrac
  numToIntegral = fromIntegral
  numToFrac = realToFrac

instance NumRangeable Word32 where
  numAddFrac = intAddFrac
  numMulFrac = intMulFrac
  numToIntegral = fromIntegral
  numToFrac = realToFrac

instance NumRangeable Word64 where
  numAddFrac = intAddFrac
  numMulFrac = intMulFrac
  numToIntegral = fromIntegral
  numToFrac = realToFrac

instance NumRangeable Integer where
  numAddFrac = intAddFrac
  numMulFrac = intMulFrac
  numToIntegral = fromIntegral
  numToFrac = realToFrac

instance NumRangeable Float where
  numAddFrac = fracAddFrac
  numMulFrac = fracMulFrac
  numToIntegral = round
  numToFrac = realToFrac

instance NumRangeable Double where
  numAddFrac = fracAddFrac
  numMulFrac = fracMulFrac
  numToIntegral = round
  numToFrac = realToFrac

instance NumRangeable CFloat where
  numAddFrac = fracAddFrac
  numMulFrac = fracMulFrac
  numToIntegral = round
  numToFrac = realToFrac

instance NumRangeable CDouble where
  numAddFrac = fracAddFrac
  numMulFrac = fracMulFrac
  numToIntegral = round
  numToFrac = realToFrac

instance NumRangeable Rational where
  numAddFrac = fracAddFrac
  numMulFrac = fracMulFrac
  numToIntegral = round
  numToFrac = realToFrac
