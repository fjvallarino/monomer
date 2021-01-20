module Monomer.Widgets.Util.SizeReq (
  sizeReqBound,
  sizeReqValid,
  sizeReqAddStyle,
  sizeReqMin,
  sizeReqMax,
  sizeReqMaxBounded,
  sizeReqFixed,
  sizeReqFlex,
  sizeReqExtra,
  sizeReqFactor,
  sizeReqMergeSum,
  sizeReqMergeMax
) where

import Data.Default
import Data.Maybe
import Data.Sequence ((|>))

import Monomer.Core
import Monomer.Event
import Monomer.Widgets.Util.Style
import Monomer.Widgets.Util.Widget

sizeReqBound :: SizeReq -> Double -> Double -> Double
sizeReqBound sizeReq offset value = max minSize (min maxSize value) where
  minSize = offset + sizeReqMin sizeReq
  maxSize = offset + sizeReqMax sizeReq

sizeReqValid :: SizeReq -> Double -> Double -> Bool
sizeReqValid sizeReq offset value = doubleInRange minSize maxSize value where
  minSize = offset + sizeReqMin sizeReq
  maxSize = offset + sizeReqMax sizeReq

sizeReqAddStyle :: StyleState -> (SizeReq, SizeReq) -> (SizeReq, SizeReq)
sizeReqAddStyle style (reqW, reqH) = (newReqW, newReqH) where
  Size w h = fromMaybe def (addOuterSize style def)
  realReqW = fromMaybe reqW (_sstSizeReqW style)
  realReqH = fromMaybe reqH (_sstSizeReqH style)
  newReqW = modifySizeReq realReqW (+w)
  newReqH = modifySizeReq realReqH (+h)

sizeReqMin :: SizeReq -> Double
sizeReqMin (FixedSize c) = c
sizeReqMin (FlexSize c _) = 0
sizeReqMin (MinSize c _) = c
sizeReqMin (MaxSize c _) = 0
sizeReqMin (RangeSize c1 c2 _) = c1

sizeReqMax :: SizeReq -> Double
sizeReqMax (FixedSize c) = c
sizeReqMax (FlexSize c _) = maxNumericValue
sizeReqMax (MinSize c _) = maxNumericValue
sizeReqMax (MaxSize c _) = c
sizeReqMax (RangeSize c1 c2 _) = c2

sizeReqMaxBounded :: SizeReq -> Double
sizeReqMaxBounded (FixedSize c) = c
sizeReqMaxBounded (FlexSize c _) = c
sizeReqMaxBounded (MinSize c _) = c
sizeReqMaxBounded (MaxSize c _) = c
sizeReqMaxBounded (RangeSize c1 c2 _) = c2

sizeReqFixed :: SizeReq -> Double
sizeReqFixed (FixedSize s) = s
sizeReqFixed (FlexSize s _) = 0
sizeReqFixed (MinSize s _) = s
sizeReqFixed (MaxSize s _) = 0
sizeReqFixed (RangeSize s1 _ _) = s1

sizeReqFlex :: SizeReq -> Double
sizeReqFlex (FixedSize s) = 0
sizeReqFlex (FlexSize s _) = s
-- This one is 0 to avoid MinSize claiming the same space again in extra
sizeReqFlex (MinSize s _) = 0
sizeReqFlex (MaxSize s _) = s
sizeReqFlex (RangeSize s1 s2 _) = s2 - s1

sizeReqExtra :: SizeReq -> Double
sizeReqExtra (FlexSize s _) = s
sizeReqExtra (MinSize s _) = s
sizeReqExtra _ = 0

sizeReqFactor :: SizeReq -> Double
sizeReqFactor (FixedSize _) = 1
sizeReqFactor (FlexSize _ f) = f
sizeReqFactor (MinSize _ f) = f
sizeReqFactor (MaxSize _ f) = f
sizeReqFactor (RangeSize _ _ f) = f

sizeReqMergeSum :: SizeReq -> SizeReq -> SizeReq
sizeReqMergeSum req1 req2 = case (req1, req2) of
  -- Fixed
  (FixedSize s1, FixedSize s2) -> FixedSize (s1 + s2)
  (FixedSize s1, FlexSize s2 f2) -> mkRangeSize s1 (s1 + s2) f2
  (FixedSize s1, MinSize s2 f2) -> MinSize (s1 + s2) f2
  (FixedSize s1, MaxSize s2 f2) -> mkRangeSize s1 (s1 + s2) f2
  (FixedSize s1, RangeSize sa2 sb2 f2) -> mkRangeSize sa2 (s1 + sb2) f2
  -- Flex
  (FlexSize s1 f1, FlexSize s2 f2) -> FlexSize (s1 + s2) (max f1 f2)
  (FlexSize s1 f1, MinSize s2 f2) -> mkRangeSize s2 (s1 + s2) (max f1 f2)
  (FlexSize s1 f1, MaxSize s2 f2) -> FlexSize (s1 + s2) (max f1 f2)
  (FlexSize s1 f1, RangeSize sa2 sb2 f2) -> mkRangeSize sa2 (s1 + sb2) (max f1 f2)
  -- Min
  (MinSize s1 f1, MinSize s2 f2) -> MinSize (s1 + s2) (max f1 f2)
  (MinSize s1 f1, MaxSize s2 f2) -> mkRangeSize s1 (s1 + s2) (max f1 f2)
  (MinSize s1 f1, RangeSize sa2 sb2 f2) -> mkRangeSize (s1 + sa2) (s1 + sb2) (max f1 f2)
  -- Max
  (MaxSize s1 f1, MaxSize s2 f2) -> MaxSize (s1 + s2) (max f1 f2)
  (MaxSize s1 f1, RangeSize sa2 sb2 f2) -> mkRangeSize sa2 (s1 + sb2) (max f1 f2)
  -- Range
  (RangeSize sa1 sb1 f1, RangeSize sa2 sb2 f2) -> mkRangeSize (sa1 + sa2) (sb1 + sb2) (max f1 f2)
  -- Reverse handled with existing cases
  (pending1, pending2) -> sizeReqMergeSum pending2 pending1

sizeReqMergeMax :: SizeReq -> SizeReq -> SizeReq
sizeReqMergeMax req1 req2 = case (req1, req2) of
  -- Fixed
  (FixedSize s1, FixedSize s2) -> FixedSize (max s1 s2)
  (FixedSize s1, FlexSize s2 f2) -> mkRangeSize s1 (max s1 s2) f2
  (FixedSize s1, MinSize s2 f2) -> MinSize (max s1 s2) f2
  (FixedSize s1, MaxSize s2 f2) -> mkRangeSize s1 (max s1 s2) f2
  (FixedSize s1, RangeSize sa2 sb2 f2) -> mkRangeSize (max s1 sa2) (max s1 sb2) f2
  -- Flex
  (FlexSize s1 f1, FlexSize s2 f2) -> FlexSize (max s1 s2) (max f1 f2)
  (FlexSize s1 f1, MinSize s2 f2) -> MinSize s2 (max f1 f2)
  (FlexSize s1 f1, MaxSize s2 f2) -> FlexSize (max s1 s2) f1
  (FlexSize s1 f1, RangeSize sa2 sb2 f2) -> mkRangeSize sa2 (max s1 sb2) (max f1 f2)
  -- Min
  (MinSize s1 f1, MinSize s2 f2) -> MinSize (max s1 s2) (max f1 f2)
  (MinSize s1 f1, MaxSize s2 f2) -> MinSize s1 f1
  (MinSize s1 f1, RangeSize sa2 sb2 f2) -> MinSize (max s1 sa2) f1
  -- Max
  (MaxSize s1 f1, MaxSize s2 f2) -> MaxSize (max s1 s2) (max f1 f2)
  (MaxSize s1 f1, RangeSize sa2 sb2 f2) -> mkRangeSize sa2 (max s1 sb2) (max f1 f2)
  -- Range
  (RangeSize sa1 sb1 f1, RangeSize sa2 sb2 f2) -> mkRangeSize (max sa1 sa2) (max sb1 sb2) (max f1 f2)
  -- Reverse handled with existing cases
  (pending1, pending2) -> sizeReqMergeMax pending2 pending1

modifySizeReq :: SizeReq -> (Double -> Double) -> SizeReq
modifySizeReq (FixedSize c) f = FixedSize (f c)
modifySizeReq (FlexSize c factor) f = FlexSize (f c) factor
modifySizeReq (MinSize c factor) f = MinSize (f c) factor
modifySizeReq (MaxSize c factor) f = MaxSize (f c) factor
modifySizeReq (RangeSize c1 c2 factor) f = RangeSize (f c1) (f c2) factor

mkRangeSize :: Double -> Double -> Double -> SizeReq
mkRangeSize s1 s2 f
  | abs (s2 - s1) < 0.01 = FixedSize s1
  | otherwise = RangeSize s1 s2 f

doubleInRange :: Double -> Double -> Double -> Bool
doubleInRange minValue maxValue curValue = validMin && validMax where
  minDiff = curValue - minValue
  maxDiff = maxValue - curValue
  -- Some calculations may leave small differences in otherwise valid results
  validMin = minDiff >= 0 || abs minDiff < 0.0001
  validMax = maxDiff >= 0 || abs maxDiff < 0.0001
