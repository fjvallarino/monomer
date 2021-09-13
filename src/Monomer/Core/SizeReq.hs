{-|
Module      : Monomer.Core.SizeReq
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions creating, validating and merging size requirements.
-}
{-# LANGUAGE Strict #-}

module Monomer.Core.SizeReq (
  SizeReqUpdater(..),
  clearExtra,
  fixedToMinW,
  fixedToMinH,
  fixedToMaxW,
  fixedToMaxH,
  fixedToExpandW,
  fixedToExpandH,
  sizeReqBounded,
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

import Control.Lens ((&), (^.), (.~))
import Data.Bits
import Data.Default
import Data.Maybe

import Monomer.Common
import Monomer.Core.StyleTypes
import Monomer.Core.StyleUtil
import Monomer.Core.Util
import Monomer.Helper

import qualified Monomer.Core.Lens as L

-- | Transforms a SizeReq pair by applying an arbitrary operation.
type SizeReqUpdater = (SizeReq, SizeReq) -> (SizeReq, SizeReq)

-- | Clears the extra field of a pair of SizeReqs.
clearExtra :: SizeReqUpdater
clearExtra (reqW, reqH) = (reqW & L.extra .~ 0, reqH & L.extra .~ 0)

-- | Switches a SizeReq pair from fixed width to minimum width.
fixedToMinW
  :: Double          -- ^ The resize factor.
  -> SizeReqUpdater  -- ^ The updated SizeReq.
fixedToMinW fw (SizeReq fixed _ _ _, reqH) = (newReqH, reqH) where
  newReqH = SizeReq fixed 0 fixed fw

-- | Switches a SizeReq pair from fixed height to minimum height.
fixedToMinH
  :: Double          -- ^ The resize factor.
  -> SizeReqUpdater  -- ^ The updated SizeReq.
fixedToMinH fh (reqW, SizeReq fixed _ _ _) = (reqW, newReqH) where
  newReqH = SizeReq fixed 0 fixed fh

-- | Switches a SizeReq pair from fixed width to maximum width.
fixedToMaxW
  :: Double          -- ^ The resize factor.
  -> SizeReqUpdater  -- ^ The updated SizeReq.
fixedToMaxW fw (SizeReq fixed _ _ _, reqH) = (newReqH, reqH) where
  newReqH = SizeReq 0 fixed 0 fw

-- | Switches a SizeReq pair from fixed height to maximum height.
fixedToMaxH
  :: Double          -- ^ The resize factor.
  -> SizeReqUpdater  -- ^ The updated SizeReq.
fixedToMaxH fh (reqW, SizeReq fixed _ _ _) = (reqW, newReqH) where
  newReqH = SizeReq 0 fixed 0 fh

-- | Switches a SizeReq pair from fixed width to expand width.
fixedToExpandW
  :: Double          -- ^ The resize factor.
  -> SizeReqUpdater  -- ^ The updated SizeReq.
fixedToExpandW fw (SizeReq fixed _ _ _, reqH) = (newReqH, reqH) where
  newReqH = SizeReq 0 fixed fixed fw

-- | Switches a SizeReq pair from fixed height to expand height.
fixedToExpandH
  :: Double          -- ^ The resize factor.
  -> SizeReqUpdater  -- ^ The updated SizeReq.
fixedToExpandH fh (reqW, SizeReq fixed _ _ _) = (reqW, newReqH) where
  newReqH = SizeReq 0 fixed fixed fh

-- | Returns a bounded value by the SizeReq, starting from value and offset.
sizeReqBounded :: SizeReq -> Double -> Double -> Double
sizeReqBounded sizeReq offset value = max minSize (min maxSize value) where
  minSize = offset + sizeReqMin sizeReq
  maxSize = offset + sizeReqMax sizeReq

-- | Checks that value, given an offset, matches a SizeReq.
sizeReqValid :: SizeReq -> Double -> Double -> Bool
sizeReqValid sizeReq offset value = doubleInRange minSize maxSize value where
  minSize = offset + sizeReqMin sizeReq
  maxSize = offset + sizeReqMax sizeReq

-- | Adds border/padding size to a SizeReq pair.
sizeReqAddStyle :: StyleState -> (SizeReq, SizeReq) -> (SizeReq, SizeReq)
sizeReqAddStyle style (reqW, reqH) = (newReqW, newReqH) where
  Size w h = fromMaybe def (addOuterSize style def)
  realReqW = fromMaybe reqW (_sstSizeReqW style)
  realReqH = fromMaybe reqH (_sstSizeReqH style)
  newReqW = modifySizeReq realReqW (+w)
  newReqH = modifySizeReq realReqH (+h)

-- | Returns the minimum valid value for a SizeReq.
sizeReqMin :: SizeReq -> Double
sizeReqMin req = req ^. L.fixed

-- | Returns the maximum valid value for a SizeReq. This can be unbounded if
--   extra field is not zero.
sizeReqMax :: SizeReq -> Double
sizeReqMax req
  | req ^. L.extra > 0 = maxNumericValue
  | otherwise = req ^. L.fixed + req ^. L.flex

-- | Returns the maximum, bounded, valid value for a SizeReq. Extra is ignored.
sizeReqMaxBounded :: SizeReq -> Double
sizeReqMaxBounded req = req ^. L.fixed + req ^. L.flex

-- | Returns the fixed size of a SizeReq.
sizeReqFixed :: SizeReq -> Double
sizeReqFixed req = req ^. L.fixed

-- | Returns the flex size of a SizeReq.
sizeReqFlex :: SizeReq -> Double
sizeReqFlex req = req ^. L.flex

-- | Returns the extra size of a SizeReq.
sizeReqExtra :: SizeReq -> Double
sizeReqExtra req = req ^. L.extra

-- | Returns the resize factor of a SizeReq.
sizeReqFactor :: SizeReq -> Double
sizeReqFactor req = req ^. L.factor

{-|
Sums two SizeReqs. This is used for combining two widgets one after the other,
/summing/ their sizes.

The fixed, flex and extra fields are summed individually, while the max factor
is kept.
-}
sizeReqMergeSum :: SizeReq -> SizeReq -> SizeReq
sizeReqMergeSum req1 req2 = newReq where
  newReq = SizeReq {
    _szrFixed = _szrFixed req1 + _szrFixed req2,
    _szrFlex = _szrFlex req1 + _szrFlex req2,
    _szrExtra = _szrExtra req1 + _szrExtra req2,
    _szrFactor = max (_szrFactor req1) (_szrFactor req2)
  }

{-|
Merges two SizeReqs. This is used for combining two widgets by keeping the
largest size requirement.

Fields are combined in order to first satisfy fixed requirements, adapting flex
if one of the fixed provided more space than required. For both extra and factor
the largest value is kept.
-}
sizeReqMergeMax :: SizeReq -> SizeReq -> SizeReq
sizeReqMergeMax req1 req2 = newReq where
  isFixedReq1 = round (req1 ^. L.fixed) > 0
  isFixedReq2 = round (req2 ^. L.fixed) > 0
  flexReq1 = req1 ^. L.flex
  flexReq2 = req2 ^. L.flex
  newFixed = max (req1 ^. L.fixed) (req2 ^. L.fixed)
  newFlex
    | not (isFixedReq1 `xor` isFixedReq2) = max flexReq1 flexReq2
    | isFixedReq1 && flexReq1 > flexReq2 = flexReq1
    | isFixedReq2 && flexReq2 > flexReq1 = flexReq2
    | otherwise = max 0 $ max flexReq1 flexReq2 - newFixed
  newReq = SizeReq {
    _szrFixed = newFixed,
    _szrFlex = newFlex,
    _szrExtra = max (req1 ^. L.extra) (req2 ^. L.extra),
    _szrFactor = max (req1 ^. L.factor) (req2 ^. L.factor)
  }

modifySizeReq :: SizeReq -> (Double -> Double) -> SizeReq
modifySizeReq (SizeReq fixed flex extra factor) fn = SizeReq {
    _szrFixed = if fixed > 0 then fn fixed else 0,
    _szrFlex = if flex > 0 then fn flex else 0,
    _szrExtra = if extra > 0 then fn extra else 0,
    _szrFactor = factor
  }

doubleInRange :: Double -> Double -> Double -> Bool
doubleInRange minValue maxValue curValue = validMin && validMax where
  minDiff = curValue - minValue
  maxDiff = maxValue - curValue
  -- Some calculations may leave small differences in otherwise valid results
  validMin = minDiff >= 0 || abs minDiff < 0.0001
  validMax = maxDiff >= 0 || abs maxDiff < 0.0001
