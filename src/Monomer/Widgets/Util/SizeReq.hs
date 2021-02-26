module Monomer.Widgets.Util.SizeReq (
  SizeReqUpdater(..),
  clearExtra,
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

import Control.Lens ((&), (^.), (.~))
import Data.Default
import Data.Maybe
import Data.Sequence ((|>))

import Monomer.Core
import Monomer.Event
import Monomer.Widgets.Util.Style
import Monomer.Widgets.Util.Widget

import qualified Monomer.Core.Lens as L
import Data.Bits

type SizeReqUpdater = (SizeReq, SizeReq) -> (SizeReq, SizeReq)

clearExtra :: SizeReqUpdater
clearExtra (req1, req2) = (req1 & L.extra .~ 0, req2 & L.extra .~ 0)

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
sizeReqMin req = req ^. L.fixed

sizeReqMax :: SizeReq -> Double
sizeReqMax req
  | req ^. L.extra > 0 = maxNumericValue
  | otherwise = req ^. L.fixed + req ^. L.flex

sizeReqMaxBounded :: SizeReq -> Double
sizeReqMaxBounded req = req ^. L.fixed + req ^. L.flex

sizeReqFixed :: SizeReq -> Double
sizeReqFixed req = req ^. L.fixed

sizeReqFlex :: SizeReq -> Double
sizeReqFlex req = req ^. L.flex

sizeReqExtra :: SizeReq -> Double
sizeReqExtra req = req ^. L.extra

sizeReqFactor :: SizeReq -> Double
sizeReqFactor req = req ^. L.factor

sizeReqMergeSum :: SizeReq -> SizeReq -> SizeReq
sizeReqMergeSum req1 req2 = newReq where
  newReq = SizeReq {
    _szrFixed = _szrFixed req1 + _szrFixed req2,
    _szrFlex = _szrFlex req1 + _szrFlex req2,
    _szrExtra = _szrExtra req1 + _szrExtra req2,
    _szrFactor = max (_szrFactor req1) (_szrFactor req2)
  }

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
modifySizeReq (SizeReq fixed flex extra factor) fn = def
  & L.fixed .~ (if fixed > 0 then fn fixed else 0)
  & L.flex .~ (if flex > 0 then fn flex else 0)
  & L.extra .~ (if extra > 0 then fn extra else 0)
  & L.factor .~ factor

doubleInRange :: Double -> Double -> Double -> Bool
doubleInRange minValue maxValue curValue = validMin && validMax where
  minDiff = curValue - minValue
  maxDiff = maxValue - curValue
  -- Some calculations may leave small differences in otherwise valid results
  validMin = minDiff >= 0 || abs minDiff < 0.0001
  validMax = maxDiff >= 0 || abs maxDiff < 0.0001
