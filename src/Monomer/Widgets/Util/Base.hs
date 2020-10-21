module Monomer.Widgets.Util.Base (
  handleSizeReqStyle,
  handleStyleChange,
  isFixedSizeReq,
  isFlexSizeReq,
  isBoundedSizeReq,
  getMinSizeReq,
  getMaxSizeReq,
  getFactorReq,
  modifySizeReq
) where

import Data.Default
import Data.Maybe
import Data.Sequence ((|>))

import Monomer.Core
import Monomer.Event
import Monomer.Widgets.Util.Style
import Monomer.Widgets.Util.Widget

import qualified Data.Sequence as Seq

type EventHandler s e
  = WidgetEnv s e
  -> Path
  -> SystemEvent
  -> WidgetInstance s e
  -> Maybe (WidgetResult s e)

handleStyleChange
  :: EventHandler s e
  -> WidgetEnv s e
  -> Path
  -> SystemEvent
  -> WidgetInstance s e
  -> Maybe (WidgetResult s e)
handleStyleChange handler wenv target evt inst = newResult where
  style = instanceStyle wenv inst
  hResult
    | _wiEnabled inst = handler wenv target evt inst
    | otherwise = Nothing
  result = fromMaybe (resultWidget inst) hResult
  -- Size
  checkSize = or $ fmap ($ evt) [isOnFocus, isOnBlur, isOnEnter, isOnLeave]
  instReqs = widgetUpdateSizeReq (_wiWidget inst) wenv inst
  oldSizeReqW = _wiSizeReqW inst
  oldSizeReqH = _wiSizeReqH inst
  newSizeReqW = _wiSizeReqW instReqs
  newSizeReqH = _wiSizeReqH instReqs
  sizeReqChanged = oldSizeReqW /= newSizeReqW || oldSizeReqH /= newSizeReqH
  -- Cursor
  setCursor = isOnEnter evt
  cursorIcon = fromMaybe CursorArrow (_sstCursorIcon style)
  -- Result
  reqs = [ Resize | checkSize && sizeReqChanged ]
    ++ [ SetCursorIcon cursorIcon | setCursor ]
  newResult
    | not (null reqs) = Just result {
        _wrRequests = _wrRequests result <> Seq.fromList reqs
      }
    | otherwise = hResult

handleSizeReqStyle :: StyleState -> (SizeReq, SizeReq) -> (SizeReq, SizeReq)
handleSizeReqStyle style (reqW, reqH) = (newReqW, newReqH) where
  realReqW = fromMaybe reqW (_sstSizeReqW style)
  realReqH = fromMaybe reqH (_sstSizeReqH style)
  (newReqW, newReqH) = addOuterSizeReq style (realReqW, realReqH)

addOuterSizeReq :: StyleState -> (SizeReq, SizeReq) -> (SizeReq, SizeReq)
addOuterSizeReq style (reqW, reqH) = (newReqW, newReqH) where
  Size w h = addOuterSize style def
  newReqW = modifySizeReq reqW (+w)
  newReqH = modifySizeReq reqH (+h)

isFixedSizeReq :: SizeReq -> Bool
isFixedSizeReq FixedSize{} = True
isFixedSizeReq _ = False

isFlexSizeReq :: SizeReq -> Bool
isFlexSizeReq FlexSize{} = True
isFlexSizeReq _ = False

isBoundedSizeReq :: SizeReq -> Bool
isBoundedSizeReq RangeSize{} = True
isBoundedSizeReq _ = False

getMinSizeReq :: SizeReq -> Double
getMinSizeReq (FixedSize c) = c
getMinSizeReq (FlexSize c _) = c
getMinSizeReq (RangeSize c1 c2 _) = c1

getMaxSizeReq :: SizeReq -> Double
getMaxSizeReq (FixedSize c) = c
getMaxSizeReq (FlexSize c _) = c
getMaxSizeReq (RangeSize c1 c2 _) = c2

getFactorReq :: SizeReq -> Factor
getFactorReq (FixedSize _) = 1
getFactorReq (FlexSize _ f) = f
getFactorReq (RangeSize _ _ f) = f

modifySizeReq :: SizeReq -> (Double -> Double) -> SizeReq
modifySizeReq (FixedSize c) f = FixedSize (f c)
modifySizeReq (FlexSize c factor) f = FlexSize (f c) factor
modifySizeReq (RangeSize c1 c2 factor) f = RangeSize (f c1) (f c2) factor
