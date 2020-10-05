module Monomer.Widget.Internal (
  addOuterSizeReq,
  handleSizeReqStyle,
  handleStyleChange,
  isFixedReq,
  isFlexReq,
  isBoundedReq,
  getMaxReqCoord,
  getReqCoord,
  getReqFactor,
  getReqFactored,
  modifyReqCoord
) where

import Data.Default
import Data.Maybe
import Data.Sequence ((|>))

import Monomer.Common.BasicTypes
import Monomer.Common.Style
import Monomer.Common.StyleUtil
import Monomer.Event.Types
import Monomer.Event.Core
import Monomer.Widget.Types
import Monomer.Widget.Util

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
  hResult = handler wenv target evt inst
  result = fromMaybe (resultWidget inst) hResult
  checkSize = or $ fmap ($ evt) [isOnFocus, isOnBlur, isOnEnter, isOnLeave]
  instReqs = widgetUpdateSizeReq (_wiWidget inst) wenv inst
  oldSizeReqW = _wiSizeReqW inst
  oldSizeReqH = _wiSizeReqH inst
  newSizeReqW = _wiSizeReqW instReqs
  newSizeReqH = _wiSizeReqH instReqs
  sizeReqChanged = oldSizeReqW /= newSizeReqW || oldSizeReqH /= newSizeReqH
  newResult
    | checkSize && sizeReqChanged = Just result {
        _wrRequests = _wrRequests result |> Resize
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
  newReqW = modifyReqCoord reqW (+w)
  newReqH = modifyReqCoord reqH (+h)

isFixedReq :: SizeReq -> Bool
isFixedReq FixedSize{} = True
isFixedReq _ = False

isFlexReq :: SizeReq -> Bool
isFlexReq FlexSize{} = True
isFlexReq _ = False

isBoundedReq :: SizeReq -> Bool
isBoundedReq BoundedSize{} = True
isBoundedReq _ = False

getReqCoord :: SizeReq -> Coord
getReqCoord (FixedSize c) = c
getReqCoord (FlexSize c _) = c
getReqCoord (BoundedSize c1 c2 _) = c1

getMaxReqCoord :: SizeReq -> Coord
getMaxReqCoord (FixedSize c) = c
getMaxReqCoord (FlexSize c _) = c
getMaxReqCoord (BoundedSize c1 c2 _) = c2

getReqFactor :: SizeReq -> Factor
getReqFactor (FixedSize _) = 1
getReqFactor (FlexSize _ f) = f
getReqFactor (BoundedSize _ _ f) = f

getReqFactored :: SizeReq -> Coord
getReqFactored req = getReqFactor req * getReqCoord req

modifyReqCoord :: SizeReq -> (Coord -> Coord) -> SizeReq
modifyReqCoord (FixedSize c) f = FixedSize (f c)
modifyReqCoord (FlexSize c factor) f = FlexSize (f c) factor
modifyReqCoord (BoundedSize c1 c2 factor) f = BoundedSize (f c1) (f c2) factor
