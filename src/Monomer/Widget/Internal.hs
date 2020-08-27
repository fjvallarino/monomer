module Monomer.Widget.Internal (
  handleStyleChange,
  handleSizeReqStyle
) where

import Data.Maybe
import Data.Sequence ((|>))

import Monomer.Common.Geometry
import Monomer.Common.Tree
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
  oldSizeReq = _wiSizeReq inst
  newSizeReq = _wiSizeReq instReqs
  sizeReqChanged = oldSizeReq /= newSizeReq
  newResult
    | checkSize && sizeReqChanged = Just result {
        _wrRequests = _wrRequests result |> Resize
      }
    | otherwise = hResult

handleSizeReqStyle :: StyleState -> SizeReq -> SizeReq
handleSizeReqStyle style sizeReq = newReq where
  outerSize = addOuterSize style (_srSize sizeReq)
  tempReq = sizeReq { _srSize = outerSize }
  newReq = handleFixedW style $ handleFixedH style tempReq

handleFixedW :: StyleState -> SizeReq -> SizeReq
handleFixedW style req
  | isJust (_sstWidth style) = SizeReq (Size newW h) StrictSize pH
  | otherwise = req
  where
    SizeReq (Size w h) pW pH = req
    newW = fromJust $ _sstWidth style

handleFixedH :: StyleState -> SizeReq -> SizeReq
handleFixedH style req
  | isJust (_sstHeight style) = SizeReq (Size w newH) pW StrictSize
  | otherwise = req
  where
    SizeReq (Size w h) pW pH = req
    newH = fromJust $ _sstHeight style
