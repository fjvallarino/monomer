module Monomer.Widget.Internal (
  handleStyleChange
) where

import Data.Maybe
import Data.Sequence ((|>))

import Monomer.Common.Tree
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
