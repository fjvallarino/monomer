module Monomer.Core.Util where

import Control.Lens ((&), (^.), (.~), (?~))
import Data.Default
import Data.Maybe
import Data.Sequence (Seq)
import Data.Text (Text)

import qualified Data.Sequence as Seq

import Monomer.Core.BasicTypes
import Monomer.Core.Style
import Monomer.Core.WidgetTypes

import qualified Monomer.Lens as L

mergeWidgetResult :: WidgetNode s e -> WidgetResult s e -> WidgetResultNode s e
mergeWidgetResult node result = WidgetResultNode newNode requests events where
  newStyle = fromMaybe (node ^. L.widgetInstance . L.style) (result ^. L.style)
  newNode = node
    & L.widget .~ fromMaybe (node ^. L.widget) (result ^. L.widget)
    & L.widgetInstance . L.style .~ newStyle
    & L.children .~ fromMaybe (node ^. L.children) (result ^. L.children)
  requests = result ^. L.requests
  events = result ^. L.events

toWidgetResult :: WidgetResultNode s e -> WidgetResult s e
toWidgetResult merged = result where
  result = def
    & L.widget ?~ merged ^. L.widgetNode . L.widget
    & L.children ?~ merged ^. L.widgetNode . L.children
    & L.style ?~ merged ^. L.widgetNode . L.widgetInstance . L.style
    & L.requests .~ merged ^. L.requests
    & L.events .~ merged ^. L.events

resultWidget :: Widget s e -> WidgetResult s e
resultWidget widget = def
  & L.widget ?~ widget

resultEvts :: [e] -> WidgetResult s e
resultEvts events = def
  & L.events .~ Seq.fromList events

resultReqs :: [WidgetRequest s] -> WidgetResult s e
resultReqs requests = def
  & L.requests .~ Seq.fromList requests

resultReqsEvts :: [WidgetRequest s] -> [e] -> WidgetResult s e
resultReqsEvts requests events = def
  & L.events .~ Seq.fromList events
  & L.requests .~ Seq.fromList requests

isMacOS :: WidgetEnv s e -> Bool
isMacOS wenv = _weOS wenv == "Mac OS X"

widgetTreeDesc :: Int -> WidgetNode s e -> String
widgetTreeDesc level node = desc where
  desc = instanceDesc level node ++ "\n" ++ childDesc
  childDesc = foldMap (widgetTreeDesc (level + 1)) (_wnChildren node)

instanceDesc :: Int -> WidgetNode s e -> String
instanceDesc level node = instDesc (_wnWidgetInstance node) where
  spaces = replicate (level * 2) ' '
  instDesc inst =
    spaces ++ "type: " ++ unWidgetType (_wiWidgetType inst) ++ "\n" ++
    spaces ++ "path: " ++ show (_wiPath inst) ++ "\n" ++
    spaces ++ "vp: " ++ rectDesc (_wiViewport inst) ++ "\n" ++
    spaces ++ "req: " ++ show (_wiSizeReqW inst, _wiSizeReqH inst) ++ "\n"
  rectDesc r = show (_rX r, _rY r, _rW r, _rH r)

maxNumericValue :: (RealFloat a) => a
maxNumericValue = x where
  n = floatDigits x
  b = floatRadix x
  (_, u) = floatRange x
  x = encodeFloat (b^n - 1) (u - n)

numberInBounds :: (Ord a, Num a) => Maybe a -> Maybe a -> a -> Bool
numberInBounds Nothing Nothing _ = True
numberInBounds (Just minVal) Nothing val = val >= minVal
numberInBounds Nothing (Just maxVal) val = val <= maxVal
numberInBounds (Just minVal) (Just maxVal) val = val >= minVal && val <= maxVal
