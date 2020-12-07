module Monomer.Core.Util where

import Control.Lens ((&), (^.), (.~), (?~))
import Data.Text (Text)

import Monomer.Core.BasicTypes
import Monomer.Core.Style
import Monomer.Core.WidgetTypes

import qualified Monomer.Lens as L

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
