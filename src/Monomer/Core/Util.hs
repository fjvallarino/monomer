module Monomer.Core.Util where

import Control.Lens ((&), (^.), (.~), (?~))
import Data.Text (Text)

import Monomer.Core.BasicTypes
import Monomer.Core.Style
import Monomer.Core.WidgetTypes

import qualified Monomer.Lens as L

createMoveFocusReq :: WidgetEnv s e -> WidgetRequest s
createMoveFocusReq wenv = MoveFocus direction where
  direction
    | wenv ^. L.inputStatus . L.keyMod . L.leftShift = FocusBwd
    | otherwise = FocusFwd

widgetTreeDesc :: Int -> WidgetInstance s e -> String
widgetTreeDesc level inst = desc where
  desc = instanceDesc level inst ++ "\n" ++ childDesc
  childDesc = foldMap (widgetTreeDesc (level + 1)) (_wiChildren inst)

instanceDesc :: Int -> WidgetInstance s e -> String
instanceDesc level inst = instDesc inst where
  spaces = replicate (level * 2) ' '
  instDesc i =
    spaces ++ "type: " ++ unWidgetType (_wiWidgetType inst) ++ "\n" ++
    spaces ++ "vp: " ++ rectDesc (_wiViewport inst) ++ "\n" ++
    spaces ++ "req: " ++ show (_wiSizeReqW inst, _wiSizeReqH inst) ++ "\n"
  rectDesc r = show (_rX r, _rY r, _rW r, _rH r)
