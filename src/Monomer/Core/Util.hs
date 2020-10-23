module Monomer.Core.Util where

import Control.Lens ((&), (^.), (.~), (?~))
import Data.Text (Text)

import Monomer.Core.Style
import Monomer.Core.WidgetTypes

import qualified Monomer.Lens as L

createMoveFocusReq :: WidgetEnv s e -> WidgetRequest s
createMoveFocusReq wenv = MoveFocus direction where
  direction
    | wenv ^. L.inputStatus . L.keyMod . L.leftShift = FocusBwd
    | otherwise = FocusFwd
