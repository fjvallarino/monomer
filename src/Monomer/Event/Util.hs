module Monomer.Event.Util where

import Data.Default
import Data.Maybe

import qualified Data.Map.Strict as M

import Monomer.Event.Types

defKeyMod :: KeyMod
defKeyMod = KeyMod {
  keyModLeftShift = False,
  keyModRightShift = False,
  keyModLeftCtrl = False,
  keyModRightCtrl = False,
  keyModLeftAlt = False,
  keyModRightAlt = False,
  keyModLeftGUI = False,
  keyModRightGUI = False,
  keyModNumLock = False,
  keyModCapsLock = False,
  keyModAltGr = False
}

defInputStatus :: InputStatus
defInputStatus = InputStatus {
  statusMousePos = def,
  statusKeyMod = defKeyMod,
  statusKeys = M.empty,
  statusButtons = M.empty
}

isButtonPressed :: InputStatus -> Button -> Bool
isButtonPressed inputStatus button = status == PressedBtn where
  currentStatus = M.lookup button (statusButtons inputStatus)
  status = fromMaybe ReleasedBtn currentStatus
