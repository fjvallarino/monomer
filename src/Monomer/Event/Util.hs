module Monomer.Event.Util where

import qualified Data.Map.Strict as M

import Monomer.Event.Types

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

defInputStatus = InputStatus {
  statusKeyMod = defKeyMod,
  statusKeys = M.empty,
  statusButtons = M.empty
}
