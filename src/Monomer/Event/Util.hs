module Monomer.Event.Util where

import Data.Default
import Data.Maybe

import qualified Data.Map.Strict as M

import Monomer.Event.Types

defKeyMod :: KeyMod
defKeyMod = KeyMod {
  kmLeftShift = False,
  kmRightShift = False,
  kmLeftCtrl = False,
  kmRightCtrl = False,
  kmLeftAlt = False,
  kmRightAlt = False,
  kmLeftGUI = False,
  kmRightGUI = False,
  kmNumLock = False,
  kmCapsLock = False,
  kmAltGr = False
}

defInputStatus :: InputStatus
defInputStatus = InputStatus {
  ipsMousePos = def,
  ipsKeyMod = defKeyMod,
  ipsKeys = M.empty,
  ipsButtons = M.empty
}

isButtonPressed :: InputStatus -> Button -> Bool
isButtonPressed inputStatus button = status == PressedBtn where
  currentStatus = M.lookup button (ipsButtons inputStatus)
  status = fromMaybe ReleasedBtn currentStatus
