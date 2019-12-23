module GUI.Common.Keyboard where

import qualified SDL

data KeyMod = KeyMod {
  keyModLeftShift :: Bool,
  keyModRightShift :: Bool,
  keyModLeftCtrl :: Bool,
  keyModRightCtrl :: Bool,
  keyModLeftAlt :: Bool,
  keyModRightAlt :: Bool,
  keyModLeftGUI :: Bool,
  keyModRightGUI :: Bool,
  keyModNumLock :: Bool,
  keyModCapsLock :: Bool,
  keyModAltGr :: Bool
} deriving (Show, Eq)

type KeyCode = Int

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

convertKeyModifier :: SDL.KeyModifier -> KeyMod
convertKeyModifier keyMod = KeyMod {
  keyModLeftShift = SDL.keyModifierLeftShift keyMod,
  keyModRightShift = SDL.keyModifierRightShift keyMod,
  keyModLeftCtrl = SDL.keyModifierLeftCtrl keyMod,
  keyModRightCtrl = SDL.keyModifierRightCtrl keyMod,
  keyModLeftAlt = SDL.keyModifierLeftAlt keyMod,
  keyModRightAlt = SDL.keyModifierRightAlt keyMod,
  keyModLeftGUI = SDL.keyModifierLeftGUI keyMod,
  keyModRightGUI = SDL.keyModifierRightGUI keyMod,
  keyModNumLock = SDL.keyModifierNumLock keyMod,
  keyModCapsLock = SDL.keyModifierCapsLock keyMod,
  keyModAltGr = SDL.keyModifierAltGr keyMod
}

getKeycode :: SDL.Keycode -> KeyCode
getKeycode keyCode = fromIntegral $ SDL.unwrapKeycode keyCode

keyBackspace = getKeycode SDL.KeycodeBackspace
keyLeft = getKeycode SDL.KeycodeLeft
keyRight = getKeycode SDL.KeycodeRight
keyUp = getKeycode SDL.KeycodeUp
keyDown = getKeycode SDL.KeycodeDown

keyC = getKeycode SDL.KeycodeC
keyV = getKeycode SDL.KeycodeV

isKeyBackspace = (== keyBackspace)
isKeyLeft = (== keyLeft)
isKeyRight = (== keyRight)
isKeyC = (== keyC)
isKeyV = (== keyV)
