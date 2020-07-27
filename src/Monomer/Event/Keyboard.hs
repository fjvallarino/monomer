module Monomer.Event.Keyboard (
  keyboardEvent,
  textEvent,

  keyTab,
  isKeyBackspace,
  isKeyEsc,
  isKeyReturn,
  isKeyTab,
  isKeyLeft,
  isKeyRight,
  isKeyUp,
  isKeyDown,
  isKeyC,
  isKeyV
) where

import Unsafe.Coerce

import qualified SDL

import Monomer.Event.Types

getKeycode :: SDL.Keycode -> KeyCode
getKeycode keyCode = fromIntegral $ SDL.unwrapKeycode keyCode

keyboardEvent :: SDL.EventPayload -> Maybe SystemEvent
keyboardEvent (SDL.KeyboardEvent eventData) = Just keyAction where
  keySym = SDL.keyboardEventKeysym eventData
  keyMod = convertKeyModifier $ SDL.keysymModifier keySym
  keyCode = SDL.unwrapKeycode $ SDL.keysymKeycode keySym
  keyStatus = case SDL.keyboardEventKeyMotion eventData of
    SDL.Pressed -> KeyPressed
    SDL.Released -> KeyReleased
  keyAction = KeyAction keyMod (fromIntegral keyCode) keyStatus
keyboardEvent _ = Nothing

textEvent :: SDL.EventPayload -> Maybe SystemEvent
textEvent (SDL.TextInputEvent input) = Just textInput where
  textInput = TextInput (SDL.textInputEventText input)
textEvent _ = Nothing

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

keyBackspace = getKeycode SDL.KeycodeBackspace
keyEsc = getKeycode SDL.KeycodeEscape
keyReturn = getKeycode SDL.KeycodeReturn
keyTab = getKeycode SDL.KeycodeTab

keyLeft = getKeycode SDL.KeycodeLeft
keyRight = getKeycode SDL.KeycodeRight
keyUp = getKeycode SDL.KeycodeUp
keyDown = getKeycode SDL.KeycodeDown

keyC = getKeycode SDL.KeycodeC
keyV = getKeycode SDL.KeycodeV

isKeyBackspace = (== keyBackspace)
isKeyEsc = (== keyEsc)
isKeyReturn = (== keyReturn)
isKeyTab = (== keyTab)

isKeyLeft = (== keyLeft)
isKeyRight = (== keyRight)
isKeyUp = (== keyUp)
isKeyDown = (== keyDown)
isKeyC = (== keyC)
isKeyV = (== keyV)
