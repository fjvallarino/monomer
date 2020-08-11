module Monomer.Event.Keyboard (
  keyboardEvent,
  textEvent,

  keyTab,
  isKeyBackspace,
  isKeyEsc,
  isKeyReturn,
  isKeySpace,
  isKeyTab,
  isKeyLeft,
  isKeyRight,
  isKeyUp,
  isKeyDown,
  isKeyC,
  isKeyV
) where

import qualified SDL

import Monomer.Event.Types

getKeycode :: SDL.Keycode -> KeyCode
getKeycode keyCode = KeyCode $ fromIntegral (SDL.unwrapKeycode keyCode)

keyboardEvent :: SDL.EventPayload -> Maybe SystemEvent
keyboardEvent (SDL.KeyboardEvent eventData) = Just keyAction where
  keySym = SDL.keyboardEventKeysym eventData
  keyMod = convertKeyModifier $ SDL.keysymModifier keySym
  keyCode = SDL.unwrapKeycode $ SDL.keysymKeycode keySym
  keyStatus = case SDL.keyboardEventKeyMotion eventData of
    SDL.Pressed -> KeyPressed
    SDL.Released -> KeyReleased
  keyAction = KeyAction keyMod (KeyCode $ fromIntegral keyCode) keyStatus
keyboardEvent _ = Nothing

textEvent :: SDL.EventPayload -> Maybe SystemEvent
textEvent (SDL.TextInputEvent input) = Just textInput where
  textInput = TextInput (SDL.textInputEventText input)
textEvent _ = Nothing

convertKeyModifier :: SDL.KeyModifier -> KeyMod
convertKeyModifier keyMod = KeyMod {
  _kmLeftShift = SDL.keyModifierLeftShift keyMod,
  _kmRightShift = SDL.keyModifierRightShift keyMod,
  _kmLeftCtrl = SDL.keyModifierLeftCtrl keyMod,
  _kmRightCtrl = SDL.keyModifierRightCtrl keyMod,
  _kmLeftAlt = SDL.keyModifierLeftAlt keyMod,
  _kmRightAlt = SDL.keyModifierRightAlt keyMod,
  _kmLeftGUI = SDL.keyModifierLeftGUI keyMod,
  _kmRightGUI = SDL.keyModifierRightGUI keyMod,
  _kmNumLock = SDL.keyModifierNumLock keyMod,
  _kmCapsLock = SDL.keyModifierCapsLock keyMod,
  _kmAltGr = SDL.keyModifierAltGr keyMod
}

keyBackspace :: KeyCode
keyBackspace = getKeycode SDL.KeycodeBackspace

keyEsc :: KeyCode
keyEsc = getKeycode SDL.KeycodeEscape

keyReturn :: KeyCode
keyReturn = getKeycode SDL.KeycodeReturn

keySpace :: KeyCode
keySpace = getKeycode SDL.KeycodeSpace

keyTab :: KeyCode
keyTab = getKeycode SDL.KeycodeTab

keyLeft :: KeyCode
keyLeft = getKeycode SDL.KeycodeLeft

keyRight :: KeyCode
keyRight = getKeycode SDL.KeycodeRight

keyUp :: KeyCode
keyUp = getKeycode SDL.KeycodeUp

keyDown :: KeyCode
keyDown = getKeycode SDL.KeycodeDown

keyC :: KeyCode
keyC = getKeycode SDL.KeycodeC

keyV :: KeyCode
keyV = getKeycode SDL.KeycodeV

isKeyBackspace :: KeyCode -> Bool
isKeyBackspace = (== keyBackspace)

isKeyEsc :: KeyCode -> Bool
isKeyEsc = (== keyEsc)

isKeyReturn :: KeyCode -> Bool
isKeyReturn = (== keyReturn)

isKeySpace :: KeyCode -> Bool
isKeySpace = (== keySpace)

isKeyTab :: KeyCode -> Bool
isKeyTab = (== keyTab)

isKeyLeft :: KeyCode -> Bool
isKeyLeft = (== keyLeft)

isKeyRight :: KeyCode -> Bool
isKeyRight = (== keyRight)

isKeyUp :: KeyCode -> Bool
isKeyUp = (== keyUp)

isKeyDown :: KeyCode -> Bool
isKeyDown = (== keyDown)

isKeyC :: KeyCode -> Bool
isKeyC = (== keyC)

isKeyV :: KeyCode -> Bool
isKeyV = (== keyV)
