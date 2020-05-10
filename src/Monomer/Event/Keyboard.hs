module Monomer.Event.Keyboard where

import Unsafe.Coerce

import qualified SDL

import Monomer.Event.Types

getKeycode :: SDL.Keycode -> KeyCode
getKeycode keyCode = fromIntegral $ SDL.unwrapKeycode keyCode

keyboardEvent :: [SDL.EventPayload] -> [SystemEvent]
keyboardEvent events = activeKeys
  where
    activeKeys = map (\(SDL.KeyboardEvent k) -> KeyAction (keyMod k) (keyCode k) (keyStatus k)) (unsafeCoerce keyboardEvents)
    keyMod event = convertKeyModifier $ SDL.keysymModifier $ SDL.keyboardEventKeysym event
    keyCode event = fromIntegral $ SDL.unwrapKeycode $ SDL.keysymKeycode $ SDL.keyboardEventKeysym event
    keyStatus event = if SDL.keyboardEventKeyMotion event == SDL.Pressed then KeyPressed else KeyReleased
    keyboardEvents = filter (\e -> case e of
                                      SDL.KeyboardEvent k -> True
                                      _ -> False) events

textEvent :: [SDL.EventPayload] -> [SystemEvent]
textEvent events = inputText
  where
    inputText = map (\(SDL.TextInputEvent t) -> TextInput (SDL.textInputEventText t)) (unsafeCoerce inputTextEvents)
    inputTextEvents = filter (\e -> case e of
                                      SDL.TextInputEvent _ -> True
                                      _ -> False) events

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
keyLeft = getKeycode SDL.KeycodeLeft
keyRight = getKeycode SDL.KeycodeRight
keyUp = getKeycode SDL.KeycodeUp
keyDown = getKeycode SDL.KeycodeDown
keyTab = getKeycode SDL.KeycodeTab

keyC = getKeycode SDL.KeycodeC
keyV = getKeycode SDL.KeycodeV

isKeyBackspace = (== keyBackspace)
isKeyLeft = (== keyLeft)
isKeyRight = (== keyRight)
isKeyC = (== keyC)
isKeyV = (== keyV)
