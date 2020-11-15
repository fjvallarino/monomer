module Monomer.Event.Core (
  convertEvents
) where

import Control.Applicative ((<|>))
import Data.Maybe (catMaybes, fromMaybe)

import qualified Data.Map.Strict as M
import qualified SDL

import Monomer.Core.BasicTypes
import Monomer.Event.Keyboard
import Monomer.Event.Types

convertEvents :: Double -> Point -> [SDL.EventPayload] -> [SystemEvent]
convertEvents devicePixelRate mousePos events = catMaybes convertedEvents where
  convertedEvents = fmap convertEvent events
  convertEvent evt =
    mouseMoveEvent devicePixelRate mousePos evt
    <|> mouseClick mousePos evt
    <|> mouseWheelEvent devicePixelRate mousePos evt
    <|> mouseMoveLeave devicePixelRate mousePos evt
    <|> keyboardEvent evt
    <|> textEvent evt

mouseClick :: Point -> SDL.EventPayload -> Maybe SystemEvent
mouseClick mousePos (SDL.MouseButtonEvent eventData) = systemEvent where
    button = case SDL.mouseButtonEventButton eventData of
      SDL.ButtonLeft -> Just LeftBtn
      SDL.ButtonRight -> Just RightBtn
      _ -> Nothing
    action = case SDL.mouseButtonEventMotion eventData of
      SDL.Pressed -> PressedBtn
      SDL.Released -> ReleasedBtn
    systemEvent = fmap (\btn -> ButtonAction mousePos btn action) button
mouseClick _ _ = Nothing

mouseMoveEvent :: Double -> Point -> SDL.EventPayload -> Maybe SystemEvent
mouseMoveEvent dpr mousePos (SDL.MouseMotionEvent _) = Just $ Move mousePos
mouseMoveEvent dpr mousePos _ = Nothing

mouseMoveLeave :: Double -> Point -> SDL.EventPayload -> Maybe SystemEvent
mouseMoveLeave dpr mousePos SDL.WindowLostMouseFocusEvent{} = evt where
  evt = Just $ Move (Point (-1) (-1))
mouseMoveLeave dpr mousePos _ = Nothing

mouseWheelEvent :: Double -> Point -> SDL.EventPayload -> Maybe SystemEvent
mouseWheelEvent dpr mousePos (SDL.MouseWheelEvent eventData) = systemEvent where
  wheelDirection = case SDL.mouseWheelEventDirection eventData of
    SDL.ScrollNormal -> WheelNormal
    SDL.ScrollFlipped -> WheelFlipped
  SDL.V2 x y = SDL.mouseWheelEventPos eventData
  wheelDelta = Point (fromIntegral x * dpr) (fromIntegral y * dpr)
  systemEvent = case SDL.mouseWheelEventWhich eventData of
    SDL.Mouse _ -> Just $ WheelScroll mousePos wheelDelta wheelDirection
    SDL.Touch -> Nothing
mouseWheelEvent dpr mousePos _ = Nothing

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
