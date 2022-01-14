{-|
Module      : Monomer.Event.Core
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Core functions for SDL event processing and conversion.
-}
module Monomer.Event.Core (
  ConvertEventsCfg(..),
  isActionEvent,
  convertEvents,
  translateEvent
) where

import Control.Applicative ((<|>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)

import qualified Data.Map.Strict as M
import qualified SDL

import Monomer.Common
import Monomer.Event.Keyboard
import Monomer.Event.Types

{-|
Checks if an SDL event is an action event. Currently only mouse and keyboard
events are considered as such (touch events should be added in the future). This
is used for triggering automatic rendering of a frame. For other events, widgets
must request rendering explicitly.
-}
isActionEvent :: SDL.EventPayload -> Bool
isActionEvent SDL.MouseButtonEvent{} = True
isActionEvent SDL.MouseWheelEvent{} = True
isActionEvent SDL.KeyboardEvent{} = True
isActionEvent SDL.TextInputEvent{} = True
isActionEvent _ = False

data ConvertEventsCfg = ConvertEventsCfg {
  _cecOs :: Text,           -- ^ The host operating system.
  _cecDpr :: Double,        -- ^ Device pixel rate.
  _cecEpr :: Double,        -- ^ Event pixel rate.
  _cecInvertWheelX :: Bool, -- ^ Whether wheel/trackpad x direction should be inverted.
  _cecInvertWheelY :: Bool  -- ^ Whether wheel/trackpad y direction should be inverted.
} deriving (Eq, Show)

-- | Converts SDL events to Monomer's SystemEvent
convertEvents
  :: ConvertEventsCfg    -- ^ Settings for event conversion.
  -> Point               -- ^ Mouse position.
  -> [SDL.EventPayload]  -- ^ List of SDL events.
  -> [SystemEvent]       -- ^ List of Monomer events.
convertEvents cfg mousePos events = catMaybes convertedEvents where
  ConvertEventsCfg os dpr epr invertX invertY = cfg
  convertedEvents = fmap convertEvent events
  convertEvent evt =
    mouseMoveEvent mousePos evt
    <|> mouseClick mousePos evt
    <|> mouseWheelEvent cfg mousePos evt
    <|> mouseMoveLeave mousePos evt
    <|> keyboardEvent evt
    <|> textEvent evt

-- | Adds a given offset to mouse related SystemEvents.
translateEvent
  :: Point        -- ^ Offset to apply
  -> SystemEvent  -- ^ Source SystemEvent
  -> SystemEvent  -- ^ Updated SystemEvent
translateEvent offset evt = case evt of
  Click p btn cl -> Click (addPoint p offset) btn cl
  ButtonAction p btn st cl -> ButtonAction (addPoint p offset) btn st cl
  WheelScroll p wxy dir -> WheelScroll (addPoint p offset) wxy dir
  Enter p -> Enter (addPoint p offset)
  Move p -> Move (addPoint p offset)
  Leave p -> Leave (addPoint p offset)
  Drag p path msg -> Drag (addPoint p offset) path msg
  Drop p path msg -> Drop (addPoint p offset) path msg
  _ -> evt

mouseClick :: Point -> SDL.EventPayload -> Maybe SystemEvent
mouseClick mousePos (SDL.MouseButtonEvent eventData) = systemEvent where
    button = case SDL.mouseButtonEventButton eventData of
      SDL.ButtonLeft -> Just BtnLeft
      SDL.ButtonRight -> Just BtnRight
      SDL.ButtonMiddle -> Just BtnMiddle
      _ -> Nothing

    action = case SDL.mouseButtonEventMotion eventData of
      SDL.Pressed -> BtnPressed
      SDL.Released -> BtnReleased

    clicks = fromIntegral $ SDL.mouseButtonEventClicks eventData
    systemEvent = fmap (\btn -> ButtonAction mousePos btn action clicks) button
mouseClick _ _ = Nothing

mouseMoveEvent :: Point -> SDL.EventPayload -> Maybe SystemEvent
mouseMoveEvent mousePos (SDL.MouseMotionEvent _) = Just $ Move mousePos
mouseMoveEvent mousePos _ = Nothing

mouseMoveLeave :: Point -> SDL.EventPayload -> Maybe SystemEvent
mouseMoveLeave mousePos SDL.WindowLostMouseFocusEvent{} = evt where
  evt = Just $ Move (Point (-1) (-1))
mouseMoveLeave mousePos _ = Nothing

mouseWheelEvent :: ConvertEventsCfg -> Point -> SDL.EventPayload -> Maybe SystemEvent
mouseWheelEvent cfg pos (SDL.MouseWheelEvent eventData) = systemEvent where
  ConvertEventsCfg os dpr epr invertX invertY = cfg
  signX = if invertX then -1 else 1
  signY = if invertY then -1 else 1
  factorX
    | os == "Windows" || os == "Mac OS X" = -signX
    | otherwise = signX
  factorY = signY
  wheelDirection = case SDL.mouseWheelEventDirection eventData of
    SDL.ScrollNormal -> WheelNormal
    SDL.ScrollFlipped -> WheelFlipped
  SDL.V2 x y = SDL.mouseWheelEventPos eventData
  wheelDelta = Point (factorX * fromIntegral x * epr) (factorY * fromIntegral y * epr)

  systemEvent = case SDL.mouseWheelEventWhich eventData of
    SDL.Mouse _ -> Just $ WheelScroll pos wheelDelta wheelDirection
    SDL.Touch -> Nothing
mouseWheelEvent cfg mousePos _ = Nothing

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
