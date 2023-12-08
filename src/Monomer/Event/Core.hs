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

--import Control.Applicative ((<|>))
import Control.Monad (forM)
import Data.Maybe (catMaybes)
import Data.Text (Text)

import qualified SDL

-- 'OsPath' is not part of current LTS snapshot, yet.
--import System.IO (utf8, utf16le, EncodingException(..))
--import System.OsPath
--import System.OsString.Internal (encodeWith)
import System.FilePath
import Foreign.C.String

import Monomer.Common
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
isActionEvent SDL.TextEditingEvent{} = True
isActionEvent SDL.TextInputEvent{} = True
isActionEvent SDL.DropEvent{} = True
isActionEvent _ = False

-- | Configuration options for converting from an SDL event to a 'SystemEvent'.
data ConvertEventsCfg = ConvertEventsCfg {
  _cecOs :: Text,           -- ^ The host operating system.
  _cecDpr :: Double,        -- ^ Device pixel rate.
  _cecEpr :: Double,        -- ^ Event pixel rate.
  _cecInvertWheelX :: Bool, -- ^ Whether wheel/trackpad x direction should be inverted.
  _cecInvertWheelY :: Bool  -- ^ Whether wheel/trackpad y direction should be inverted.
} deriving (Eq, Show)

-- | Converts SDL events to Monomer's SystemEvent. 
--   IO necessary for marshalling (i.e. peekCString)
convertEvents
  :: ConvertEventsCfg    -- ^ Settings for event conversion.
  -> Point               -- ^ Mouse position.
  -> [SDL.EventPayload]  -- ^ List of SDL events.
  -> IO [SystemEvent]    -- ^ List of Monomer events.
convertEvents cfg mousePos events = 
  fmap catMaybes $ forM events $ eventHandler [ mouseMove, mouseClk, mouseWheel, mouseLeave, keyboard, text, fileDrop ] 
  where
    ConvertEventsCfg os dpr epr invertX invertY = cfg
    mouseMove  = \e -> return $ mouseMoveEvent mousePos e
    mouseClk   = \e -> return $ mouseClick mousePos e
    mouseWheel = \e -> return $ mouseWheelEvent cfg mousePos e
    mouseLeave = \e -> return $ mouseMoveLeave mousePos e
    keyboard   = \e -> return $ keyboardEvent e
    text       = \e -> return $ textEvent e
    fileDrop   = \e -> fileDropEvent mousePos e

    eventHandler (f:fs) = \e -> f e >>= \maybeSE -> case maybeSE of
      Just se  -> return $ Just se
      Nothing  -> eventHandler fs e
    eventHandler [] = \e -> return Nothing


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

-- TODO: 'OsPath' instead of 'FilePath' should be used, but is currently not part of the newest LTS snapshot
--fileDropEvent :: Point -> SDL.EventPayload -> IO (Maybe SystemEvent)
--fileDropEvent _ (SDL.DropEvent (SDL.DropEventData cstr)) = 
--        peekCString cstr >>= \str -> case encodeWith utf8 utf16le str of
--            Right osstr -> return $ Just (FileDrop osstr)
--            _           -> return Nothing
fileDropEvent :: Point -> SDL.EventPayload -> IO (Maybe SystemEvent)
fileDropEvent _ (SDL.DropEvent (SDL.DropEventData cstr)) = do
    str <- peekCString cstr -- the filepath 'cstr' must be an utf8 string when we compare to the SDL example https://wiki.libsdl.org/SDL2/SDL_DropEvent
    putStrLn $ "FileDrop: " ++ str
    return $ Just (FileDrop str)
fileDropEvent _ _ = return Nothing

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
