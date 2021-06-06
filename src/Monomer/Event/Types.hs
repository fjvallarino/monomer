{-|
Module      : Monomer.Event.Types
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Basic types for Monomer events.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module Monomer.Event.Types where

import Data.Default
import Data.Text (Text)
import Data.Typeable (Typeable, cast, typeOf)
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as M

import Monomer.Common

-- | Keycode for keyboard events. Used instead of Scancodes to avoid mappings.
newtype KeyCode
  = KeyCode { unKeyCode :: Int }
  deriving (Eq, Ord, Show)

-- | Status of a keyboard key.
data KeyStatus
  = KeyPressed
  | KeyReleased
  deriving (Eq, Show)

-- | Button of a pointer device (mouse).
data Button
  = BtnLeft
  | BtnMiddle
  | BtnRight
  deriving (Eq, Show, Ord)

-- | Status of a mouse button.
data ButtonState
  = BtnPressed
  | BtnReleased
  deriving (Eq, Show)

-- | Movement direction in which wheel values are positive.
data WheelDirection
  = WheelNormal
  | WheelFlipped
  deriving (Eq, Show)

-- | Types of clipboard content.
data ClipboardData
  = ClipboardEmpty
  | ClipboardText Text
  deriving (Eq, Show)

-- | Constraints for drag event messages.
type DragMsg i = (Eq i, Typeable i)

-- | Drag message container.
data WidgetDragMsg
  = forall i . DragMsg i => WidgetDragMsg i

instance Eq WidgetDragMsg where
  WidgetDragMsg d1 == WidgetDragMsg d2 = case cast d1 of
    Just d -> d == d2
    _ -> False

instance Show WidgetDragMsg where
  show (WidgetDragMsg info) = "WidgetDragMsg: " ++ show (typeOf info)

-- | Supported Monomer SystemEvents
data SystemEvent
  -- | Click (press and release) of a mouse button. Includes mouse position.
  = Click Point Button
  -- | Double click of a mouse button. Includes mouse position.
  | DblClick Point Button
  -- | Click or release of a mouse button. Includes times pressed/released.
  -- | Includes mouse position.
  | ButtonAction Point Button ButtonState Int
  -- | Mouse wheel movement. Includes mouse position, move size in both axes and
  -- | wheel direction.
  | WheelScroll Point Point WheelDirection
  -- | Keyboard key action. Includes modifiers, keyCode and pressed/released.
  -- | This event should not be used for text input.
  | KeyAction KeyMod KeyCode KeyStatus
  -- | Processed keyboard events. Some Unicode characters require several key
  -- | presses to produce the result. This event provides the final result.
  | TextInput Text
  -- | Provides current clipboard contents to a requesting widget.
  | Clipboard ClipboardData
  -- | Target now has focus. Includes path of the previously focused widget.
  | Focus Path
  -- | Target has lost focus. Includes path of the next focused widget.
  | Blur Path
  -- | Mouse has entered the assigned viewport.
  | Enter Point
  -- | Mouse has moved inside the assigned viewport. This event keeps being
  -- | received if the main mouse button is pressed, even if the mouse is
  -- | outside the assigned bounds or even the screen.
  | Move Point
  -- | Mouse has left the assigned viewport. This event is not received until
  -- | the main mouse button has been pressed.
  | Leave Point
  -- | A drag action is active and the mouse is inside the current viewport. The
  -- | messsage can be used to decide if it applies to the current widget. This
  -- | event is not received by the widget which initiated the drag action.
  | Drag Point Path WidgetDragMsg
  -- | A drag action was active and the main button was released inside the
  -- | current viewport.
  | Drop Point Path WidgetDragMsg
  deriving (Eq, Show)

-- | Status of input devices.
data InputStatus = InputStatus {
  -- | Mouse position.
  _ipsMousePos :: Point,
  -- | Previous mouse position.
  _ipsMousePosPrev :: Point,
  -- | Current key modifiers (shift, ctrl, alt, etc).
  _ipsKeyMod :: KeyMod,
  -- | Current status of keyCodes. If not in the map, status is KeyReleased.
  _ipsKeys :: Map KeyCode KeyStatus,
  -- | Status of mouse buttons. If not in the map, status is BtnReleased.
  _ipsButtons :: Map Button ButtonState
} deriving (Eq, Show)

instance Default InputStatus where
  def = InputStatus {
    _ipsMousePos = Point (-1) (-1),
    _ipsMousePosPrev = Point (-1) (-1),
    _ipsKeyMod = def,
    _ipsKeys = M.empty,
    _ipsButtons = M.empty
  }

{-|
Keyboard modifiers. True indicates the key is pressed.
Note: The __fn__ function in Macs cannot be detected individually.
-}
data KeyMod = KeyMod {
  _kmLeftShift :: Bool,
  _kmRightShift :: Bool,
  _kmLeftCtrl :: Bool,
  _kmRightCtrl :: Bool,
  _kmLeftAlt :: Bool,
  _kmRightAlt :: Bool,
  _kmLeftGUI :: Bool,
  _kmRightGUI :: Bool,
  _kmNumLock :: Bool,
  _kmCapsLock :: Bool,
  _kmAltGr :: Bool
} deriving (Eq, Show)

instance Default KeyMod where
  def = KeyMod {
    _kmLeftShift = False,
    _kmRightShift = False,
    _kmLeftCtrl = False,
    _kmRightCtrl = False,
    _kmLeftAlt = False,
    _kmRightAlt = False,
    _kmLeftGUI = False,
    _kmRightGUI = False,
    _kmNumLock = False,
    _kmCapsLock = False,
    _kmAltGr = False
  }
