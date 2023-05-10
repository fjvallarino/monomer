{-|
Module      : Monomer.Event.Util
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Utility functions for event handling.
-}
module Monomer.Event.Util (
  isShiftPressed,
  isCtrlPressed,
  isAltPressed,
  isGUIPressed,
  isOnClick,
  isOnButtonAction,
  isOnWheelScroll,
  isOnKeyAction,
  isOnTextInput,
  isOnClipboard,
  isOnFocus,
  isOnBlur,
  isOnEnter,
  isOnMove,
  isOnLeave,
  isOnDrag,
  isOnDrop,
  checkKeyboard
) where

import Monomer.Event.Types

-- | Checks if Windows/Cmd key is pressed.
isGUIPressed :: KeyMod -> Bool
isGUIPressed mod = _kmLeftGUI mod || _kmRightGUI mod

-- | Checks if Ctrl key is pressed.
isCtrlPressed :: KeyMod -> Bool
isCtrlPressed keyMod = _kmLeftCtrl keyMod || _kmRightCtrl keyMod

-- | Checks if Shift key is pressed.
isShiftPressed :: KeyMod -> Bool
isShiftPressed keyMod = _kmLeftShift keyMod || _kmRightShift keyMod

-- | Checks if Alt key is pressed.
isAltPressed :: KeyMod -> Bool
isAltPressed keyMod = _kmLeftAlt keyMod || _kmRightAlt keyMod

-- | Checks if it's a Click event.
isOnClick :: SystemEvent -> Bool
isOnClick Click{} = True
isOnClick _ = False

-- | Checks if it's a ButtonAction event.
isOnButtonAction :: SystemEvent -> Bool
isOnButtonAction ButtonAction{} = True
isOnButtonAction _ = False

-- | Checks if it's a WheelScroll event.
isOnWheelScroll :: SystemEvent -> Bool
isOnWheelScroll WheelScroll{} = True
isOnWheelScroll _ = False

-- | Checks if it's a KeyAction event.
isOnKeyAction :: SystemEvent -> Bool
isOnKeyAction KeyAction{} = True
isOnKeyAction _ = False

-- | Checks if it's a TextInput event.
isOnTextInput :: SystemEvent -> Bool
isOnTextInput TextInput{} = True
isOnTextInput _ = False

-- | Checks if it's a Clipboard event.
isOnClipboard :: SystemEvent -> Bool
isOnClipboard Clipboard{} = True
isOnClipboard _ = False

-- | Checks if it's a Focus event.
isOnFocus :: SystemEvent -> Bool
isOnFocus Focus{} = True
isOnFocus _ = False

-- | Checks if it's a Blur event.
isOnBlur :: SystemEvent -> Bool
isOnBlur Blur{} = True
isOnBlur _ = False

-- | Checks if it's an Enter event.
isOnEnter :: SystemEvent -> Bool
isOnEnter Enter{} = True
isOnEnter _ = False

-- | Checks if it's a Move event.
isOnMove :: SystemEvent -> Bool
isOnMove Move{} = True
isOnMove _ = False

-- | Checks if it's a Leave event.
isOnLeave :: SystemEvent -> Bool
isOnLeave Leave{} = True
isOnLeave _ = False

-- | Checks if it's a Drag event.
isOnDrag :: SystemEvent -> Bool
isOnDrag Drag{} = True
isOnDrag _ = False

-- | Checks if it's a Drop event.
isOnDrop :: SystemEvent -> Bool
isOnDrop Drop{} = True
isOnDrop _ = False

-- | Applies a provided function to test a KeyAction event
checkKeyboard :: SystemEvent -> (KeyMod -> KeyCode -> KeyStatus -> Bool) -> Bool
checkKeyboard (KeyAction mod code motion) testFn = testFn mod code motion
checkKeyboard _ _ = False
