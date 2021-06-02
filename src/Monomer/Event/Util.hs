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
  isButtonPressed,
  getKeyStatus,
  isShortCutControl,
  isKeyboardCopy,
  isKeyboardPaste,
  isKeyboardCut,
  isKeyboardUndo,
  isKeyboardRedo,
  isShiftPressed,
  isCtrlPressed,
  isAltPressed,
  isGUIPressed,
  isOnEnter,
  isOnLeave,
  isOnMove,
  isOnFocus,
  isOnBlur,
  isDropEvent
) where

import Data.Maybe (fromMaybe)

import qualified Data.Map as M

import Monomer.Core
import Monomer.Event.Core
import Monomer.Event.Keyboard
import Monomer.Event.Types

-- | Checks if the given button is pressed.
isButtonPressed :: InputStatus -> Button -> Bool
isButtonPressed inputStatus button = status == BtnPressed where
  currentStatus = M.lookup button (_ipsButtons inputStatus)
  status = fromMaybe BtnReleased currentStatus

-- | Gets the status of the given key.
getKeyStatus :: InputStatus -> KeyCode -> KeyStatus
getKeyStatus inputStatus code = status where
  keys = _ipsKeys inputStatus
  status = fromMaybe KeyReleased (M.lookup code keys)

-- | Checks if Ctrl/Cmd, depending on OS, is pressed.
isShortCutControl :: WidgetEnv s e -> KeyMod -> Bool
isShortCutControl wenv mod = isControl || isCommand where
  isControl = not (isMacOS wenv) && isCtrlPressed mod
  isCommand = isMacOS wenv && isGUIPressed mod

-- | Checks if a copy shortcut has been pressed.
isKeyboardCopy :: WidgetEnv s e -> SystemEvent -> Bool
isKeyboardCopy wenv event = checkKeyboard event testFn where
  testFn mod code motion = isShortCutControl wenv mod && isKeyC code

-- | Checks if a paste shortcut has been pressed.
isKeyboardPaste :: WidgetEnv s e -> SystemEvent -> Bool
isKeyboardPaste wenv event = checkKeyboard event testFn where
  testFn mod code motion = isShortCutControl wenv mod && isKeyV code

-- | Checks if a cut shortcut has been pressed.
isKeyboardCut :: WidgetEnv s e -> SystemEvent -> Bool
isKeyboardCut wenv event = checkKeyboard event testFn where
  testFn mod code motion = isShortCutControl wenv mod && isKeyX code

-- | Checks if an undo shortcut has been pressed.
isKeyboardUndo :: WidgetEnv s e -> SystemEvent -> Bool
isKeyboardUndo wenv event = checkKeyboard event testFn where
  testFn mod code motion = isShortCutControl wenv mod
    && not (_kmLeftShift mod)
    && isKeyZ code

-- | Checks if a redo shortcut has been pressed.
isKeyboardRedo :: WidgetEnv s e -> SystemEvent -> Bool
isKeyboardRedo wenv event = checkKeyboard event testFn where
  testFn mod code motion = isShortCutControl wenv mod
    && _kmLeftShift mod
    && isKeyZ code

-- | Checks if Winddows/Cmd key is pressed.
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

-- | Checks if it's a Leave event.
isOnLeave :: SystemEvent -> Bool
isOnLeave Leave{} = True
isOnLeave _ = False

-- | Checks if it's a Move event.
isOnMove :: SystemEvent -> Bool
isOnMove Move{} = True
isOnMove _ = False

-- | Checks if it's a Drop event.
isDropEvent :: SystemEvent -> Bool
isDropEvent Drop{} = True
isDropEvent _ = False

-- Helpers
checkKeyboard :: SystemEvent -> (KeyMod -> KeyCode -> KeyStatus -> Bool) -> Bool
checkKeyboard (KeyAction mod code motion) testFn = testFn mod code motion
checkKeyboard _ _ = False
