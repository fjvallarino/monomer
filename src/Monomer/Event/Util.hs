module Monomer.Event.Util where

import Data.Maybe (fromMaybe)

import qualified Data.Map as M

import Monomer.Core
import Monomer.Event.Core
import Monomer.Event.Keyboard
import Monomer.Event.Types

isButtonPressed :: InputStatus -> Button -> Bool
isButtonPressed inputStatus button = status == PressedBtn where
  currentStatus = M.lookup button (_ipsButtons inputStatus)
  status = fromMaybe ReleasedBtn currentStatus

getKeyStatus :: InputStatus -> KeyCode -> KeyStatus
getKeyStatus inputStatus code = status where
  keys = _ipsKeys inputStatus
  status = fromMaybe KeyReleased (M.lookup code keys)

isShortCutControl :: WidgetEnv s e -> KeyMod -> Bool
isShortCutControl wenv mod = isControl || isCommand where
  isControl = not (isMacOS wenv) && isCtrlPressed mod
  isCommand = isMacOS wenv && isGUIPressed mod

isKeyboardCopy :: WidgetEnv s e -> SystemEvent -> Bool
isKeyboardCopy wenv event = checkKeyboard event testFn where
  testFn mod code motion = isShortCutControl wenv mod && isKeyC code

isKeyboardPaste :: WidgetEnv s e -> SystemEvent -> Bool
isKeyboardPaste wenv event = checkKeyboard event testFn where
  testFn mod code motion = isShortCutControl wenv mod && isKeyV code

isKeyboardCut :: WidgetEnv s e -> SystemEvent -> Bool
isKeyboardCut wenv event = checkKeyboard event testFn where
  testFn mod code motion = isShortCutControl wenv mod && isKeyX code

isKeyboardUndo :: WidgetEnv s e -> SystemEvent -> Bool
isKeyboardUndo wenv event = checkKeyboard event testFn where
  testFn mod code motion = isShortCutControl wenv mod
    && not (_kmLeftShift mod)
    && isKeyZ code

isKeyboardRedo :: WidgetEnv s e -> SystemEvent -> Bool
isKeyboardRedo wenv event = checkKeyboard event testFn where
  testFn mod code motion = isShortCutControl wenv mod
    && _kmLeftShift mod
    && isKeyZ code

checkKeyboard :: SystemEvent -> (KeyMod -> KeyCode -> KeyStatus -> Bool) -> Bool
checkKeyboard (KeyAction mod code motion) testFn = testFn mod code motion
checkKeyboard _ _ = False

isButtonPressedEvent :: SystemEvent -> Bool
isButtonPressedEvent (ButtonAction _ _ PressedBtn _) = True
isButtonPressedEvent _ = False

isButtonReleasedEvent :: SystemEvent -> Bool
isButtonReleasedEvent (ButtonAction _ _ ReleasedBtn _) = True
isButtonReleasedEvent _ = False

isDropEvent :: SystemEvent -> Bool
isDropEvent Drop{} = True
isDropEvent _ = False

isKeyboardEvent :: SystemEvent -> Bool
isKeyboardEvent KeyAction{} = True
isKeyboardEvent _ = False

isKeyPressed :: SystemEvent -> KeyCode -> Bool
isKeyPressed (KeyAction _ code KeyPressed) codeChecked = code == codeChecked
isKeyPressed _ _ = False

isGUIPressed :: KeyMod -> Bool
isGUIPressed mod = _kmLeftGUI mod || _kmRightGUI mod

isCtrlPressed :: KeyMod -> Bool
isCtrlPressed keyMod = _kmLeftCtrl keyMod || _kmRightCtrl keyMod

isShiftPressed :: KeyMod -> Bool
isShiftPressed keyMod = _kmLeftShift keyMod || _kmRightShift keyMod

isAltPressed :: KeyMod -> Bool
isAltPressed keyMod = _kmLeftAlt keyMod || _kmRightAlt keyMod

isOnFocus :: SystemEvent -> Bool
isOnFocus Focus{} = True
isOnFocus _ = False

isOnBlur :: SystemEvent -> Bool
isOnBlur Blur{} = True
isOnBlur _ = False

isOnEnter :: SystemEvent -> Bool
isOnEnter Enter{} = True
isOnEnter _ = False

isOnLeave :: SystemEvent -> Bool
isOnLeave Leave{} = True
isOnLeave _ = False

isOnMove :: SystemEvent -> Bool
isOnMove Move{} = True
isOnMove _ = False
