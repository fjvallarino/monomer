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

isButtonPressedInRect :: InputStatus -> Button -> Rect -> Bool
isButtonPressedInRect inputStatus button rect = pressed && inRect where
  mousePos = _ipsMousePos inputStatus
  inRect = pointInRect mousePos rect
  pressed = isButtonPressed inputStatus button

getKeyStatus :: InputStatus -> KeyCode -> KeyStatus
getKeyStatus inputStatus code = status where
  keys = _ipsKeys inputStatus
  status = fromMaybe KeyReleased (M.lookup code keys)

isShortCutControl :: WidgetEnv s e -> KeyMod -> Bool
isShortCutControl wenv mod = isControl || isCommand where
  isControl = not (isMacOS wenv) && _kmLeftCtrl mod
  isCommand = isMacOS wenv && _kmLeftGUI mod

isClipboardCopy :: WidgetEnv s e -> SystemEvent -> Bool
isClipboardCopy wenv event = checkKeyboard event testFn where
  testFn mod code motion = isShortCutControl wenv mod && isKeyC code

isClipboardPaste :: WidgetEnv s e -> SystemEvent -> Bool
isClipboardPaste wenv event = checkKeyboard event testFn where
  testFn mod code motion = isShortCutControl wenv mod && isKeyV code

checkKeyboard :: SystemEvent -> (KeyMod -> KeyCode -> KeyStatus -> Bool) -> Bool
checkKeyboard (KeyAction mod code motion) testFn = testFn mod code motion
checkKeyboard _ _ = False

isKeyboardEvent :: SystemEvent -> Bool
isKeyboardEvent KeyAction{} = True
isKeyboardEvent _ = False

isKeyPressed :: SystemEvent -> KeyCode -> Bool
isKeyPressed (KeyAction _ code KeyPressed) codeChecked = code == codeChecked
isKeyPressed _ _ = False

isShiftPressed :: SystemEvent -> Bool
isShiftPressed (KeyAction keyMod _ _) = _kmLeftShift keyMod
isShiftPressed _ = False

isOnFocus :: SystemEvent -> Bool
isOnFocus Focus = True
isOnFocus _ = False

isOnBlur :: SystemEvent -> Bool
isOnBlur Blur = True
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
