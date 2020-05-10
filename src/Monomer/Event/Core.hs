module Monomer.Event.Core where

import qualified SDL

import Monomer.Common.Types
import Monomer.Event.Keyboard
import Monomer.Event.Mouse
import Monomer.Event.Types

convertEvents :: Double -> Point -> [SDL.EventPayload] -> [SystemEvent]
convertEvents devicePixelRate mousePos events = newEvents
  where
    newEvents = mouseEvents ++ mouseMoveEvents ++ mouseWheelEvents ++ keyboardEvents ++ textEvents
    mouseEvents = mouseClick devicePixelRate events
    mouseMoveEvents = mouseMoveEvent devicePixelRate mousePos events
    mouseWheelEvents = mouseWheelEvent devicePixelRate mousePos events
    keyboardEvents = keyboardEvent events
    textEvents = textEvent events

checkKeyboard :: SystemEvent -> (KeyMod -> KeyCode -> KeyStatus -> Bool) -> Bool
checkKeyboard (KeyAction mod code motion) testFn = testFn mod code motion
checkKeyboard _ _ = False

isIgnoreParentEvents :: EventRequest -> Bool
isIgnoreParentEvents IgnoreParentEvents = True
isIgnoreParentEvents _ = False

isIgnoreChildrenEvents :: EventRequest -> Bool
isIgnoreChildrenEvents IgnoreChildrenEvents = True
isIgnoreChildrenEvents _ = False

isResizeChildren :: EventRequest -> Bool
isResizeChildren ResizeChildren = True
isResizeChildren _ = False

isResizeAll :: EventRequest -> Bool
isResizeAll ResizeAll = True
isResizeAll _ = False

isGetClipboard :: EventRequest -> Bool
isGetClipboard GetClipboard = True
isGetClipboard _ = False

isSetClipboard :: EventRequest -> Bool
isSetClipboard (SetClipboard _) = True
isSetClipboard _ = False

isClipboardCopy :: SystemEvent -> Bool
isClipboardCopy event = checkKeyboard event (\mod code motion -> (keyModLeftGUI mod || keyModLeftCtrl mod) && isKeyC code)

isClipboardPaste :: SystemEvent -> Bool
isClipboardPaste event = checkKeyboard event (\mod code motion -> (keyModLeftGUI mod || keyModLeftCtrl mod) && isKeyV code)

isKeyboardEvent :: SystemEvent -> Bool
isKeyboardEvent (KeyAction _ _ _) = True
isKeyboardEvent _ = False

isKeyPressed :: SystemEvent -> KeyCode -> Bool
isKeyPressed (KeyAction _ keyCode KeyPressed) keyCodeChecked = keyCode == keyCodeChecked
isKeyPressed _ _ = False

isShiftPressed :: SystemEvent -> Bool
isShiftPressed (KeyAction keyMod _ _) = keyModLeftShift keyMod
isShiftPressed _ = False
