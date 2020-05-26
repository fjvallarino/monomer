module Monomer.Event.Core where

import Data.Traversable
import Data.Sequence (Seq(..), (<|), (|>), (><))

import qualified Data.Sequence as Seq
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

isCustomHandler :: EventRequest s -> Bool
isCustomHandler (RunCustom _ _) = True
isCustomHandler _ = False

isIgnoreParentEvents :: EventRequest s -> Bool
isIgnoreParentEvents IgnoreParentEvents = True
isIgnoreParentEvents _ = False

isIgnoreChildrenEvents :: EventRequest s -> Bool
isIgnoreChildrenEvents IgnoreChildrenEvents = True
isIgnoreChildrenEvents _ = False

isResizeChildren :: EventRequest s -> Bool
isResizeChildren (ResizeChildren _) = True
isResizeChildren _ = False

isResizeAll :: EventRequest s -> Bool
isResizeAll ResizeAll = True
isResizeAll _ = False

isGetClipboard :: EventRequest s -> Bool
isGetClipboard (GetClipboard _) = True
isGetClipboard _ = False

isSetClipboard :: EventRequest s -> Bool
isSetClipboard (SetClipboard _) = True
isSetClipboard _ = False

isUpdateUserState :: EventRequest s -> Bool
isUpdateUserState (UpdateUserState _) = True
isUpdateUserState _ = False

getUpdateUserStates :: (Traversable t) => t (EventRequest s) -> Seq (s -> s)
getUpdateUserStates reqs = foldl foldHelper Seq.empty reqs where
  foldHelper acc (UpdateUserState fn) = acc |> fn
  foldHelper acc _ = acc

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
