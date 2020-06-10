module Monomer.Event.Core where

import Control.Applicative ((<|>))
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import Data.Traversable

import qualified Data.Sequence as Seq
import qualified SDL

import Monomer.Common.Geometry
import Monomer.Event.Keyboard
import Monomer.Event.Mouse
import Monomer.Event.Types

convertEvents :: Double -> Point -> [SDL.EventPayload] -> [SystemEvent]
convertEvents devicePixelRate mousePos events = catMaybes convertedEvents where
  convertedEvents = fmap convertEvent events
  convertEvent evt =
    mouseMoveEvent devicePixelRate mousePos evt
    <|> mouseClick mousePos evt
    <|> mouseWheelEvent devicePixelRate mousePos evt
    <|> keyboardEvent evt
    <|> textEvent evt

checkKeyboard :: SystemEvent -> (KeyMod -> KeyCode -> KeyStatus -> Bool) -> Bool
checkKeyboard (KeyAction mod code motion) testFn = testFn mod code motion
checkKeyboard _ _ = False

isCustomHandler :: EventRequest s -> Bool
isCustomHandler (RunCustom _ _) = True
isCustomHandler _ = False

isProducerHandler :: EventRequest s -> Bool
isProducerHandler (RunProducer _ _) = True
isProducerHandler _ = False

isIgnoreParentEvents :: EventRequest s -> Bool
isIgnoreParentEvents IgnoreParentEvents = True
isIgnoreParentEvents _ = False

isIgnoreChildrenEvents :: EventRequest s -> Bool
isIgnoreChildrenEvents IgnoreChildrenEvents = True
isIgnoreChildrenEvents _ = False

isSetFocus :: EventRequest s -> Bool
isSetFocus (SetFocus _) = True
isSetFocus _ = False

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
getUpdateUserStates reqs = foldl' foldHelper Seq.empty reqs where
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

convertRequest :: EventRequest s -> Maybe (EventRequest s2)
convertRequest IgnoreParentEvents = Just IgnoreParentEvents
convertRequest IgnoreChildrenEvents = Just IgnoreChildrenEvents
convertRequest (SetFocus path) = Just (SetFocus path)
convertRequest (GetClipboard path) = Just (GetClipboard path)
convertRequest (SetClipboard clipboard) = Just (SetClipboard clipboard)
convertRequest (RunCustom path action) = Just (RunCustom path action)
convertRequest (RunProducer path action) = Just (RunProducer path action)
convertRequest (UpdateUserState fn) = Nothing

convertRequests :: Seq (EventRequest s) -> Seq (EventRequest sp)
convertRequests reqs = fmap fromJust $ Seq.filter isJust $ fmap convertRequest reqs
