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

isKeyboardEvent :: SystemEvent -> Bool
isKeyboardEvent KeyAction{} = True
isKeyboardEvent _ = False

isKeyPressed :: SystemEvent -> KeyCode -> Bool
isKeyPressed (KeyAction _ code KeyPressed) codeChecked = code == codeChecked
isKeyPressed _ _ = False

isShiftPressed :: SystemEvent -> Bool
isShiftPressed (KeyAction keyMod _ _) = kmLeftShift keyMod
isShiftPressed _ = False
