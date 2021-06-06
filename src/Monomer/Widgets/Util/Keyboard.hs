{-|
Module      : Monomer.Widgets.Util.Keyboard
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Utility functions for widget keyboard handling.
-}
module Monomer.Widgets.Util.Keyboard (
  isShortCutControl,
  isKeyboardCopy,
  isKeyboardPaste,
  isKeyboardCut,
  isKeyboardUndo,
  isKeyboardRedo
) where

import Data.Maybe (fromMaybe)

import qualified Data.Map as M

import Monomer.Core
import Monomer.Event.Core
import Monomer.Event.Keyboard
import Monomer.Event.Types
import Monomer.Event.Util

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
