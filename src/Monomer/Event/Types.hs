{-# LANGUAGE ExistentialQuantification #-}

module Monomer.Event.Types where

import Data.Text (Text)
import Data.Map.Strict (Map)

import Monomer.Common.Geometry
import Monomer.Common.Tree (Path)

type KeyCode = Int

data KeyStatus
  = KeyPressed
  | KeyReleased
  deriving (Show, Eq)

data Button
  = LeftBtn
  | MiddleBtn
  | RightBtn
  deriving (Show, Eq, Ord)

data ButtonState
  = PressedBtn
  | ReleasedBtn
  deriving (Show, Eq)

data WheelDirection
  = WheelNormal
  | WheelFlipped
  deriving (Show, Eq)

data ClipboardData
  = ClipboardEmpty
  | ClipboardText Text
  deriving (Eq, Show)

data SystemEvent
  = Click Point Button
  | ButtonAction Point Button ButtonState
  | WheelScroll Point Point WheelDirection
  | KeyAction KeyMod KeyCode KeyStatus
  | TextInput Text
  | Clipboard ClipboardData
  | Focus
  | Blur
  | Enter Point
  | Move Point
  | Leave Path Point
  deriving (Show, Eq)

data InputStatus = InputStatus {
  statusMousePos :: Point,
  statusKeyMod :: KeyMod,
  statusKeys :: Map KeyCode KeyStatus,
  statusButtons :: Map Button ButtonState
} deriving (Eq, Show)

data KeyMod = KeyMod {
  keyModLeftShift :: Bool,
  keyModRightShift :: Bool,
  keyModLeftCtrl :: Bool,
  keyModRightCtrl :: Bool,
  keyModLeftAlt :: Bool,
  keyModRightAlt :: Bool,
  keyModLeftGUI :: Bool,
  keyModRightGUI :: Bool,
  keyModNumLock :: Bool,
  keyModCapsLock :: Bool,
  keyModAltGr :: Bool
} deriving (Show, Eq)
