{-# LANGUAGE ExistentialQuantification #-}

module Monomer.Event.Types where

import Data.Default
import Data.Text (Text)
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as M

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
  ipsMousePos :: Point,
  ipsKeyMod :: KeyMod,
  ipsKeys :: Map KeyCode KeyStatus,
  ipsButtons :: Map Button ButtonState
} deriving (Eq, Show)

instance Default InputStatus where
  def = InputStatus {
    ipsMousePos = def,
    ipsKeyMod = def,
    ipsKeys = M.empty,
    ipsButtons = M.empty
  }

data KeyMod = KeyMod {
  kmLeftShift :: Bool,
  kmRightShift :: Bool,
  kmLeftCtrl :: Bool,
  kmRightCtrl :: Bool,
  kmLeftAlt :: Bool,
  kmRightAlt :: Bool,
  kmLeftGUI :: Bool,
  kmRightGUI :: Bool,
  kmNumLock :: Bool,
  kmCapsLock :: Bool,
  kmAltGr :: Bool
} deriving (Show, Eq)

instance Default KeyMod where
  def = KeyMod {
    kmLeftShift = False,
    kmRightShift = False,
    kmLeftCtrl = False,
    kmRightCtrl = False,
    kmLeftAlt = False,
    kmRightAlt = False,
    kmLeftGUI = False,
    kmRightGUI = False,
    kmNumLock = False,
    kmCapsLock = False,
    kmAltGr = False
  }
