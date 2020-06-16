{-# LANGUAGE ExistentialQuantification #-}

module Monomer.Event.Types where

import Control.Concurrent.STM.TChan (TChan)
import Data.Typeable (Typeable)

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Monomer.Common.Geometry
import Monomer.Common.Tree

type KeyCode = Int
data KeyStatus = KeyPressed | KeyReleased deriving (Show, Eq)

data Button = LeftBtn | MiddleBtn | RightBtn deriving (Show, Eq, Ord)
data ButtonState = PressedBtn | ReleasedBtn deriving (Show, Eq)
data WheelDirection = WheelNormal | WheelFlipped deriving (Show, Eq)

data ClipboardData = ClipboardEmpty | ClipboardText T.Text deriving (Eq, Show)

data EventRequest s = IgnoreParentEvents
                    | IgnoreChildrenEvents
                    | SetFocus Path
                    | GetClipboard Path
                    | SetClipboard ClipboardData
                    | UpdateUserState (s -> s)
                    | forall a . Typeable a => RunTask Path (IO a)
                    | forall a . Typeable a => RunProducer Path ((a -> IO ()) -> IO ())

data SystemEvent = Click Point Button ButtonState
                 | WheelScroll Point Point WheelDirection
                 | KeyAction !KeyMod !KeyCode !KeyStatus
                 | TextInput T.Text
                 | Clipboard ClipboardData
                 | Focus
                 | Blur
                 | Enter Point
                 | Move Point
                 | Leave Path Point
                 deriving (Show, Eq)

data InputStatus = InputStatus {
  statusKeyMod :: KeyMod,
  statusKeys :: M.Map KeyCode KeyStatus,
  statusButtons :: M.Map Button ButtonState
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
