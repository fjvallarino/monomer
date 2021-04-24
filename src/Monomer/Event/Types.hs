{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module Monomer.Event.Types where

import Data.Default
import Data.Text (Text)
import Data.Typeable (Typeable, cast, typeOf)
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as M

import Monomer.Core.BasicTypes

newtype KeyCode
  = KeyCode { unKeyCode :: Int }
  deriving (Eq, Ord, Show)

data KeyStatus
  = KeyPressed
  | KeyReleased
  deriving (Eq, Show)

data Button
  = LeftBtn
  | MiddleBtn
  | RightBtn
  deriving (Eq, Show, Ord)

data ButtonState
  = PressedBtn
  | ReleasedBtn
  deriving (Eq, Show)

data WheelDirection
  = WheelNormal
  | WheelFlipped
  deriving (Eq, Show)

data ClipboardData
  = ClipboardEmpty
  | ClipboardText Text
  deriving (Eq, Show)

type DragMsg i = (Eq i, Typeable i)

data WidgetDragMsg
  = forall i . DragMsg i => WidgetDragMsg i

instance Eq WidgetDragMsg where
  WidgetDragMsg d1 == WidgetDragMsg d2 = case cast d1 of
    Just d -> d == d2
    _ -> False

instance Show WidgetDragMsg where
  show (WidgetDragMsg info) = "WidgetDragMsg: " ++ show (typeOf info)

data SystemEvent
  = Click Point Button
  | DblClick Point Button
  | ButtonAction Point Button ButtonState Int
  | WheelScroll Point Point WheelDirection
  | KeyAction KeyMod KeyCode KeyStatus
  | TextInput Text
  | Clipboard ClipboardData
  | Focus
  | Blur
  | Enter Point
  | Move Point
  | Leave Point
  | Drag Point Path WidgetDragMsg
  | Drop Point Path WidgetDragMsg
  deriving (Eq, Show)

data InputStatus = InputStatus {
  _ipsMousePos :: Point,
  _ipsMousePosPrev :: Point,
  _ipsKeyMod :: KeyMod,
  _ipsKeys :: Map KeyCode KeyStatus,
  _ipsButtons :: Map Button ButtonState
} deriving (Eq, Show)

instance Default InputStatus where
  def = InputStatus {
    _ipsMousePos = Point (-1) (-1),
    _ipsMousePosPrev = Point (-1) (-1),
    _ipsKeyMod = def,
    _ipsKeys = M.empty,
    _ipsButtons = M.empty
  }

data KeyMod = KeyMod {
  _kmLeftShift :: Bool,
  _kmRightShift :: Bool,
  _kmLeftCtrl :: Bool,
  _kmRightCtrl :: Bool,
  _kmLeftAlt :: Bool,
  _kmRightAlt :: Bool,
  _kmLeftGUI :: Bool,
  _kmRightGUI :: Bool,
  _kmNumLock :: Bool,
  _kmCapsLock :: Bool,
  _kmAltGr :: Bool
} deriving (Eq, Show)

instance Default KeyMod where
  def = KeyMod {
    _kmLeftShift = False,
    _kmRightShift = False,
    _kmLeftCtrl = False,
    _kmRightCtrl = False,
    _kmLeftAlt = False,
    _kmRightAlt = False,
    _kmLeftGUI = False,
    _kmRightGUI = False,
    _kmNumLock = False,
    _kmCapsLock = False,
    _kmAltGr = False
  }
