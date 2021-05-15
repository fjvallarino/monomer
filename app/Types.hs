{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens (makeLenses)
import Data.Default
import Data.Text (Text)
import Data.Time
import GHC.Generics

import Monomer

data Fruit
  = Apple
  | Orange
  | Pear
  deriving (Eq, Show)

data App = App {
  _clickCount :: !Int,
  _msgCount :: !Int,
  _rational1 :: Rational,
  _int1 :: Int,
  _mint1 :: Maybe Int,
  _int1Valid :: Bool,
  _mint1Valid :: Bool,
  _integer1 :: Integer,
  _validInteger1 :: Bool,
  _float1 :: Float,
  _float1Valid :: Bool,
  _mfloat1 :: Maybe Float,
  _mfloat1Valid :: Bool,
  _double1 :: Double,
  _textField1 :: Text,
  _textField2 :: Text,
  _dropdown1 :: Text,
  _fruit :: Fruit,
  _condition1 :: Bool,
  _condition2 :: Bool,
  _condition3 :: Bool,
  _showAlert :: Bool,
  _showConfirm :: Bool,
  _splitPos :: Double,
  _dragList1 :: [Int],
  _dragList2 :: [Int],
  _color :: Color,
  _testDay :: Day,
  _testDayValid :: Bool,
  _mtestDay :: Maybe Day,
  _mtestDayValid :: Bool,
  _testTimeOfDay :: TimeOfDay,
  _testTimeOfDayValid :: Bool,
  _mtestTimeOfDay :: Maybe TimeOfDay,
  _mtestTimeOfDayValid :: Bool
} deriving (Eq, Show)

instance Default App where
  def = App {
    _clickCount = 0,
    _msgCount = 0,
    _rational1 = 0,
    _int1 = 0,
    _int1Valid = True,
    _mint1 = Nothing,
    _mint1Valid = True,
    _integer1 = 0,
    _validInteger1 = True,
    _float1 = 0,
    _float1Valid = True,
    _mfloat1 = Nothing,
    _mfloat1Valid = True,
    _double1 = 0,
    _textField1 = "This is a long piece of text used to test mouse selection",
    _textField2 = "",
    _dropdown1 = "",
    _fruit = Orange,
    _condition1 = False,
    _condition2 = False,
    _condition3 = False,
    _showAlert = False,
    _showConfirm = False,
    _splitPos = 0.5,
    _dragList1 = [1..100],
    _dragList2 = [],
    _color = def,
    _testDay = fromGregorian 2015 06 07,
    _testDayValid = True,
    _mtestDay = Nothing,
    _mtestDayValid = True,
    _testTimeOfDay = TimeOfDay 15 30 16,
    _testTimeOfDayValid = True,
    _mtestTimeOfDay = Nothing,
    _mtestTimeOfDayValid = True
  }

makeLenses ''App

data AppEvent
  = IgnoreEvt
  | RunShortTask
  | RunLongTask
  | PrintTextFields
  | AppButton
  | IncreaseMessage
  | UpdateText Text
  | IncButton
  | PrintMessage Text
  | CheckboxSt Bool
  | RadioSt Fruit
  | ImageMsg ImageLoadError
  | DropdownVal Text
  | DropdownIdx Int Text
  | DropTo1 Int
  | DropTo2 Int
  | ShowAlert
  | CloseAlert
  | ShowConfirm
  | AcceptConfirm
  | CancelConfirm
  | ChangeTitle Text
  | SliderPos Double
  | InitApp
  | DisposeApp
  | ExitApp
  | ResizeApp Rect
  | CancelExitApp
  | MaxWindow
  | MinWindow
  | FullWindow
  | RestoreWindow
  | RestoreWindowSchedule
  | ToFrontWindow
  | ToFrontWindowSchedule
  | StartAnimation
  | StopAnimation
  | UpdateColor Color
  | FocusColor Path
  | BlurColor Path
  deriving (Eq, Show)
