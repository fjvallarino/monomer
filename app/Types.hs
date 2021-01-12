{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Codec.Serialise
import Control.Lens (makeLenses)
import Data.Default
import Data.Text (Text)
import GHC.Generics

import Monomer

data Fruit
  = Apple
  | Orange
  | Pear
  deriving (Eq, Show, Generic, Serialise)

data App = App {
  _clickCount :: !Int,
  _msgCount :: !Int,
  _int1 :: Int,
  _validInt1 :: Bool,
  _integer1 :: Integer,
  _validInteger1 :: Bool,
  _float1 :: Double,
  _validFloat1 :: Bool,
  _textField1 :: Text,
  _textField2 :: Text,
  _dropdown1 :: Text,
  _fruit :: Fruit,
  _condition1 :: Bool,
  _condition2 :: Bool,
  _condition3 :: Bool,
  _showAlert :: Bool,
  _showConfirm :: Bool
} deriving (Show, Eq, Generic, Serialise)

instance Default App where
  def = App {
    _clickCount = 0,
    _msgCount = 0,
    _int1 = 0,
    _validInt1 = True,
    _integer1 = 0,
    _validInteger1 = True,
    _float1 = 0,
    _validFloat1 = True,
    _textField1 = "This is a long piece of text used to test mouse selection",
    _textField2 = "",
    _dropdown1 = "",
    _fruit = Orange,
    _condition1 = False,
    _condition2 = False,
    _condition3 = False,
    _showAlert = False,
    _showConfirm = False
  }

makeLenses ''App

data AppEvent
  = InitApp
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
  | ShowAlert
  | CloseAlert
  | ShowConfirm
  | AcceptConfirm
  | CancelConfirm
  | ChangeTitle Text
  | ExitApp
  | CancelExitApp
  | MaxWindow
  | MinWindow
  | FullWindow
  | RestoreWindow
  | RestoreWindowSchedule
  | ToFrontWindow
  | ToFrontWindowSchedule
  deriving (Show, Eq)
