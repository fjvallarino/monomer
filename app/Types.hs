{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens (makeLenses)

import Data.Default
import Data.Text (Text)

data Fruit
  = Apple
  | Orange
  | Pear
  deriving (Eq, Show)

data App = App {
  _clickCount :: !Int,
  _msgCount :: !Int,
  _word1 :: Word,
  _validWord1 :: Bool,
  _int1 :: Int,
  _validInt1 :: Bool,
  _integer1 :: Integer,
  _validInteger1 :: Bool,
  _float1 :: Double,
  _validFloat1 :: Bool,
  _textField1 :: Text,
  _textField2 :: Text,
  _validText2 :: Bool,
  _textField3 :: Text,
  _dropdown1 :: Text,
  _fruit :: Fruit,
  _condition1 :: Bool,
  _condition2 :: Bool,
  _condition3 :: Bool
} deriving (Show, Eq)

instance Default App where
  def = App {
    _clickCount = 0,
    _msgCount = 0,
    _word1 = 0,
    _validWord1 = True,
    _int1 = 0,
    _validInt1 = True,
    _integer1 = 0,
    _validInteger1 = True,
    _float1 = 0,
    _validFloat1 = True,
    _textField1 = "",
    _textField2 = "",
    _validText2 = True,
    _textField3 = "",
    _dropdown1 = "",
    _fruit = Orange,
    _condition1 = False,
    _condition2 = False,
    _condition3 = False
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
  deriving (Show, Eq)
