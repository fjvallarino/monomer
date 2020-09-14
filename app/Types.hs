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
  _integer1 :: Integer,
  _float1 :: Double,
  _validFloat1 :: Bool,
  _textField1 :: Text,
  _textField2 :: Text,
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
    _integer1 = 0,
    _float1 = 0,
    _validFloat1 = True,
    _textField1 = "",
    _textField2 = "",
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
  deriving (Show, Eq)
