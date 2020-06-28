{-# LANGUAGE TemplateHaskell #-}

module Types where

import Lens.Micro.TH (makeLenses)

import Data.Default
import Data.Text (Text)

data App = App {
  _clickCount :: !Int,
  _msgCount :: !Int,
  _textField1 :: Text,
  _textField2 :: Text,
  _textField3 :: Text,
  _dropdown1 :: Text
} deriving (Show, Eq)

instance Default App where
  def = App 0 0 "" "" "" ""

makeLenses ''App

data AppEvent = InitApp
              | RunShortTask
              | RunLongTask
              | PrintTextFields
              | AppButton
              | IncreaseMessage
              | UpdateText Text
              deriving (Show, Eq)
