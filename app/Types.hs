{-# LANGUAGE TemplateHaskell #-}

module Types where

import Lens.Micro.TH (makeLenses)

import Data.Default
import qualified Data.Text as T

data App = App {
  _clickCount :: !Int,
  _textField1 :: T.Text,
  _textField2 :: T.Text,
  _textField3 :: T.Text
} deriving (Show, Eq)

instance Default App where
  def = App 0 "" "" ""

makeLenses ''App

data CompState = CompState {
  _csCounter :: Int
} deriving (Show, Eq)

instance Default CompState where
  def = CompState 0

makeLenses ''CompState
