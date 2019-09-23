{-# LANGUAGE TemplateHaskell #-}

module Types where

import Lens.Micro.TH (makeLenses)

import Data.Default

data App = App {
  _clickCount :: !Int
} deriving (Show, Eq)

instance Default App where
  def = App 0

makeLenses ''App
