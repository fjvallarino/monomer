{-# LANGUAGE TemplateHaskell #-}

module Types where

import Lens.Micro.TH (makeLenses)

import Data.Default

import qualified GUI.Data.Tree as TR
import GUI.Widget.Core (GUIContext)

data App = App {
  _clickCount :: !Int
} deriving (Show, Eq)

instance Default App where
  def = App 0

makeLenses ''App
makeLenses ''GUIContext
