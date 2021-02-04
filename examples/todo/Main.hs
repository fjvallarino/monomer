{-# LANGUAGE  DeriveAnyClass #-}
{-# LANGUAGE  DeriveGeneric #-}
{-# LANGUAGE  ScopedTypeVariables #-}

module Main where

import Codec.Serialise
import Data.Default
import Data.Text (Text)
import GHC.Generics

import Monomer

data TodoModel = TodoModel {
  _active :: [Todo],
  _complete :: [Todo]
} deriving (Eq, Show, Generic, Serialise)

data Todo = Todo {
  _todoDesc :: Text,
  _todoComplete :: Bool
} deriving (Eq, Show, Generic, Serialise)

data TodoEvt
  = TodoInit
  deriving (Eq, Show)

buildUI wenv model = widgetTree where
  widgetTree = vstack [
      label "Test"
    ]

handleEvent wenv node model evt = case evt of
  TodoInit -> []

main = do
  simpleApp_ model handleEvent buildUI config
  where
    model = TodoModel [] []
    config = [
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
      ]
