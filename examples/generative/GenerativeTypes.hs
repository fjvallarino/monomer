{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module GenerativeTypes where

import Control.Lens.TH
import Data.Default
import Data.Text (Text)

import Monomer

data GenerativeType
  = CirclesGrid
  | BoxesPalette
  deriving (Eq, Show, Enum)

newtype GenerativeModel = GenerativeModel {
  _activeGenerative :: GenerativeType
} deriving (Eq, Show)

data GenerativeEvt
  = GenerativeInit
  deriving (Eq, Show)

makeLenses ''GenerativeType
makeLenses ''GenerativeModel

generativeTypes :: [GenerativeType]
generativeTypes = enumFrom (toEnum 0)
