{-|
Module      : GenerativeTypes
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Types for the 'Generative' example.
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module GenerativeTypes where

import Control.Lens.TH
import Data.Default
import Data.Text (Text)

import Monomer
import Widgets.CirclesGrid
import Widgets.BoxesPalette

data GenerativeType
  = CirclesGrid
  | BoxesPalette
  deriving (Eq, Show, Enum)

data GenerativeModel = GenerativeModel {
  _activeGen :: GenerativeType,
  _showCfg :: Bool,
  _circlesCfg :: CirclesGridCfg,
  _boxesCfg :: BoxesPaletteCfg
} deriving (Eq, Show)

data GenerativeEvt
  = GenerativeInit
  deriving (Eq, Show)

makeLenses ''GenerativeType
makeLenses ''GenerativeModel

genTypes :: [GenerativeType]
genTypes = enumFrom (toEnum 0)
