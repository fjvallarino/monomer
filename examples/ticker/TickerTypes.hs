{-|
Module      : Main
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Types for the 'Ticker' example.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module TickerTypes where

import Control.Concurrent.STM.TChan
import Control.Lens.TH
import Data.Default
import Data.Foldable (asum)
import Data.Map (Map)
import Data.Text (Text, pack)

import qualified Data.Map as M

import BinanceTypes

newtype AppEnv = AppEnv {
  _envChannel :: TChan ServerRequest
}

data TickerModel = TickerModel {
  _prcNewPair :: Text,
  _prcSymbolPairs :: [Text],
  _prcTickers :: Map Text Ticker
} deriving (Eq, Show)

instance Default TickerModel where
  def = TickerModel {
    _prcNewPair = "",
    _prcSymbolPairs = [],
    _prcTickers = M.empty
  }

data TickerEvt
  = TickerInit
  | TickerIgnore
  | TickerAddClick
  | TickerRemovePairBegin Text
  | TickerRemovePair Text
  | TickerMovePair Text Text
  | TickerUpdate [Ticker]
  | TickerError ServerError
  | TickerResponse ServerResponse
  deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppEnv
makeLensesWith abbreviatedFields 'TickerModel
