{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module TickerTypes where

import Control.Applicative ((<|>))
import Control.Concurrent.STM.TChan
import Control.Lens.TH
import Data.Aeson
import Data.Default
import Data.Foldable (asum)
import Data.Maybe
import Data.Map (Map)
import Data.Scientific
import Data.Text (Text, pack)

import qualified Data.Map as M

import Monomer (WidgetModel)

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
  | TickerAddPair Text
  | TickerRemovePairBegin Text
  | TickerRemovePair Text
  | TickerMovePair Text Text
  | TickerUpdate Ticker
  | TickerError ServerError
  | TickerResponse ServerResponse
  deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppEnv
makeLensesWith abbreviatedFields 'TickerModel
