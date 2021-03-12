{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module TickerTypes where

import Control.Lens.TH
import Data.Aeson
import Data.Default
import Data.Maybe
import Data.Map (Map)
import Data.Scientific
import Data.Text (Text, pack)
import GHC.Generics

import qualified Data.Map as M

import Monomer

data Ticker = Ticker {
  _tckSymbolPair :: Text,
  _tckTs :: Int,
  _tckOpen :: Scientific,
  _tckClose :: Scientific,
  _tckHigh :: Scientific,
  _tckLow :: Scientific,
  _tckVolume :: Scientific,
  _tckTrades :: Scientific
} deriving (Eq, Show, Generic)

instance FromJSON Ticker where
  parseJSON = withObject "ticker" $ \o -> do
    _tckSymbolPair <- o .: "s"
    _tckTs <- o .: "E"
    _tckOpen <- read <$> o .: "o"
    _tckClose <- read <$> o .: "c"
    _tckHigh <- read <$> o .: "h"
    _tckLow <- read <$> o .: "l"
    _tckVolume <- read <$> o .: "v"
    _tckTrades <- read <$> o .: "q"

    return Ticker{..}

data TickerModel = TickerModel {
  _prcSymbolPairs :: [Text],
  _prcTickers :: Map Text Ticker
} deriving (Eq, Show, WidgetModel)

instance Default TickerModel where
  def = TickerModel {
    _prcSymbolPairs = [],
    _prcTickers = M.empty
  }

data TickerEvt
  = TickerInit
  | TickerIgnore
  | TickerAddPair Text
  | TickerUpdate Ticker
  deriving (Eq, Show)

makeLensesWith abbreviatedFields 'Ticker
makeLensesWith abbreviatedFields 'TickerModel
