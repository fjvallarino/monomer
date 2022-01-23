{-|
Module      : BinanceTypes
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Types for the Binance Exchange in the 'Ticker' example.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module BinanceTypes where

import Control.Applicative ((<|>))
import Control.Concurrent.STM.TChan
import Control.Lens.TH
import Data.Aeson
import Data.Default
import Data.Fixed
import Data.Foldable (asum)
import Data.Maybe
import Data.Map (Map)
import Data.Text (Text, pack)

type FixedFloat = Pico

data ServerRequest = ServerRequest {
  _srqRequestId :: Int,
  _srqMethod :: Text,
  _srqParams :: [Text]
} deriving (Eq, Show)

instance ToJSON ServerRequest where
  toJSON (ServerRequest reqId method params) = object [
    "id" .= reqId,
    "method" .= method,
    "params" .= params
    ]

data ServerResponse = ServerResponse {
  _srpRequestId :: Int,
  _srpResult :: [Text]
} deriving (Eq, Show)

instance FromJSON ServerResponse where
  parseJSON = withObject "response" $ \o -> do
    _srpRequestId <- o .: "id"
    _srpResult <- o .: "result" <|> pure []

    return ServerResponse{..}

data ServerError = ServerError {
  _sveCode :: Int,
  _sveMessage :: Text
} deriving (Eq, Show)

instance FromJSON ServerError where
  parseJSON = withObject "error" $ \o -> do
    _sveCode <- o .: "code"
    _sveMessage <- o .: "msg"

    return ServerError{..}

data Ticker = Ticker {
  _tckSymbolPair :: Text,
  _tckTs :: Int,
  _tckOpen :: FixedFloat,
  _tckClose :: FixedFloat,
  _tckHigh :: FixedFloat,
  _tckLow :: FixedFloat,
  _tckVolume :: FixedFloat,
  _tckTrades :: FixedFloat
} deriving (Eq, Show)

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

data ServerMsg
  = MsgResponse ServerResponse
  | MsgError ServerError
  | MsgTicker Ticker
  deriving (Eq, Show)

instance FromJSON ServerMsg where
  parseJSON v = asum [
    MsgResponse <$> parseJSON v,
    MsgError <$> parseJSON v,
    MsgTicker <$> parseJSON v
    ]

makeLensesWith abbreviatedFields 'Ticker
