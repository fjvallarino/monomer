{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.Aeson
import Data.Aeson.Types (parseEither, parseMaybe)
import Data.Default
import Data.Maybe
import Data.Scientific
import Data.Text (Text)
import TextShow

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Network.Wreq as W
import qualified Network.WebSockets as WS
import qualified Wuss

import TickerTypes
import Monomer

import qualified Monomer.Lens as L

buildUI
  :: WidgetEnv TickerModel TickerEvt
  -> TickerModel
  -> WidgetNode TickerModel TickerEvt
buildUI wenv model = widgetTree where
  pairItem pair = label pair
  pairsList = vstack (pairItem <$> model ^. symbolPairs)
  tickerItem t = hstack [
      label (t ^. symbolPair),
      spacer,
      label $ scientificText (t ^. close)
    ]
  tickerList = vstack (tickerItem <$> model ^. tickers)
  mainLayer = hstack [
      pairsList `style` [width 200],
      spacer,
      tickerList
    ]
  widgetTree = zstack [
      mainLayer
    ]

handleEvent
  :: WidgetEnv TickerModel TickerEvt
  -> WidgetNode TickerModel TickerEvt
  -> TickerModel
  -> TickerEvt
  -> [EventResponse TickerModel TickerEvt ()]
handleEvent wenv node model evt = case evt of
  TickerInit -> [
    Producer startProducer,
    setFocus wenv "query"
    ]
  TickerAddPair pair -> []
  TickerUpdate ticker -> [
    Model $ model & tickers . at (ticker ^. symbolPair) ?~ ticker,
    Task $ print ticker >> return TickerIgnore
    ]
  TickerIgnore -> []

--https://www.reddit.com/r/haskell/comments/4knu6r/how_to_manage_exceptions/d3gezq0/?utm_source=reddit&utm_medium=web2x&context=3
startProducer :: (TickerEvt -> IO ()) -> IO ()
startProducer sendMsg = do
  channel <- newTChanIO

  Wuss.runSecureClient url port path $ \connection -> do
    receiveWs connection sendMsg
    sendWs channel connection
  where
    url = "stream.binance.com"
    port = 9443
    path = "/ws/btcusdt@miniTicker"

receiveWs :: WS.Connection -> (TickerEvt -> IO ()) -> IO ()
receiveWs conn sendMsg = void . forkIO . forever $ do
  msg <- WS.receiveData conn
  let ticker = decode msg

  when (isJust ticker) $ do
    sendMsg $ TickerUpdate (fromJust ticker)

sendWs :: TChan a -> p -> IO ()
sendWs channel connection = do
  void . liftIO . atomically $ readTChan channel

main :: IO ()
main = do
  simpleApp initModel handleEvent buildUI config
  where
    config = [
      appWindowTitle "Ticker",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appInitEvent TickerInit
      ]
    initModel = def & symbolPairs .~ ["BTCUSDT"]

setFocus :: WidgetEnv s e -> Text -> EventResponse s e ep
setFocus wenv key = Request (SetFocus widgetId) where
  widgetId = fromMaybe def (globalKeyWidgetId wenv key)

scientificText :: Scientific -> Text
scientificText = T.pack . formatScientific Fixed Nothing
