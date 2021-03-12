{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad (forever, forM_, void, when)
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
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

import Monomer

import BinanceTypes
import TickerTypes

import qualified Monomer.Lens as L

buildUI
  :: WidgetEnv TickerModel TickerEvt
  -> TickerModel
  -> WidgetNode TickerModel TickerEvt
buildUI wenv model = widgetTree where
  tickerItem t = hstack [
      label (t ^. symbolPair),
      spacer,
      label $ scientificText (t ^. close)
    ]
  tickerList = vstack (tickerItem <$> model ^. tickers)
  mainLayer = vstack [
      hstack [
        label "New pair: ",
        keystroke [("Enter", TickerAddPair)] $ textField newPair `key` "newPair",
        button "Add" TickerAddPair
      ],
      tickerList
    ]
  widgetTree = zstack [
      mainLayer
    ]

handleEvent
  :: AppEnv
  -> WidgetEnv TickerModel TickerEvt
  -> WidgetNode TickerModel TickerEvt
  -> TickerModel
  -> TickerEvt
  -> [EventResponse TickerModel TickerEvt ()]
handleEvent env wenv node model evt = case evt of
  TickerInit -> [
    Producer (startProducer env),
    setFocus wenv "newPair"
    ]
  TickerAddPair -> [
    Task $ do
      let subscription = T.toLower (model^.newPair) <> "@miniTicker"
      let req = ServerRequest 1 "SUBSCRIBE" [subscription]
      liftIO . atomically $ writeTChan (env^.channel) req
      return TickerIgnore,
    Model $ model & newPair .~ "",
    setFocus wenv "newPair"
    ]
  TickerUpdate ticker -> [
    Model $ model & tickers . at (ticker ^. symbolPair) ?~ ticker
    ]
  TickerError err -> [
    Task $ do
      print ("Error", err)
      return TickerIgnore
    ]
  TickerResponse resp -> [
    Task $ do
      print ("Resp", resp)
      return TickerIgnore
    ]
  TickerIgnore -> []

--https://www.reddit.com/r/haskell/comments/4knu6r/how_to_manage_exceptions/d3gezq0/?utm_source=reddit&utm_medium=web2x&context=3
startProducer :: AppEnv -> (TickerEvt -> IO ()) -> IO ()
startProducer env sendMsg = do
  Wuss.runSecureClient url port path $ \connection -> do
    receiveWs connection sendMsg
    sendWs (env^.channel) connection
  where
    url = "stream.binance.com"
    port = 9443
    --path = "/ws/btcusdt@miniTicker"
    path = "/ws"

receiveWs :: WS.Connection -> (TickerEvt -> IO ()) -> IO ()
receiveWs conn sendMsg = void . forkIO . forever $ do
  msg <- WS.receiveData conn
  let serverMsg = decode msg

  forM_ serverMsg $ \case
    MsgResponse resp -> sendMsg $ TickerResponse resp
    MsgError err -> sendMsg $ TickerError err
    MsgTicker ticker -> sendMsg $ TickerUpdate ticker

sendWs :: (Show a, ToJSON a) => TChan a -> WS.Connection -> IO ()
sendWs channel connection = forever $ do
  msg <- liftIO . atomically $ readTChan channel
  WS.sendTextData connection (encode msg)

main :: IO ()
main = do
  channel <- newTChanIO
  let env = AppEnv channel

  simpleApp initModel (handleEvent env) buildUI config
  where
    config = [
      appWindowTitle "Ticker",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appInitEvent TickerInit
      ]
    initModel = def

setFocus :: WidgetEnv s e -> Text -> EventResponse s e ep
setFocus wenv key = Request (SetFocus widgetId) where
  widgetId = fromMaybe def (globalKeyWidgetId wenv key)

scientificText :: Scientific -> Text
scientificText = T.pack . formatScientific Fixed Nothing
