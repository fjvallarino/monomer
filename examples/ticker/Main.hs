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
  closeIcon = icon_ IconClose [width 4] `style` [width 12, height 12, fgColor crimson, cursorIcon CursorHand]
  dropTicker pair = dropTarget_ (TickerMovePair pair) [dropTargetStyle [bgColor darkGray]] spacer
  tickerItem t = vstack [
      dropTicker (t ^. symbolPair),
      draggable (t ^. symbolPair) $ hstack [
        label (t ^. symbolPair) `style` [width 100],
        spacer,
        label (scientificText (t ^. close)) `style` [textRight, minWidth 100],
        spacer,
        box_ [onClick (TickerRemovePair (t ^. symbolPair))] closeIcon
      ]
    ]
  tickerList = vstack (tickerRows ++ [dropTicker "--Invalid--"]) where
    orderedTickers = (\e -> model ^? tickers . ix e) <$> model ^. symbolPairs
    tickerRows = tickerItem <$> catMaybes orderedTickers
  mainLayer = vstack [
      hstack [
        label "New pair: ",
        keystroke [("Enter", TickerAddClick)] $ textField newPair `key` "newPair",
        spacer,
        button "Add" TickerAddClick
      ] `style` [padding 5, paddingB 0],
      spacer,
      scroll $ tickerList `style` [padding 5, paddingT 0]
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
    Model $ model
      & symbolPairs .~ initialList,
    Producer (startProducer env),
    Producer $ \sendMsg -> do
      threadDelay 500000
      void $ subscribe env initialList,
    setFocus wenv "newPair"
    ]
  TickerAddClick -> [Event $ TickerAddPair (model ^. newPair)]
  TickerAddPair pair -> [
    Model $ model
      & symbolPairs %~ (pair <|)
      & newPair .~ "",
    Task $ subscribe env [pair],
    setFocus wenv "newPair"
    ]
  TickerRemovePair pair -> [
      Task $ unsubscribe env [pair],
      Model $ model & tickers . at pair .~ Nothing
    ]
  TickerMovePair target pair -> [
      Model $ model & symbolPairs .~ moveBefore (model^.symbolPairs) target pair
    ]
  TickerUpdate ticker -> [
    Model $ model & tickers . at (ticker ^. symbolPair) ?~ ticker
    ]
  TickerError err -> [Task $ print ("Error", err) >> return TickerIgnore]
  TickerResponse resp -> [Task $ print ("Response", resp) >> return TickerIgnore]
  TickerIgnore -> []

handleSubscription :: AppEnv -> [Text] -> Text -> IO TickerEvt
handleSubscription env pairs action = do
  liftIO . atomically $ writeTChan (env^.channel) req
  return TickerIgnore
  where
    subscription pair = T.toLower pair <> "@miniTicker"
    req = ServerRequest 1 action (subscription <$> pairs)

subscribe :: AppEnv -> [Text] -> IO TickerEvt
subscribe env pairs = handleSubscription env pairs "SUBSCRIBE"

unsubscribe :: AppEnv -> [Text] -> IO TickerEvt
unsubscribe env pairs = handleSubscription env pairs "UNSUBSCRIBE"

moveBefore :: Eq a => [a] -> a -> a -> [a]
moveBefore list target item = result where
  result
    | target == item = list
    | otherwise = moveBefore_ cleanList target item
  cleanList = filter (/= item) list
  moveBefore_ [] target item = [item]
  moveBefore_ (x:xs) target item
    | x == target = item : x : xs
    | otherwise = x : moveBefore_ xs target item

--https://www.reddit.com/r/haskell/comments/4knu6r/how_to_manage_exceptions/d3gezq0/?utm_source=reddit&utm_medium=web2x&context=3
startProducer :: AppEnv -> (TickerEvt -> IO ()) -> IO ()
startProducer env sendMsg = do
  Wuss.runSecureClient url port path $ \connection -> do
    receiveWs connection sendMsg
    sendWs (env^.channel) connection
  where
    url = "stream.binance.com"
    port = 9443
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
      appMaxFps 10,
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
scientificText = T.pack . formatScientific Fixed (Just 8)

initialList :: [Text]
initialList = ["BTCUSDT", "ADABTC", "COTIBTC", "SXPBTC", "BNBBTC", "GRTBTC",
  "CRVBTC", "ZRXBTC", "NEXOBTC", "XTZBTC", "BATBTC", "VETBTC", "REEFBTC",
  "EOSBTC", "AKROBTC", "DOGEBTC", "XHVBTC", "ANKRBTC", "BNTBTC", "XLMBTC",
  "XRPBTC", "SOLBTC", "SRMBTC", "DOTBTC", "ETHBTC", "LTCBTC"]
