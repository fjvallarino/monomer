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
  dragColor = lightGray & L.a .~ 0.5
  closeIcon = icon_ IconClose [width 4] `style` [width 12, height 12, fgColor crimson, cursorHand]
  dropTicker pair widget = dropTarget_ (TickerMovePair pair) [dropTargetStyle [bgColor darkGray]] widget
  tickerPct t = label pctText `style` [width 100, textRight, textColor pctColor] where
    diff = toRealFloat $ 100 * (t ^. close - t ^. open)
    pct = diff / toRealFloat (t ^. open)
    pctText = formatTickerPct (fromFloatDigits pct) <> "%"
    pctColor
      | abs pct < 0.01 = white
      | pct > 0 = green
      | otherwise = red
  tickerItem t = vstack [
      spacer,
      dropTicker (t ^. symbolPair) $
        draggable_ (t ^. symbolPair) [draggableStyle [bgColor dragColor]] $ hstack [
          label (t ^. symbolPair) `style` [width 100],
          spacer,
          label (formatTickerValue (t ^. close)) `style` [textRight, minWidth 100],
          spacer,
          tickerPct t,
          spacer,
          box_ [onClick (TickerRemovePairBegin (t ^. symbolPair))] closeIcon
        ] `style` [cursorHand]
    ]
  tickerList = vstack tickerRows where
    orderedTickers = (\e -> model ^? tickers . ix e) <$> model ^. symbolPairs
    tickerFade t = fadeOut_ [onFinished action] item `key` (t ^. symbolPair) where
      action = TickerRemovePair (t ^. symbolPair)
      item = tickerItem t
    tickerRows = tickerFade <$> catMaybes orderedTickers
  widgetTree = vstack [
      hstack [
        label "New pair: ",
        keystroke [("Enter", TickerAddClick)] $ textField newPair `key` "newPair",
        spacer,
        button "Add" TickerAddClick
      ] `style` [padding 5, paddingB 0],
      spacer,
      scroll $ tickerList `style` [padding 5, paddingT 0]
    ]

handleEvent
  :: AppEnv
  -> WidgetEnv TickerModel TickerEvt
  -> WidgetNode TickerModel TickerEvt
  -> TickerModel
  -> TickerEvt
  -> [EventResponse TickerModel TickerEvt TickerModel ()]
handleEvent env wenv node model evt = case evt of
  TickerInit -> [
    Model $ model
      & symbolPairs .~ initialList,
    Producer (startProducer env),
    Task (subscribeInitial env initialList),
    setFocusOnKey wenv "newPair"
    ]
  TickerAddClick -> [Event $ TickerAddPair (model ^. newPair)]
  TickerAddPair pair -> [
    Model $ model
      & symbolPairs %~ (pair <|)
      & newPair .~ "",
    Task $ subscribe env [pair],
    setFocusOnKey wenv "newPair"
    ]
  TickerRemovePairBegin pair -> [
    Message (WidgetKey pair) AnimationStart]
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

startProducer :: AppEnv -> (TickerEvt -> IO ()) -> IO ()
startProducer env sendMsg = do
  Wuss.runSecureClient url port path $ \connection -> do
    receiveWs connection sendMsg
    sendWs (env^.channel) connection
  where
    url = "stream.binance.com"
    port = 9443
    path = "/ws"

subscribeInitial :: AppEnv -> [Text] -> IO TickerEvt
subscribeInitial env initialList = do
  threadDelay 500000
  subscribe env initialList

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

  startApp initModel (handleEvent env) buildUI config
  where
    config = [
      appWindowTitle "Ticker",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appInitEvent TickerInit
      ]
    initModel = def

formatTickerValue :: Scientific -> Text
formatTickerValue = T.pack . formatScientific Fixed (Just 8)

formatTickerPct :: Scientific -> Text
formatTickerPct = T.pack . formatScientific Fixed (Just 2)

initialList :: [Text]
initialList = ["BTCUSDT", "ETHBTC", "BNBBTC", "ADABTC", "DOTBTC", "XRPBTC",
  "UNIBTC", "LTCBTC", "LINKBTC", "BCHBTC", "DOGEBTC", "THETABTC", "LUNABTC",
  "AAVEBTC", "CROBTC", "VETBTC", "XMRBTC", "ATOMBTC", "FTTBTC", "SOLBTC",
  "SXPBTC", "BATBTC", "VETBTC", "REEFBTC"]
