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
import Data.Default
import Data.Maybe
import Data.Scientific
import Data.Text (Text)

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
  sectionColor = wenv ^. L.theme . L.sectionColor
  dragColor = rgbaHex "#D3D3D3" 0.5
  rowSep = rgbaHex "#A9A9A9" 0.5
  rowBg = wenv ^. L.theme . L.userColorMap . at "rowBg" . non def
  trashBg = wenv ^. L.theme . L.userColorMap . at "trashBg" . non def
  trashFg = wenv ^. L.theme . L.userColorMap . at "trashFg" . non def
  trashIcon action = button remixDeleteBinLine action
    `style` [textFont "Remix", textMiddle, textColor trashFg, bgColor transparent, border 0 transparent]
    `hover` [bgColor trashBg]
  dropTicker pair widget = dropTarget_ (TickerMovePair pair) [dropTargetStyle [bgColor darkGray]] widget
  tickerPct t = pctLabel where
    diff = toRealFloat $ 100 * (t ^. close - t ^. open)
    pct = diff / toRealFloat (t ^. open)
    pctText = formatTickerPct (fromFloatDigits pct) <> "%"
    pctColor
      | abs pct < 0.01 = rgbHex "#428FE0"
      | pct > 0 = rgbHex "#51A39A"
      | otherwise = rgbHex "#E25141"
    pctLabel = label pctText `style` [width 100, textRight, textColor pctColor]
  tickerItem idx t = hstack [
      dropTicker (t ^. symbolPair) $
        draggable_ (t ^. symbolPair) [draggableStyle [bgColor dragColor]] $
          hstack [
            label (t ^. symbolPair) `style` [width 100],
            spacer,
            label (formatTickerValue (t ^. close))
              `style` [textRight, minWidth 100],
            spacer,
            tickerPct t
          ] `style` [cursorHand],
      spacer,
      trashIcon (TickerRemovePairBegin (t ^. symbolPair))
    ] `style` [padding 10, borderB 1 rowSep]
      `hover` [bgColor rowBg]
  tickerList = vstack tickerRows where
    orderedTickers = (\e -> model ^? tickers . ix e) <$> model ^. symbolPairs
    tickerFade idx t = fadeOut_ [onFinished action] item `key` (t ^. symbolPair) where
      action = TickerRemovePair (t ^. symbolPair)
      item = tickerItem idx t
    tickerRows = zipWith tickerFade [0..] (catMaybes orderedTickers)
  widgetTree = vstack [
      hstack [
        label "New pair: ",
        keystroke [("Enter", TickerAddClick)] $ textField newPair `key` "newPair",
        spacer,
        button "Add" TickerAddClick
      ] `style` [padding 20, bgColor sectionColor],
      scroll_ [scrollOverlay] $ tickerList `style` [padding 20, paddingT 10]
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
  TickerAddClick -> [
    Model $ model
      & symbolPairs %~ (model ^. newPair <|)
      & newPair .~ "",
    Task $ subscribe env [model ^. newPair],
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
      appTheme customDarkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Remix" "./assets/fonts/remixicon.ttf",
      appInitEvent TickerInit
      ]
    initModel = def

customLightTheme :: Theme
customLightTheme = lightTheme
  & L.userColorMap . at "rowBg" ?~ rgbHex "#ECECEC"
  & L.userColorMap . at "trashBg" ?~ rgbHex "#D3D3D3"
  & L.userColorMap . at "trashFg" ?~ rgbHex "#808080"

customDarkTheme :: Theme
customDarkTheme = darkTheme
  & L.userColorMap . at "rowBg" ?~ rgbHex "#515151"
  & L.userColorMap . at "trashBg" ?~ rgbHex "#656565"
  & L.userColorMap . at "trashFg" ?~ rgbHex "#909090"

formatTickerValue :: Scientific -> Text
formatTickerValue = T.pack . formatScientific Fixed (Just 8)

formatTickerPct :: Scientific -> Text
formatTickerPct = T.pack . formatScientific Fixed (Just 2)

initialList :: [Text]
initialList = ["BTCUSDT", "ETHBTC", "BNBBTC", "ADABTC", "DOTBTC", "XRPBTC",
  "UNIBTC", "LTCBTC", "LINKBTC", "BCHBTC", "DOGEBTC", "THETABTC", "LUNABTC",
  "AAVEBTC", "CROBTC", "VETBTC", "XMRBTC", "ATOMBTC", "FTTBTC", "SOLBTC",
  "SXPBTC", "BATBTC", "VETBTC", "REEFBTC"]
