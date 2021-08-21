{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tutorial05_Producers where

import Control.Exception (bracket)
import System.Remote.Monitoring
import Control.Concurrent

import Control.Concurrent (threadDelay)
import Control.Lens
import Data.Text (Text)
import Data.Time
import Monomer

import qualified Data.Text as T
import qualified Monomer.Lens as L

newtype AppModel = AppModel {
  _currentTime :: TimeOfDay
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppSetTime TimeOfDay
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  timeString = T.pack . show $ model ^. currentTime

--  timeLabel = label (T.takeWhile (/= '.') timeString)
--    `styleBasic` [textFont "Bold", textSize 80, textCenter, textMiddle, flexHeight 100]

  timeLabel = label (T.takeWhile (/= '.') timeString)
    `styleBasic` [textFont "Regular", textSize 20, textCenter, textMiddle, padding 10]

  timeLabels = vstack (replicate 10 timeLabel)

--  widgetTree = timeLabel
  widgetTree = vstack [
      animFadeIn timeLabels `nodeKey` "fadeTimeLabel"
    ]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> [Producer timeOfDayProducer]
  AppSetTime time -> fadeInMsg time ++ [Model $ model & currentTime .~ time]
  where
    fadeInMsg time
      | truncate (todSec time) `mod` 10 /= 0 = []
      | otherwise = [Message "fadeTimeLabel" AnimationStart]

timeOfDayProducer :: (AppEvent -> IO ()) -> IO ()
timeOfDayProducer sendMsg = do
  time <- getLocalTimeOfDay
  sendMsg (AppSetTime time)
  threadDelay $ 1000 * 10
  timeOfDayProducer sendMsg

getLocalTimeOfDay :: IO TimeOfDay
getLocalTimeOfDay = do
  time <- getZonedTime
  return . localTimeOfDay . zonedTimeToLocalTime $ time

main05 :: IO ()
main05 = do
  time <- getLocalTimeOfDay
  startApp (model time) handleEvent buildUI config
  where
    config = [
      appWindowTitle "Tutorial 05 - Producers",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appInitEvent AppInit
      ]
    model time = AppModel {
      _currentTime = time
    }

withMonitoring :: IO a -> IO a
withMonitoring action = do
  bracket
    startServer
    stopServer
    (const action)
  where
    startServer = forkServer "localhost" 28000
    stopServer server = killThread (serverThreadId server)
