{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tutorial03 where

import Control.Concurrent (threadDelay)
import Control.Lens
import Data.Text (Text)
import Data.Time
import Monomer
import System.Random

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
  widgetTree = vstack [
      label (T.takeWhile (/= '.') timeString)
        `style` [textFont "Bold", textSize 80, textCenter, textMiddle, flexHeight 100]
    ]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> [Producer timeOfDayProducer]
  AppSetTime value -> [Model $ model & currentTime .~ value]

timeOfDayProducer :: (AppEvent -> IO ()) -> IO ()
timeOfDayProducer sendMsg = do
  time <- getLocalTimeOfDay
  sendMsg (AppSetTime time)
  threadDelay (1000 * 1000)
  timeOfDayProducer sendMsg

getLocalTimeOfDay :: IO TimeOfDay
getLocalTimeOfDay = do
  time <- getZonedTime
  return . localTimeOfDay . zonedTimeToLocalTime $ time

main03 :: IO ()
main03 = do
  time <- getLocalTimeOfDay
  simpleApp (model time) handleEvent buildUI config
  where
    config = [
      appWindowTitle "Tutorial 03",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appInitEvent AppInit
      ]
    model time = AppModel {
      _currentTime = time
    }
