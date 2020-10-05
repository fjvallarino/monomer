{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.WidgetTask (handleWidgetTasks) where

import Control.Concurrent.Async (poll)
import Control.Concurrent.STM.TChan (tryReadTChan)
import Control.Exception.Base
import Control.Lens
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.STM (atomically)
import Data.Foldable (toList)
import Data.Maybe
import Data.Sequence ((><))
import Data.Typeable

import qualified Data.Sequence as Seq

import Monomer.Core.BasicTypes
import Monomer.Core.Types
import Monomer.Main.Handlers
import Monomer.Main.Types

handleWidgetTasks
  :: (MonomerM s m)
  => WidgetEnv s e -> WidgetInstance s e -> m (HandlerStep s e)
handleWidgetTasks wenv widgetRoot = do
  tasks <- use widgetTasks
  (active, finished) <- partitionM isThreadActive (toList tasks)
  widgetTasks .= Seq.fromList active

  processTasks wenv widgetRoot tasks

processTasks
  :: (MonomerM s m, Traversable t)
  => WidgetEnv s e
  -> WidgetInstance s e
  -> t WidgetTask
  -> m (HandlerStep s e)
processTasks wenv widgetRoot tasks = nextStep where
  reducer (wWctx, wEvts, wRoot) task = do
    (wWctx2, wEvts2, wRoot2) <- processTask wWctx wRoot task
    return (wWctx2, wEvts >< wEvts2, wRoot2)
  nextStep = foldM reducer (wenv, Seq.empty, widgetRoot) tasks

processTask
  :: (MonomerM s m)
  => WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetTask
  -> m (HandlerStep s e)
processTask wenv widgetRoot (WidgetTask path task) = do
  taskStatus <- liftIO $ poll task

  case taskStatus of
    Just taskRes -> processTaskResult wenv widgetRoot path taskRes
    Nothing -> return (wenv, Seq.empty, widgetRoot)
processTask model widgetRoot (WidgetProducer path channel task) = do
  channelStatus <- liftIO . atomically $ tryReadTChan channel

  case channelStatus of
    Just taskMsg -> processTaskEvent model widgetRoot path taskMsg
    Nothing -> return (model, Seq.empty, widgetRoot)

processTaskResult
  :: (MonomerM s m, Typeable a)
  => WidgetEnv s e
  -> WidgetInstance s e
  -> Path
  -> Either SomeException a
  -> m (HandlerStep s e)
processTaskResult wenv widgetRoot _ (Left ex) = do
  liftIO . putStrLn $ "Error processing Widget task result: " ++ show ex
  return (wenv, Seq.empty, widgetRoot)
processTaskResult wenv widgetRoot path (Right taskResult)
  = processTaskEvent wenv widgetRoot path taskResult

processTaskEvent
  :: (MonomerM s m, Typeable a)
  => WidgetEnv s e
  -> WidgetInstance s e
  -> Path
  -> a
  -> m (HandlerStep s e)
processTaskEvent wenv widgetRoot path event = do
  currentFocus <- use pathFocus

  let emptyResult = WidgetResult Seq.empty Seq.empty widgetRoot
  let widget = _wiWidget widgetRoot
  let msgResult = widgetHandleMessage widget wenv path event widgetRoot
  let widgetResult = fromMaybe emptyResult msgResult

  handleWidgetResult wenv widgetResult

isThreadActive :: (MonomerM s m) => WidgetTask -> m Bool
isThreadActive (WidgetTask _ task) = fmap isNothing (liftIO $ poll task)
isThreadActive (WidgetProducer _ _ task) = fmap isNothing (liftIO $ poll task)
