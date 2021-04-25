{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.WidgetTask (handleWidgetTasks) where

import Control.Concurrent.Async (poll)
import Control.Concurrent.STM.TChan (tryReadTChan)
import Control.Exception.Base
import Control.Lens ((&), (^.), (.=), use)
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.STM (atomically)
import Data.Foldable (toList)
import Data.Maybe
import Data.Typeable

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Main.Handlers
import Monomer.Main.Lens
import Monomer.Main.Util
import Monomer.Main.Types

import qualified Monomer.Lens as L

handleWidgetTasks
  :: (MonomerM s m)
  => WidgetEnv s e -> WidgetNode s e -> m (HandlerStep s e)
handleWidgetTasks wenv widgetRoot = do
  tasks <- use widgetTasks
  (active, finished) <- partitionM isThreadActive (toList tasks)
  widgetTasks .= Seq.fromList active

  processTasks wenv widgetRoot tasks

processTasks
  :: (MonomerM s m, Traversable t)
  => WidgetEnv s e
  -> WidgetNode s e
  -> t WidgetTask
  -> m (HandlerStep s e)
processTasks wenv widgetRoot tasks = nextStep where
  reducer (wenv1, root1, reqs1) task = do
    (wenv2, root2, reqs2) <- processTask wenv1 root1 task
    return (wenv2, root2, reqs1 <> reqs2)
  nextStep = foldM reducer (wenv, widgetRoot, Seq.empty) tasks

processTask
  :: (MonomerM s m)
  => WidgetEnv s e
  -> WidgetNode s e
  -> WidgetTask
  -> m (HandlerStep s e)
processTask wenv widgetRoot (WidgetTask widgetId task) = do
  taskStatus <- liftIO $ poll task

  case taskStatus of
    Just taskRes -> processTaskResult wenv widgetRoot widgetId taskRes
    Nothing -> return (wenv, widgetRoot, Seq.empty)
processTask model widgetRoot (WidgetProducer widgetId channel task) = do
  channelStatus <- liftIO . atomically $ tryReadTChan channel

  case channelStatus of
    Just taskMsg -> processTaskEvent model widgetRoot widgetId taskMsg
    Nothing -> return (model, widgetRoot, Seq.empty)

processTaskResult
  :: (MonomerM s m, Typeable i)
  => WidgetEnv s e
  -> WidgetNode s e
  -> WidgetId
  -> Either SomeException i
  -> m (HandlerStep s e)
processTaskResult wenv widgetRoot _ (Left ex) = do
  liftIO . putStrLn $ "Error processing Widget task result: " ++ show ex
  return (wenv, widgetRoot, Seq.empty)
processTaskResult wenv widgetRoot widgetId (Right taskResult)
  = processTaskEvent wenv widgetRoot widgetId taskResult

processTaskEvent
  :: (MonomerM s m, Typeable i)
  => WidgetEnv s e
  -> WidgetNode s e
  -> WidgetId
  -> i
  -> m (HandlerStep s e)
processTaskEvent wenv widgetRoot widgetId event = do
  path <- getWidgetIdPath widgetId

  let emptyResult = WidgetResult widgetRoot Seq.empty
  let widget = widgetRoot ^. L.widget
  let msgResult = widgetHandleMessage widget wenv widgetRoot path event
  let widgetResult = fromMaybe emptyResult msgResult

  handleWidgetResult wenv True widgetResult

isThreadActive :: MonomerM s m => WidgetTask -> m Bool
isThreadActive (WidgetTask _ task) = fmap isNothing (liftIO $ poll task)
isThreadActive (WidgetProducer _ _ task) = fmap isNothing (liftIO $ poll task)

taskWidgetId :: WidgetTask -> WidgetId
taskWidgetId (WidgetTask widgetId _) = widgetId
taskWidgetId (WidgetProducer widgetId _ _) = widgetId
