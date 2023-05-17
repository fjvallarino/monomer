{-|
Module      : Monomer.Main.WidgetTask
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Handles the lifecycle and reporting of generated events of WidgetTasks (single
message) and Producers (multiple messages).
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict #-}

module Monomer.Main.WidgetTask (handleWidgetTasks) where

import Control.Concurrent.Async (poll)
import Control.Concurrent.STM.TChan (tryReadTChan)
import Control.Exception.Base
import Control.Lens ((^.), (.=), use)
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.STM (atomically)
import Data.Foldable (toList)
import Data.Maybe
import Data.Typeable

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Helper (collectJustM, putStrLnErr)
import Monomer.Main.Handlers
import Monomer.Main.Lens
import Monomer.Main.Util
import Monomer.Main.Types

import qualified Monomer.Core.Lens as L

-- | Checks the status and collects results of active tasks.
handleWidgetTasks
  :: MonomerM s e m
  => WidgetEnv s e        -- ^ The widget environment.
  -> WidgetNode s e       -- ^ The widget root.
  -> m (HandlerStep s e)  -- ^ The updated "Monomer.Main.Handlers.HandlerStep".
handleWidgetTasks wenv widgetRoot = do
  tasks <- use widgetTasks
  (active, finished) <- partitionM isThreadActive (toList tasks)
  widgetTasks .= Seq.fromList active

  processTasks wenv widgetRoot tasks

processTasks
  :: (MonomerM s e m, Traversable t)
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
  :: MonomerM s e m
  => WidgetEnv s e
  -> WidgetNode s e
  -> WidgetTask
  -> m (HandlerStep s e)
processTask wenv widgetRoot (WidgetTask widgetId task) = do
  taskStatus <- liftIO $ poll task

  case taskStatus of
    Just taskRes -> processTaskResult wenv widgetRoot widgetId taskRes
    Nothing -> return (wenv, widgetRoot, Seq.empty)
processTask wenv widgetRoot (WidgetProducer widgetId channel task) = do
  channelStatus <- collectJustM . liftIO . atomically $ tryReadTChan channel

  foldM processMsg (wenv, widgetRoot, Seq.empty) channelStatus
  where
    processMsg (wenv1, root1, reqs1) taskMsg = do
      (wenv2, root2, reqs2) <- processTaskEvent wenv1 root1 widgetId taskMsg
      return (wenv2, root2, reqs1 <> reqs2)

processTaskResult
  :: (MonomerM s e m, Typeable i)
  => WidgetEnv s e
  -> WidgetNode s e
  -> WidgetId
  -> Either SomeException i
  -> m (HandlerStep s e)
processTaskResult wenv widgetRoot _ (Left ex) = do
  liftIO . putStrLnErr $ "Error processing Widget task result: " ++ show ex
  return (wenv, widgetRoot, Seq.empty)
processTaskResult wenv widgetRoot widgetId (Right taskResult)
  = processTaskEvent wenv widgetRoot widgetId taskResult

processTaskEvent
  :: (MonomerM s e m, Typeable i)
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

isThreadActive :: MonomerM s e m => WidgetTask -> m Bool
isThreadActive (WidgetTask _ task) = fmap isNothing (liftIO $ poll task)
isThreadActive (WidgetProducer _ _ task) = fmap isNothing (liftIO $ poll task)

taskWidgetId :: WidgetTask -> WidgetId
taskWidgetId (WidgetTask widgetId _) = widgetId
taskWidgetId (WidgetProducer widgetId _ _) = widgetId
