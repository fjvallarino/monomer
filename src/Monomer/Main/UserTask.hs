{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.UserTask where

import Control.Concurrent.Async (async, poll)
import Control.Exception.Base
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Maybe
import Lens.Micro.Mtl

import Monomer.Common.Core

launchUserTasks :: MonomerM a e m => [IO (Maybe e)] -> m ()
launchUserTasks handlers = do
  tasks <- forM handlers $ \handler -> do
    asyncTask <- liftIO $ async handler

    return $ UserTask asyncTask

  previousTasks <- use userTasks
  userTasks .= previousTasks ++ tasks

checkUserTasks :: MonomerM a e m => m [e]
checkUserTasks = do
  tasks <- use userTasks
  (active, finished) <- partitionM (\(UserTask task) -> fmap isNothing (liftIO $ poll task)) tasks
  userTasks .= active

  processUserTaskHandlers finished

processUserTaskHandlers :: MonomerM a e m => [UserTask (Maybe e)] -> m [e]
processUserTaskHandlers tasks = do
  results <- forM tasks processUserTaskHandler
  return $ catMaybes results

processUserTaskHandler :: MonomerM a e m => UserTask (Maybe e) -> m (Maybe e)
processUserTaskHandler (UserTask task) = do
  taskStatus <- liftIO $ poll task

  return $ maybe Nothing processUserTaskHandlerResult taskStatus

processUserTaskHandlerResult :: Either SomeException (Maybe e) -> Maybe e
processUserTaskHandlerResult (Left _) = Nothing
processUserTaskHandlerResult (Right Nothing) = Nothing
processUserTaskHandlerResult (Right evt) = evt
