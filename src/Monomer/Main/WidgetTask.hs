{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.WidgetTask where

import Control.Concurrent.Async (async, poll)
import Control.Exception.Base
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Maybe
import Data.Typeable
import Lens.Micro.Mtl

import qualified Data.List as L

import Monomer.Common.Core
import Monomer.Common.Event
import Monomer.Common.Types
import Monomer.Common.Util
import Monomer.Data.Tree
import Monomer.Main.Util

launchWidgetTasks :: (MonomerM s e m) => [(Path, EventRequest)] -> m ()
launchWidgetTasks eventRequests = do
  let customHandlers = L.filter isCustomHandler eventRequests

  tasks <- forM customHandlers $ \(path, RunCustom handler) -> do
    asyncTask <- liftIO $ async (liftIO handler)

    return $ WidgetTask path asyncTask

  previousTasks <- use widgetTasks
  widgetTasks .= previousTasks ++ tasks

checkWidgetTasks :: (MonomerM s e m) => MonomerApp s e m -> WidgetNode s e m -> m (WidgetNode s e m, [e], Bool)
checkWidgetTasks mapp widgets = do
  tasks <- use widgetTasks
  (active, finished) <- partitionM (\(WidgetTask _ task) -> fmap isNothing (liftIO $ poll task)) tasks
  widgetTasks .= active

  processCustomHandlers mapp widgets finished

processCustomHandlers :: (MonomerM s e m) => MonomerApp s e m -> WidgetNode s e m -> [WidgetTask] -> m (WidgetNode s e m, [e], Bool)
processCustomHandlers mapp widgets tasks = foldM reducer (widgets, [], False) tasks where
  reducer (ws, es, resize) task = do
    (ws2, es2, resize2) <- processCustomHandler mapp ws task
    return (ws2, es ++ es2, resize || resize2)

processCustomHandler :: (MonomerM s e m) => MonomerApp s e m -> WidgetNode s e m -> WidgetTask -> m (WidgetNode s e m, [e], Bool)
processCustomHandler mapp widgets (WidgetTask path task) = do
  app <- use appContext
  taskStatus <- liftIO $ poll task

  if (isJust taskStatus)
    then processCustomHandlerResult mapp app widgets path (fromJust taskStatus)
    else return (widgets, [], False)

processCustomHandlerResult :: (MonomerM s e m, Typeable a) => MonomerApp s e m -> s -> WidgetNode s e m -> Path -> Either SomeException a -> m (WidgetNode s e m, [e], Bool)
processCustomHandlerResult mapp app widgets _ (Left _) = return (widgets, [], False)
processCustomHandlerResult mapp app widgets path (Right val) = do
  let (ChildEventResult stopProcessing eventRequests appEvents newWidgets newStates) = handleCustomCommand app path widgets val
  let newRoot = fromMaybe widgets newWidgets
  let resizeRequested = isJust $ L.find (\(path, evt) -> isResizeChildren evt) eventRequests

  appContext %= compose newStates
  launchWidgetTasks eventRequests

  return (newRoot, appEvents, resizeRequested)
