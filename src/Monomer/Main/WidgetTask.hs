{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.WidgetTask where

import Control.Concurrent.Async (async, poll)
import Control.Exception.Base
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Foldable (toList)
import Data.Maybe
import Data.Sequence (Seq(..), (<|), (|>), (><))
import Data.Typeable
import Lens.Micro.Mtl

import qualified Data.List as L
import qualified Data.Sequence as Seq

import Monomer.Common.Types
import Monomer.Common.Util
import Monomer.Common.Tree
import Monomer.Event.Core
import Monomer.Event.Types
import Monomer.Main.Internal
import Monomer.Main.Util
import Monomer.Main.Types
import Monomer.Widget.Core
import Monomer.Widget.PathContext
import Monomer.Widget.Types

launchWidgetTasks :: (MonomerM s e m) => Seq (EventRequest s) -> m ()
launchWidgetTasks eventRequests = do
  let customHandlers = Seq.filter isCustomHandler eventRequests

  tasks <- forM customHandlers $ \(RunCustom path handler) -> do
    asyncTask <- liftIO $ async (liftIO handler)

    return $ WidgetTask path asyncTask

  previousTasks <- use widgetTasks
  widgetTasks .= previousTasks >< tasks

checkWidgetTasks :: (MonomerM s e m) => MonomerApp s e m -> WidgetInstance s e m -> m (WidgetInstance s e m, Seq e, Bool)
checkWidgetTasks mapp widgets = do
  tasks <- use widgetTasks
  (active, finished) <- partitionM (\(WidgetTask _ task) -> fmap isNothing (liftIO $ poll task)) (toList tasks)
  widgetTasks .= Seq.fromList active

  processCustomHandlers mapp widgets finished

processCustomHandlers :: (MonomerM s e m) => MonomerApp s e m -> WidgetInstance s e m -> [WidgetTask] -> m (WidgetInstance s e m, Seq e, Bool)
processCustomHandlers mapp widgets tasks = foldM reducer (widgets, Seq.empty, False) tasks where
  reducer (ws, es, resize) task = do
    (ws2, es2, resize2) <- processCustomHandler mapp ws task
    return (ws2, es >< es2, resize || resize2)

processCustomHandler :: (MonomerM s e m) => MonomerApp s e m -> WidgetInstance s e m -> WidgetTask -> m (WidgetInstance s e m, Seq e, Bool)
processCustomHandler mapp widgets (WidgetTask path task) = do
  app <- use appContext
  taskStatus <- liftIO $ poll task

  if (isJust taskStatus)
    then processCustomHandlerResult mapp app widgets path (fromJust taskStatus)
    else return (widgets, Seq.empty, False)

processCustomHandlerResult :: (MonomerM s e m, Typeable a) => MonomerApp s e m -> s -> WidgetInstance s e m -> Path -> Either SomeException a -> m (WidgetInstance s e m, Seq e, Bool)
processCustomHandlerResult mapp app widgetRoot _ (Left _) = return (widgetRoot, Seq.empty, False)
processCustomHandlerResult mapp app widgetRoot path (Right val) = do
  currentFocus <- use focused

  let ctx = PathContext currentFocus path rootPath
  let emptyResult = EventResult Seq.empty Seq.empty widgetRoot
  let EventResult eventRequests appEvents newWidgetRoot = fromMaybe emptyResult $ _widgetHandleCustom (_instanceWidget widgetRoot) ctx val app widgetRoot
  let resizeRequested = isJust $ Seq.findIndexL isResizeChildren eventRequests
  let newStates = getUpdateUserStates eventRequests

  appContext %= compose newStates
  launchWidgetTasks eventRequests

  return (newWidgetRoot, appEvents, resizeRequested)
