{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.WidgetTask (handleWidgetTasks) where

import Debug.Trace

import Control.Concurrent.Async (poll)
import Control.Concurrent.STM.TChan (tryReadTChan)
import Control.Exception.Base
import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.STM (atomically)
import Data.Foldable (toList)
import Data.Maybe
import Data.Sequence ((><))
import Data.Typeable

import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Event.Core
import Monomer.Event.Types
import Monomer.Graphics.Renderer
import Monomer.Main.Handlers
import Monomer.Main.Util
import Monomer.Main.Types
import Monomer.Widget.WidgetContext
import Monomer.Widget.Types

handleWidgetTasks :: (MonomerM s m) => Renderer m -> WidgetEnv s e -> WidgetInstance s e -> m (HandlerStep s e)
handleWidgetTasks renderer wenv widgetRoot = do
  tasks <- use widgetTasks
  (active, finished) <- partitionM isThreadActive (toList tasks)
  widgetTasks .= Seq.fromList active

  processWidgetTasks renderer wenv widgetRoot tasks

processWidgetTasks :: (MonomerM s m, Traversable t) => Renderer m -> WidgetEnv s e -> WidgetInstance s e -> t WidgetTask -> m (HandlerStep s e)
processWidgetTasks renderer wenv widgetRoot tasks = foldM reducer (wenv, Seq.empty, widgetRoot) tasks where
  reducer (wWctx, wEvts, wRoot) task = do
    (wWctx2, wEvts2, wRoot2) <- processWidgetTask renderer wWctx wRoot task
    return (wWctx2, wEvts >< wEvts2, wRoot2)

processWidgetTask :: (MonomerM s m) => Renderer m -> WidgetEnv s e -> WidgetInstance s e -> WidgetTask -> m (HandlerStep s e)
processWidgetTask renderer wenv widgetRoot (WidgetTask path task) = do
  taskStatus <- liftIO $ poll task

  case taskStatus of
    Just taskResult -> processWidgetTaskResult renderer wenv widgetRoot path taskResult
    Nothing -> return (wenv, Seq.empty, widgetRoot)
processWidgetTask renderer model widgetRoot (WidgetProducer path channel task) = do
  channelStatus <- liftIO . atomically $ tryReadTChan channel

  case channelStatus of
    Just taskMessage -> processWidgetTaskEvent renderer model widgetRoot path taskMessage
    Nothing -> return (model, Seq.empty, widgetRoot)

processWidgetTaskResult :: (MonomerM s m, Typeable a) => Renderer m -> WidgetEnv s e -> WidgetInstance s e -> Path -> Either SomeException a -> m (HandlerStep s e)
processWidgetTaskResult renderer wenv widgetRoot _ (Left ex) = do
  liftIO . putStrLn $ "Error processing Widget task result: " ++ show ex
  return (wenv, Seq.empty, widgetRoot)
processWidgetTaskResult renderer wenv widgetRoot path (Right taskResult) = processWidgetTaskEvent renderer wenv widgetRoot path taskResult

processWidgetTaskEvent :: (MonomerM s m, Typeable a) => Renderer m -> WidgetEnv s e -> WidgetInstance s e -> Path -> a -> m (HandlerStep s e)
processWidgetTaskEvent renderer wenv widgetRoot path event = do
  currentFocus <- use focused

  let emptyResult = WidgetResult Seq.empty Seq.empty widgetRoot
  let widgetResult = fromMaybe emptyResult $ _widgetHandleMessage (_instanceWidget widgetRoot) wenv path event widgetRoot

  handleWidgetResult renderer wenv widgetResult

isThreadActive :: (MonomerM s m) => WidgetTask -> m Bool
isThreadActive (WidgetTask _ task) = fmap isNothing (liftIO $ poll task)
isThreadActive (WidgetProducer _ _ task) = fmap isNothing (liftIO $ poll task)
