{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.WidgetTask (handleWidgetTasks) where

import Debug.Trace

import Control.Concurrent.Async (poll)
import Control.Concurrent.STM.TChan (tryReadTChan)
import Control.Exception.Base
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.STM (atomically)
import Data.Foldable (toList)
import Data.Maybe
import Data.Sequence ((><))
import Data.Typeable
import Lens.Micro.Mtl

import qualified Data.List as L
import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Event.Core
import Monomer.Event.Types
import Monomer.Graphics.Renderer
import Monomer.Main.Handlers
import Monomer.Main.Util
import Monomer.Main.Types
import Monomer.Widget.Core
import Monomer.Widget.PathContext
import Monomer.Widget.Types

handleWidgetTasks :: (MonomerM s m) => Renderer m -> s -> WidgetInstance s e -> m (HandlerStep s e)
handleWidgetTasks renderer app widgetRoot = do
  tasks <- use widgetTasks
  (active, finished) <- partitionM isThreadActive (toList tasks)
  widgetTasks .= Seq.fromList active

  processWidgetTasks renderer app widgetRoot tasks

processWidgetTasks :: (MonomerM s m, Traversable t) => Renderer m -> s -> WidgetInstance s e -> t WidgetTask -> m (HandlerStep s e)
processWidgetTasks renderer app widgetRoot tasks = foldM reducer (app, Seq.empty, widgetRoot) tasks where
  reducer (wApp, wEvts, wRoot) task = do
    (wApp2, wEvts2, wRoot2) <- processWidgetTask renderer wApp wRoot task
    return (wApp2, wEvts >< wEvts2, wRoot2)

processWidgetTask :: (MonomerM s m) => Renderer m -> s -> WidgetInstance s e -> WidgetTask -> m (HandlerStep s e)
processWidgetTask renderer app widgetRoot (WidgetTask path task) = do
  taskStatus <- liftIO $ poll task

  case taskStatus of
    Just taskResult -> processWidgetTaskResult renderer app widgetRoot path taskResult
    Nothing -> return (app, Seq.empty, widgetRoot)
processWidgetTask renderer app widgetRoot (WidgetProducer path channel task) = do
  channelStatus <- liftIO . atomically $ tryReadTChan channel

  case channelStatus of
    Just taskMessage -> processWidgetTaskEvent renderer app widgetRoot path taskMessage
    Nothing -> return (app, Seq.empty, widgetRoot)

processWidgetTaskResult :: (MonomerM s m, Typeable a) => Renderer m -> s -> WidgetInstance s e -> Path -> Either SomeException a -> m (HandlerStep s e)
processWidgetTaskResult renderer app widgetRoot _ (Left ex) = do
  liftIO . putStrLn $ "Error processing Widget task result: " ++ show ex
  return (app, Seq.empty, widgetRoot)
processWidgetTaskResult renderer app widgetRoot path (Right taskResult) = processWidgetTaskEvent renderer app widgetRoot path taskResult

processWidgetTaskEvent :: (MonomerM s m, Typeable a) => Renderer m -> s -> WidgetInstance s e -> Path -> a -> m (HandlerStep s e)
processWidgetTaskEvent renderer app widgetRoot path event = do
  currentFocus <- use focused

  let ctx = PathContext currentFocus path rootPath
  let emptyResult = WidgetResult Seq.empty Seq.empty widgetRoot
  let widgetResult = fromMaybe emptyResult $ _widgetHandleCustom (_instanceWidget widgetRoot) ctx event app widgetRoot

  handleWidgetResult renderer ctx app widgetResult

isThreadActive :: (MonomerM s m) => WidgetTask -> m Bool
isThreadActive (WidgetTask _ task) = fmap isNothing (liftIO $ poll task)
isThreadActive (WidgetProducer _ _ task) = fmap isNothing (liftIO $ poll task)
