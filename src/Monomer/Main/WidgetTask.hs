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
import Monomer.Widget.PathContext
import Monomer.Widget.Types

handleWidgetTasks :: (MonomerM s m) => Renderer m -> WidgetContext s e -> WidgetInstance s e -> m (HandlerStep s e)
handleWidgetTasks renderer wctx widgetRoot = do
  tasks <- use widgetTasks
  (active, finished) <- partitionM isThreadActive (toList tasks)
  widgetTasks .= Seq.fromList active

  processWidgetTasks renderer wctx widgetRoot tasks

processWidgetTasks :: (MonomerM s m, Traversable t) => Renderer m -> WidgetContext s e -> WidgetInstance s e -> t WidgetTask -> m (HandlerStep s e)
processWidgetTasks renderer wctx widgetRoot tasks = foldM reducer (wctx, Seq.empty, widgetRoot) tasks where
  reducer (wWctx, wEvts, wRoot) task = do
    (wWctx2, wEvts2, wRoot2) <- processWidgetTask renderer wWctx wRoot task
    return (wWctx2, wEvts >< wEvts2, wRoot2)

processWidgetTask :: (MonomerM s m) => Renderer m -> WidgetContext s e -> WidgetInstance s e -> WidgetTask -> m (HandlerStep s e)
processWidgetTask renderer wctx widgetRoot (WidgetTask path task) = do
  taskStatus <- liftIO $ poll task

  case taskStatus of
    Just taskResult -> processWidgetTaskResult renderer wctx widgetRoot path taskResult
    Nothing -> return (wctx, Seq.empty, widgetRoot)
processWidgetTask renderer app widgetRoot (WidgetProducer path channel task) = do
  channelStatus <- liftIO . atomically $ tryReadTChan channel

  case channelStatus of
    Just taskMessage -> processWidgetTaskEvent renderer app widgetRoot path taskMessage
    Nothing -> return (app, Seq.empty, widgetRoot)

processWidgetTaskResult :: (MonomerM s m, Typeable a) => Renderer m -> WidgetContext s e -> WidgetInstance s e -> Path -> Either SomeException a -> m (HandlerStep s e)
processWidgetTaskResult renderer wctx widgetRoot _ (Left ex) = do
  liftIO . putStrLn $ "Error processing Widget task result: " ++ show ex
  return (wctx, Seq.empty, widgetRoot)
processWidgetTaskResult renderer wctx widgetRoot path (Right taskResult) = processWidgetTaskEvent renderer wctx widgetRoot path taskResult

processWidgetTaskEvent :: (MonomerM s m, Typeable a) => Renderer m -> WidgetContext s e -> WidgetInstance s e -> Path -> a -> m (HandlerStep s e)
processWidgetTaskEvent renderer wctx widgetRoot path event = do
  currentFocus <- use focused

  let ctx = PathContext currentFocus path rootPath
  let emptyResult = WidgetResult Seq.empty Seq.empty widgetRoot
  let widgetResult = fromMaybe emptyResult $ _widgetHandleMessage (_instanceWidget widgetRoot) wctx ctx event widgetRoot

  handleWidgetResult renderer wctx ctx widgetResult

isThreadActive :: (MonomerM s m) => WidgetTask -> m Bool
isThreadActive (WidgetTask _ task) = fmap isNothing (liftIO $ poll task)
isThreadActive (WidgetProducer _ _ task) = fmap isNothing (liftIO $ poll task)
