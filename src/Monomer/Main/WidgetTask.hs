{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.WidgetTask where

import Control.Concurrent.Async (poll)
import Control.Exception.Base
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
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

handleWidgetTasks :: (MonomerM s m) => Renderer m -> s -> WidgetInstance s e m -> m (HandlerStep s e m)
handleWidgetTasks renderer app widgetRoot = do
  tasks <- use widgetTasks
  (active, finished) <- partitionM (\(WidgetTask _ task) -> fmap isNothing (liftIO $ poll task)) (toList tasks)
  widgetTasks .= Seq.fromList active

  processWidgetTasks renderer app widgetRoot finished

processWidgetTasks :: (MonomerM s m) => Renderer m -> s -> WidgetInstance s e m -> [WidgetTask] -> m (HandlerStep s e m)
processWidgetTasks renderer app widgetRoot tasks = foldM reducer (app, Seq.empty, widgetRoot) tasks where
  reducer (wApp, wEvts, wRoot) task = do
    (wApp2, wEvts2, wRoot2) <- processWidgetTask renderer wApp wRoot task
    return (wApp2, wEvts >< wEvts2, wRoot2)

processWidgetTask :: (MonomerM s m) => Renderer m -> s -> WidgetInstance s e m -> WidgetTask -> m (HandlerStep s e m)
processWidgetTask renderer app widgetRoot (WidgetTask path task) = do
  taskStatus <- liftIO $ poll task

  if (isJust taskStatus)
    then processWidgetTaskResult renderer app widgetRoot path (fromJust taskStatus)
    else return (app, Seq.empty, widgetRoot)

processWidgetTaskResult :: (MonomerM s m, Typeable a) => Renderer m -> s -> WidgetInstance s e m -> Path -> Either SomeException a -> m (HandlerStep s e m)
processWidgetTaskResult renderer app widgetRoot _ (Left _) = return (app, Seq.empty, widgetRoot)
processWidgetTaskResult renderer app widgetRoot path (Right val) = do
  currentFocus <- use focused

  let ctx = PathContext currentFocus path rootPath
  let emptyResult = EventResult Seq.empty Seq.empty widgetRoot
  let eventResult = fromMaybe emptyResult $ _widgetHandleCustom (_instanceWidget widgetRoot) ctx val app widgetRoot

  handleEventResult renderer ctx app eventResult
