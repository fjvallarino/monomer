{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.WidgetTask where

import Control.Concurrent.Async (poll)
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
import Monomer.Graphics.Renderer
import Monomer.Main.Handlers
import Monomer.Main.Internal
import Monomer.Main.Util
import Monomer.Main.Types
import Monomer.Widget.Core
import Monomer.Widget.PathContext
import Monomer.Widget.Types

handleWidgetTasks :: (MonomerM s e m) => Renderer m -> MonomerApp s e m -> s -> WidgetInstance s e m -> m (HandlerStep s e m)
handleWidgetTasks renderer mapp app widgetRoot = do
  tasks <- use widgetTasks
  (active, finished) <- partitionM (\(WidgetTask _ task) -> fmap isNothing (liftIO $ poll task)) (toList tasks)
  widgetTasks .= Seq.fromList active

  processWidgetTasks renderer mapp app widgetRoot finished

processWidgetTasks :: (MonomerM s e m) => Renderer m -> MonomerApp s e m -> s -> WidgetInstance s e m -> [WidgetTask] -> m (HandlerStep s e m)
processWidgetTasks renderer mapp app widgetRoot tasks = foldM reducer (app, Seq.empty, widgetRoot) tasks where
  reducer (wApp, wEvts, wRoot) task = do
    (wApp2, wEvts2, wRoot2) <- processWidgetTask renderer mapp wApp wRoot task
    return (wApp2, wEvts >< wEvts2, wRoot2)

processWidgetTask :: (MonomerM s e m) => Renderer m -> MonomerApp s e m -> s -> WidgetInstance s e m -> WidgetTask -> m (HandlerStep s e m)
processWidgetTask renderer mapp app widgetRoot (WidgetTask path task) = do
  app <- use appContext
  taskStatus <- liftIO $ poll task

  if (isJust taskStatus)
    then processWidgetTaskResult renderer mapp app widgetRoot path (fromJust taskStatus)
    else return (app, Seq.empty, widgetRoot)

processWidgetTaskResult :: (MonomerM s e m, Typeable a) => Renderer m -> MonomerApp s e m -> s -> WidgetInstance s e m -> Path -> Either SomeException a -> m (HandlerStep s e m)
processWidgetTaskResult renderer mapp app widgetRoot _ (Left _) = return (app, Seq.empty, widgetRoot)
processWidgetTaskResult renderer mapp app widgetRoot path (Right val) = do
  currentFocus <- use focused

  let ctx = PathContext currentFocus path rootPath
  let emptyResult = EventResult Seq.empty Seq.empty widgetRoot
  let eventResult = fromMaybe emptyResult $ _widgetHandleCustom (_instanceWidget widgetRoot) ctx val app widgetRoot

  handleEventResult renderer mapp ctx app eventResult
