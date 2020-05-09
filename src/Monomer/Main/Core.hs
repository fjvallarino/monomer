{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Core where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Maybe
import Lens.Micro.Mtl

import qualified Data.Map as M
import qualified SDL
import qualified NanoVG as NV

import Monomer.Common.Core
import Monomer.Common.Event
import Monomer.Common.Types
import Monomer.Common.Util
import Monomer.Main.Platform
import Monomer.Main.UserTask
import Monomer.Main.Util
import Monomer.Platform.NanoVGRenderer
import Monomer.Widgets

handleAppEvents :: (MonomerM s e m) => MonomerApp s e m -> [e] -> m ()
handleAppEvents mapp events = do
  app <- use appContext
  let (newApp, tasks) = reduceAppEvents (_appEventHandler mapp) app events

  appContext .= newApp
  launchUserTasks tasks

reduceAppEvents :: AppEventHandler s e -> s -> [e] -> (s, [IO (Maybe e)])
reduceAppEvents appEventHandler app events = foldl reducer (app, []) events where
  reducer (app, tasks) event = case appEventHandler app event of
    State newApp -> (newApp, tasks)
    StateEvent newApp newEvent -> reducer (newApp, tasks) newEvent
    Task newApp task -> (newApp, task : tasks)
