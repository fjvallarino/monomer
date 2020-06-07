module Monomer.Main.App (createApp) where

import Debug.Trace

import Control.Monad.State
import Data.Default
import Data.Typeable (Typeable)
import Lens.Micro
import TextShow

import Monomer.Common.Style
import Monomer.Graphics.Color
import Monomer.Main.Types
import Monomer.Widget.CompositeWidget
import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widgets

createApp :: (Eq s, Monad m, Typeable s, Typeable e, Typeable m) => s -> AppEventHandler s e -> UIBuilder s e m -> WidgetInstance s e m
createApp app eventHandler uiBuilder = composite "app" app (handleAppEvent eventHandler) uiBuilder

handleAppEvent :: AppEventHandler s e -> s -> e -> EventResponseC s e e
handleAppEvent eventHandler app evt = case eventHandler app evt of
  State newApp -> StateC newApp
  StateEvent newApp newEvent -> StateEventC newApp newEvent
  Task newApp newAction -> TaskC newApp newAction
