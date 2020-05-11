module Monomer.Widget.Internal where

import Data.Default

import qualified Data.Sequence as SQ

import Monomer.Data.Tree
import Monomer.Event.Types
import Monomer.Widget.Types

type ChildrenSelector s e m a = a -> SQ.Seq (WidgetNode s e m) -> (a, Maybe Int)

data EventsParent s e m = EventsParent {
  epIgnoreChildrenEvents :: Bool,
  epIgnoreParentEvents :: Bool,
  epEventRequests :: [(Path, EventRequest)],
  epUserEvents :: [e],
  epUpdatedNode :: Maybe (Tree (WidgetInstance s e m)),
  epNewStates :: [s -> s]
}

data EventsChildren s e m = EventsChildren {
  ecIgnoreParentEvents :: Bool,
  ecEventRequests :: [(Path, EventRequest)],
  ecUserEvents :: [e],
  ecUpdatedNode :: Maybe (Tree (WidgetInstance s e m)),
  ecNewStates :: [s -> s],
  ecNodePosition :: Int
}

defaultWidgetInstance :: (Monad m) => Widget s e m -> WidgetInstance s e m
defaultWidgetInstance widget = WidgetInstance {
  _widgetInstanceKey = Nothing,
  _widgetInstanceWidget = widget,
  _widgetInstanceEnabled = True,
  _widgetInstanceVisible = True,
  _widgetInstanceFocused = False,
  _widgetInstanceViewport = def,
  _widgetInstanceRenderArea = def,
  _widgetInstanceStyle = mempty
}
