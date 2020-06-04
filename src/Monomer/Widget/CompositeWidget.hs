module Monomer.Widget.CompositeWidget where

import Data.Maybe
import Data.Sequence (Seq(..))
import Data.Typeable (Typeable)

import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Tree
import Monomer.Event.Core
import Monomer.Event.Types
import Monomer.Graphics.Renderer
import Monomer.Widget.BaseContainer
import Monomer.Widget.PathContext
import Monomer.Widget.Types
import Monomer.Widget.Util

type UIBuilderC s e m = s -> WidgetInstance s e m
type EventHandlerC s e ep = s -> e -> EventResponseC s e ep

data EventResponseC s e ep = StateC s
                           | TaskC s (IO (Maybe e))
                           | MessageC ep

data CompositeState s e m = CompositeState {
  _compositeApp :: s,
  _compositeRoot :: WidgetInstance s e m
} deriving (Typeable)

composite :: (Monad m, Typeable s, Typeable e, Typeable m) => WidgetType -> s -> EventHandlerC s e ep -> UIBuilderC s e m -> WidgetInstance sp ep m
composite widgetType app eventHandler uiBuilder = defaultWidgetInstance widgetType widget where
  widgetRoot = uiBuilder app
  state = CompositeState app widgetRoot
  widget = createComposite state eventHandler

createComposite :: (Monad m, Typeable s, Typeable e, Typeable m) => CompositeState s e m -> EventHandlerC s e ep -> Widget sp ep m
createComposite state eventHandler = widget where
  CompositeState app widgetRoot = state
  widget = Widget {
    _widgetGetState = makeState state,-- state,
    _widgetMerge = compositeMerge state eventHandler, --containerMergeTrees ignoreOldInstance,
    _widgetNextFocusable = containerNextFocusable,
    _widgetFind = containerFind,
    _widgetHandleEvent = compositeHandleEvent state eventHandler, --containerHandleEvent ignoreEvent,
    _widgetHandleCustom = containerHandleCustom,
    _widgetPreferredSize = compositePreferredSize state, -- containerPreferredSize defaultPreferredSize,
    _widgetResize = compositeResize state eventHandler, -- containerResize defaultResize,
    _widgetRender = compositeRender state --containerRender
  }

compositeMerge :: (Monad m, Typeable s, Typeable e, Typeable m) => CompositeState s e m -> EventHandlerC s e ep -> sp -> WidgetInstance sp ep m -> WidgetInstance sp ep m -> WidgetInstance sp ep m
compositeMerge state eventHandler pApp newComposite oldComposite = newInstance where
  oldState = _widgetGetState (_instanceWidget oldComposite) pApp
  CompositeState oldApp oldRoot = fromMaybe state (useState oldState)
  CompositeState newApp newRoot = state
  widgetRoot = _widgetMerge (_instanceWidget newRoot) newApp newRoot oldRoot
  newState = CompositeState newApp widgetRoot
  newInstance = newComposite {
    _instanceWidget = createComposite newState eventHandler
  }

--_widgetHandleEvent :: PathContext -> SystemEvent -> s -> WidgetInstance s e m -> Maybe (EventResult s e m)
compositeHandleEvent :: (Monad m, Typeable s, Typeable e, Typeable m) => CompositeState s e m -> EventHandlerC s e ep -> PathContext -> SystemEvent -> sp -> WidgetInstance sp ep m -> Maybe (EventResult sp ep m)
compositeHandleEvent (CompositeState app widgetRoot) eventHandler ctx evt pApp widgetComposite = fmap processEvent result where
  result = _widgetHandleEvent (_instanceWidget widgetRoot) ctx evt app widgetRoot
  processEvent (EventResult reqs evts newRoot) = EventResult Seq.empty Seq.empty widgetComposite

--convertRequest :: EventRequest s -> Maybe (EventRequest sp)
--convertRequest req = case isUpdateUserState req of
--  True -> Nothing
--  False -> Just req

compositePreferredSize :: CompositeState s e m -> Renderer m -> sp -> WidgetInstance sp ep m -> Tree SizeReq
compositePreferredSize (CompositeState app widgetRoot) renderer _ _ = _widgetPreferredSize (_instanceWidget widgetRoot) renderer app widgetRoot

compositeResize :: (Monad m, Typeable s, Typeable e, Typeable m) => CompositeState s e m -> EventHandlerC s e ep -> sp -> Rect -> Rect -> WidgetInstance sp ep m -> Tree SizeReq -> WidgetInstance sp ep m
compositeResize state eventHandler _ viewport renderArea widgetComposite reqs = newInstance where
  CompositeState app widgetRoot = state
  newRoot = _widgetResize (_instanceWidget widgetRoot) app viewport renderArea widgetRoot reqs
  newState = CompositeState app newRoot
  newInstance = widgetComposite {
    _instanceWidget = createComposite newState eventHandler
  }

compositeRender :: (Monad m) => CompositeState s e m -> Renderer m -> Timestamp -> PathContext -> sp -> WidgetInstance sp ep m -> m ()
compositeRender (CompositeState app widgetRoot) renderer ts ctx _ _ = _widgetRender (_instanceWidget widgetRoot) renderer ts ctx app widgetRoot

{--
  The objective is allowing modules that are not tied to the main application's state type, and that provide event handling at a higher level
  Having said that, using the same state type should be possible

  Event handling should:
    - Provide a way to update custom state (State s)
    - Provide a way to run actions in IO (through WidgetTask)
    - Provide a way to send events to parent (ep)
    - Provide a way to update parent state (State sp)

  Handle Custom should route to corresponding Widget

  _widgetGetState = ignoreGetState,
    Saves custom state
  _widgetMerge = containerMergeTrees ignoreOldInstance,
    Relies on baseContainer's merge
  _widgetNextFocusable = containerNextFocusable,
    Calls the main widget's nextFocusable, with the corresponding ctx
  _widgetFind = containerFind,
    Calls the main widget's find, with the corresponding ctx
  _widgetHandleEvent = containerHandleEvent ignoreEvent,
  _widgetHandleCustom = containerHandleCustom,
    Route to corresponding Widget
  _widgetPreferredSize = containerPreferredSize defaultPreferredSize,
    Returns the main widget's preferredSize
  _widgetResize = containerResize defaultResize,
    Calls the main widget's resize, passing the assigned size
  _widgetRender
    Calls the main widget's render  function
--}
