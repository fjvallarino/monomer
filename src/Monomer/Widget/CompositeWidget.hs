module Monomer.Widget.CompositeWidget where

import Monomer.Common.Geometry
import Monomer.Common.Tree
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

--data Composite s e ep m = Composite {
--  _initialStateC :: s,
--  _uiBuilderC :: UIBuilderC s e m,
--  _eventHandlerC :: EventHandlerC s e ep
--}

--handleEvent :: Monad m => s -> e -> EventResponseC s e ep

--type app :: (Monad m) => Composite s e e m -> WidgetInstance s e m
composite :: (Monad m) => WidgetType -> s -> EventHandlerC s e ep -> UIBuilderC s e m -> WidgetInstance sp ep m
composite widgetType state eventHandler uiBuilder = defaultWidgetInstance widgetType widget where
  widgetRoot = uiBuilder state
  widget = createComposite state widgetRoot eventHandler

createComposite :: (Monad m) => s -> WidgetInstance s e m -> EventHandlerC s e ep -> Widget sp ep m
createComposite state widgetRoot eventHandler = widget where
  widget = Widget {
    _widgetGetState = ignoreGetState,
    _widgetMerge = containerMergeTrees ignoreOldInstance,
    _widgetNextFocusable = containerNextFocusable,
    _widgetFind = containerFind,
    _widgetHandleEvent = containerHandleEvent ignoreEvent,
    _widgetHandleCustom = containerHandleCustom,
    _widgetPreferredSize = compositePreferredSize state widgetRoot, -- containerPreferredSize defaultPreferredSize,
    _widgetResize = compositeResize state widgetRoot eventHandler, -- containerResize defaultResize,
    _widgetRender = compositeRender state widgetRoot --containerRender
  }

compositePreferredSize :: s -> WidgetInstance s e m -> Renderer m -> sp -> WidgetInstance sp ep m -> Tree SizeReq
compositePreferredSize app widgetRoot renderer _ _ = _widgetPreferredSize (_instanceWidget widgetRoot) renderer app widgetRoot

compositeResize :: (Monad m) => s -> WidgetInstance s e m -> EventHandlerC s e ep -> sp -> Rect -> Rect -> WidgetInstance sp ep m -> Tree SizeReq -> WidgetInstance sp ep m
compositeResize app widgetRoot eventHandler _ viewport renderArea widgetComposite reqs = newInstance where
  newRoot = _widgetResize (_instanceWidget widgetRoot) app viewport renderArea widgetRoot reqs
  newInstance = widgetComposite {
    _instanceWidget = createComposite app newRoot eventHandler
  }

compositeRender :: (Monad m) => s -> WidgetInstance s e m -> Renderer m -> Timestamp -> PathContext -> sp -> WidgetInstance sp ep m -> m ()
compositeRender app widgetRoot renderer ts ctx _ _ = _widgetRender (_instanceWidget widgetRoot) renderer ts ctx app widgetRoot

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
