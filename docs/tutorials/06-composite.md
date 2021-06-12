# Composite

## Motivation

Sometimes your application can become large, or there is functionality you want
to share with a different project. In these cases, it would be nice to be able
to have separate functions for building a section of the UI and handling its
events. This is what `Composite` provides and, inadvertently, you have been
using it all along: the `startApp` call creates a Composite under the hood.

## Types

Up to this point, our event handlers have returned a list `AppEventResponse`.

```haskell
type AppEventHandler s e
  = WidgetEnv s e            -- ^ The widget environment.
  -> WidgetNode s e          -- ^ The root node of the application.
  -> s                       -- ^ The application's model.
  -> e                       -- ^ The event to handle.
  -> [AppEventResponse s e]  -- ^ The list of requested actions.
```

This type is just an alias for `EventResponse`, with two of its type parameters
fixed.

```haskell
data EventResponse s e sp ep
  -- Sets the model
  = Model s
  -- Raises an event to itself
  | Event e
  -- Raises an event to its parent
  | Report ep
  -- Sends a Widget Request
  | Request (WidgetRequest s e)
  -- Sends a message to the specified key
  | forall i . Typeable i => Message WidgetKey i
  -- Launches a Task
  | Task (TaskHandler e)
  -- Launches a Producer
  | Producer (ProducerHandler e)

type AppEventResponse s e = EventResponse s e s ()
```

Those two extra type parameters represent the model and the event types of the
parent in the widget tree. If the composite you are creating uses `Report` to
send events to the parent, you will need to indicate its type (otherwise it can
stay as a type variable).

## Two way communication

Communication between the parent and the composite can work in both ways:

- The parent can provide information to the composite through the model,
  parameters accepted by the function that creates the composite or any kind of
  configuration.
- The composite can provide information to the parent by using the `Report`
  response. To avoid coupling the composite to the parent, the usual solution
  is to have the parent provide the expected event for each situation (the same
  as how we've been using any other widget).

## Widget Requests

Besides raising events, a widget can also make requests to the runtime. In fact,
some of the responses we have been using are just `WidgetRequest`s but with an
easier to use interface:

- Message is SendMessage
- Task is RunTask
- Producer is RunProducer

`WidgetRequest`s will be explored more in the custom widgets tutorial, but some
requests can be interesting for an application or composite:

- MoveFocus: Moves focus to the next focusable widget in the given direction.
- SetFocus: Sets focus on a specific widget.
- ExitApplication: Requests to exit the application or cancel an active request
  to exit. This is useful combined with `appExitEvent`.
- UpdateWindow: Allows making window related actions, such as setting window
  title, size, full screen mode and bring to front.

Some of these requests take a `WidgetId` as a parameter. But, what are they and
how can you get the WidgetId of a widget node?

### Key, Path and WidgetId

The are three ways of identifying a widget. One, as we've seen, is the `key`.
This is a user defined identifier and it exists because its just easier to use.
Besides key, there are two identifiers which are mostly internal, unless you are
writing custom widgets or making WidgetRequests:

- Path: This is a sequence of numbers starting from the root node, adding one
  number per level, until reaching the node of interest. Each number is an index
  in the list of children of the parent node. The main advantages of a path are
  that they clearly denote a hierarchy and that they allow to easily get the
  path of an arbitrary parent: you just need to drop as many steps as needed.
  The disadvantage is that it can become invalid if widgets change positions.
  Paths are used for several status related operations (focus, hover, etc), but
  they should not be stored beyond the current operation.
- WidgetId: The WidgetId of an item is made out of the Path and Timestamp when
  the widget was initialized. This makes it unique, and allows keeping track of
  the widget's path if its location in the widget tree changed. The disadvantage
  is that it's not possible to deduce the WidgetId of a widget, requiring a call
  to a helper function instead.

There are a couple of functions you can use to get the WidgetId of a node:

- If you have a Path, you can call `findWidgetByPath`. If the path is valid, it
  returns a `WidgetNodeInfo` instance which, amongst other node related data,
  has the WidgetId.
- If you have a key, you can call `globalKeyWidgetId` which returns the WidgetId
  if the Path is valid. There is also a `globalKeyPath` to get the Path instead.

## New Widgets

In this tutorial two new widgets are used, and in general they work together.

### Draggable

A `draggable` widget receives two arguments:

- A value that is going to be carried during a drag event. This value can be
  used by other widgets when hovered, to indicate they accept the item or not.
- A child node. This node will displayed as usual, but the user will be able to
  drag it and drop it.

This widget supports setting the style of the dragged state, plus its max size
and optional transparency.

### DropTarget

A `dropTarget` also receives two arguments:

- A function that can create user events. The type of the argument must match
  the type of the item being dragged. This event will be raised when the item is
  dropped.
- A child node, which represents the target of the drop operation. This node can
  be anything, although it will usually give an indication of what has been
  dropped on it.

The style of the dropTarget can be customized to let the user know it accepts
the dragged item.
