# Composite

## Motivation

Sometimes your application can become large, or there is functionality you want
to share with a different project. In these cases, it would be nice to be able
to have separate functions for building a section of the UI and handling its
events. This is what
[Composite](https://hackage.haskell.org/package/monomer/docs/Monomer-Widgets-Composite.html)
provides and, inadvertently, you have been using it all along: the `startApp`
call creates a Composite under the hood.

## Types

Up to this point, our event handlers have returned a list of `AppEventResponse`.

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
  -- Sets focus on the given key
  | SetFocusOnKey WidgetKey
  -- Moves focus forward/backward, optionally starting from the given key.
  | MoveFocusFromKey (Maybe WidgetKey) FocusDirection
  -- Sends a message to the specified key
  | forall i . Typeable i => Message WidgetKey i
  -- Launches a Task
  | Task (TaskHandler e)
  -- Launches a Producer
  | Producer (ProducerHandler e)

type AppEventResponse s e = EventResponse s e s ()
```

Those two extra type parameters represent the model and the event types of the
parent in the widget tree.

## Two way communication

Communication between the parent and the composite can work in both ways:

- The parent can provide information to the composite through the model,
  parameters accepted by the function that creates the composite, or any kind of
  configuration.
- The composite can provide information to the parent by using the `Report`
  response. To avoid coupling the composite to the parent, and having to provide
  the type of the parent's event, the usual solution is to have the parent
  provide the expected event for each situation (the same as how we've been
  using other widgets).

## Widget Requests

Besides raising events, a widget can also make requests to the runtime. In fact,
some of the responses we have been using are just `WidgetRequest`s but with an
easier to use interface:

- `Message` is `SendMessage`.
- `Task` is `RunTask`.
- `Producer` is `RunProducer`.

`WidgetRequest`s will be explored in more detail in the custom widgets tutorial,
but some requests can be interesting for an application or composite:

- ExitApplication: Requests to exit the application or cancel an active request
  to exit. This is useful combined with `appExitEvent`.
- UpdateWindow: Allows making window related actions, such as setting window
  title, size, full screen mode and bring to front.

Some of these requests take a `WidgetId` as a parameter. But, what are they and
how can you get the WidgetId of a widget node?

### Key, Path and WidgetId

The are three types of identifiers for a widget. One, as we've seen, is the
`WidgetKey`. This is a user defined identifier, and provides a human friendly
way of identifying widgets. Besides `WidgetKey`, there are two identifiers which
are mostly internal, unless you are writing custom widgets or making
WidgetRequests:

- `Path`: This is a sequence of numbers starting from the root node, adding one
  number per level, until reaching the node of interest. Each number is an index
  in the list of children of the parent node. The main advantages of a path are
  that they clearly denote a hierarchy and that they allow to easily get the
  path of an arbitrary parent: you just need to drop as many steps as needed.
  The disadvantage is that it can become invalid if widgets change positions.
  Paths are used for several status related operations (focus, hover, etc), but
  they should not be stored since they can become stale.
- `WidgetId`: The WidgetId of an item is made out of the Path and the timestamp
  in milliseconds when the widget was initialized. This makes it unique, and
  allows keeping track of the widget's path if its location in the widget tree
  changes. The disadvantage is that it's not possible to deduce the WidgetId of
  a widget, requiring a call to a helper function to find it.

There are a couple of functions you can use to get the WidgetId of a node:

- You can tag any WidgetNode with a user defined WidgetKey via `nodeKey`. This
  name will be stable between tree merges on the UI.
- If you have a WidgetKey you can call `widgetIdFromKey`, which returns a
  WidgetId if the Path is valid. There is also a `pathFromKey` to get the Path
  instead.
- If you have a Path you can call `widgetIdFromPath`, which returns a WidgetId
  if the Path is valid.
- Alternatively, you can call `nodeInfoFromKey` or `nodeInfoFromPath` to get all
  the information about a node (including the key, path and widgetId).

## New Widgets

In this tutorial two new widgets are used and, in general, they work together.

### Draggable

A
[draggable](https://hackage.haskell.org/package/monomer/docs/Monomer-Widgets-Containers-Draggable.html)
widget receives two arguments:

- The value that is going to be carried during a drag event. This value can be
  used by other widgets when hovered, to indicate they accept the item or not.
- A child node. This node will be rendered as a regular node, but the user will
  be able to drag and drop it. By default, the content of this node will also be
  displayed while dragging it.

This widget supports setting the style of the dragged state, plus its max size
and optional transparency.

### DropTarget

A
[dropTarget](https://hackage.haskell.org/package/monomer/docs/Monomer-Widgets-Containers-DropTarget.html)
also receives two arguments:

- A function that can create user events. The type of the argument must match
  the type of the value provided by the widget being dragged. This event will be
  raised when the item is dropped.
- A child node, which represents the target of the drop operation. This node can
  be anything, although it will usually give an indication of what has been
  dropped on it (for example, it's a list containing the dropped widgets).

The style of the dropTarget can be customized to let the user know it accepts
the dragged item. This behaves similarly to `styleHover`, but it is only active
when an item is being dragged.

<br/>

[Next: Custom widgets](07-custom-widgets.md)
