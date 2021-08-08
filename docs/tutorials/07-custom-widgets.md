# Custom Widgets

Sometimes you can't build the UI you need using only the stock widgets. In that
case, you can create a custom widget.

Custom widgets give you the ability to handle mouse and keyboard events and to
render arbitrary content to the screen. They also provide more explicit control
over other features we have been using, such as tasks and producers.

Monomer provides the `Widget` type, which represents the interface for creating
widgets. As an interface, it requires implementing everything from scratch,
which is not usually the easiest approach. In general, you will be interested in
using either of:

- `Single`: used for creating widgets without children. Checkbox, radio, label,
  slider and textField are all instances of Single.
- `Container`: used for creating widgets with children. In general, this type of
  widget represent some sort of layout (box, grid, stack, split), but they can
  also be used to provide services to its children (for example, keystroke,
  animFadeIn or tooltip).

## Common lifecycle functions

Although their prefix will be different (single vs container), for both types of
widgets you may be interested in overriding these functions:

- Init: Called the first time a widget is added to the widget tree. It's useful
  if you need to load external resources (an image) or if precomputing parts of
  your state would improve performance (the glyphs of a label).
- Merge: Called every time the UI is rebuilt. In general, you need to override
  this function for your widget to work correctly.
- Dispose: Useful if you have external resources that cannot be automatically
  freed by the garbage collector.
- HandleEvent: Used when you need to handle low level events from mouse,
  keyboard, focus, clipboard or drag and drop.
- HandleMessage: Used when the widget uses custom Tasks or Producers, or when
  it supports external messages (such as the animation messages we've used).
- GetSizeReq: Indicates the preferred size of the widget, whether it is fixed or
  flexible. This is particularly important with stack or box, although other
  containers (grid, for example) may choose to ignore it.
- Render: Takes care of drawing the content of the widget to the screen. The
  Renderer interface provides low level rendering functions, while the drawing
  module provides some convenience higher level ones.

Both Single and Container have other overridable functions and attributes. Check
their respective Haddocks for more details.

## Single

### Creation

A typical way to organize the code of a custom widget is by having a user facing
function and an internal one, that will be used to handle updates to the state.

```haskell
canvas :: WidgetNode s e
canvas = canvas_ def

canvas_ :: [CanvasCfg] -> WidgetNode s e
canvas_ configs = defaultWidgetNode "canvas" newWidget where
  config = mconcat configs
  state = CanvasState []
  newWidget = makeCanvas config state

makeCanvas :: CanvasCfg -> CanvasState -> Widget s e
makeCanvas cfg state = widget where
  widget = createSingle state def {
  ...
  }
```

In this case, `canvas` is the user facing function, which returns a default
WidgetNode (the rest of the information of the node will be completed by its
parent). On the other hand, `makeCanvas` returns a widget, which is later used
in other widget functions to update its state (and, if needed, the node too).

Another common pattern is to provide a default version of the widget, without
requiring configuration, and a configurable one, which is distinguished by an
underscore as a suffix.

### Merging

The makeCanvas function is used in merge. It creates a new version of the widget
using the old state. Some widgets may need to modify the old state before using
it. In the case of `textField`, if the text it handles changed because of a
model reset, the position of the cursor may be invalid and its state would need
to be adjusted.

```haskell
merge wenv node oldNode oldState = result where
  newNode = node
    & L.widget .~ makeCanvas oldState
  result = resultNode newNode
```

You may wonder why the oldNode is not used directly, or at least its widget. The
reason is that, for the node, the styling may have changed when the new UI was
built. For the widget, since we reference the config parameter (which may also
have changed), if we keep the old widget we'd still be using the previous
version of the config.

### WidgetResult

Some operations return a WidgetResponse, which contains the new version of the
node plus a list of WidgetRequests. There are a few helpers available for
creating instances of this type:

- resultNode: updates the node, without requests.
- resultReqs: updates the node and includes requests.
- resultEvts: updates the node and includes user events.
- resultReqsEvts: updates the node and includes requests and user events.

User events are, under the hood, sent as requests using `RaiseEvent`. The helper
functions simplify this process. You might be interested in using RaiseEvent if
you need to have control regarding the order in which your requests are
processed.

### Handle event

The standard way to implement handleEvent is to pattern match on the `evt`
argument, handle the events of interest and return Nothing for the rest. In the
example `Click` and `Move` are handled.

```haskell
  handleEvent wenv node target evt = case evt of
    Click point button clicks -> Just result
    ...
    Move _ -> Just (resultReqs node [RenderOnce])
```

The main point of interest is the request made by Move. Rendering only happens
automatically for keyboard and mouse action events, not for movement. This
means, if rendering is not requested, the new line will not be displayed until
clicking the button again. In case you need to render periodically, you may want
to check `RenderEvery`.

Both Single and Container provide support for handling style changes based on
status (hover, active, etc); in those cases, the `RenderOnce` request is created
automatically.

### Handle message

The case of `handleMessage` is similar but, since the `msg` argument is an
instance of `Typeable`, you need to use `cast` first. In this case the message
is generated by the application using the widget but, if the widget called
`RunTask` or `RunProducer`, handleMessage would also be the place where the
generated messages are received.

```haskell
  handleMessage wenv node target msg = case cast msg of
    Just ResetCanvas -> Just result where
    ...
```

### Size requirement

Widgets may have preferences regarding their size. For instance, a single line
label will try by default to fit its text completely, while its multiline
version will be fine with getting more or less space.

The example uses the `minWidth`/`minHeight` combinator. There are a few others:

- width: fixed size.
- flexWidth: suggested size, although accepts variations. The provided value
  affects how space is assigned proportionally based on the available space and
  the size of other widgets.
- minWidth: base fixed size, accept more space.
- maxWidth: maximum size, accepts less space (even zero).
- rangeWidth: provides a base fixed size, plus a maximum size.

Equivalent versions for height also exist.

These combinators can also be used by the user when setting the style, and those
take precedence over what the widget prefers.

### Rendering

Finally, the render function takes care of displaying the content on the screen.

```haskell
render wenv node renderer = do
  drawInTranslation renderer origin $
    forM_ tuples $ \(idx, pointA, pointB) -> do
      setStrokeColor renderer (nextColor idx)
      setStrokeWidth renderer 2
      beginPath renderer
      renderLine renderer pointA pointB
      stroke renderer
```

The `Renderer` instance provides access to all drawing functionality. You can
check the docs for details on the API, but a few important points are:

- `beginPath` is needed before drawing any shape.
- `stroke` is needed to actually finish the shape, and only draw its outline.
- `fill` is needed to finish the shape but drawing a solid shape.

Between those calls, primitives exist for rendering lines, rectangles, ellipses,
arcs, images, etc. There is support for applying global offset, rotation, alpha
and gradients.

Besides these, the `Drawing` module provides higher level functions that receive
`StyleState` objects to simplify some common operations.

### Single widgets you may want to check

`spacer`, `checkbox`, `radio` and `label` (sorted by complexity).

## Container

Everything described for Single also applies here. For containers there is
another method that you may want to implement, in case your widget is expected
to handle more than one child.

- Resize: depending on the layout logic and the sizeReqs of its children, the
  container will assign each of its children the corresponding viewport.

### Container widgets you may want to check

`themeSwitch`, `grid`, `keystroke`, `tooltip`, `stack` (sorted by complexity).

[Previous: Composite](06-composite.md) - [Next: Themes](08-themes.md)
