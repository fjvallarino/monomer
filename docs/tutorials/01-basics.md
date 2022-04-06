# Basics

A Monomer application has five main components that are provided to the
`startApp` function:

- **Model**: contains data specific to your application.
- **Events**: generated from user actions or asynchronous tasks.
- **Build UI function**: creates the UI using the current model.
- **Event handler**: reacts to events and can update the model, run asynchronous
  tasks and other actions.
- **Configuration**: several options to indicate window size, available fonts
  and theme.

We'll explore these components next.

### Notes

The code in this tutorial matches the one in `monomer-starter` and it is also
the same as in this package's `Tutorial01_Basics.hs`. Next tutorials will have
their own files matching the tutorial number; you can just copy the code over to
your project for testing.

In general, you will want to have your type definitions in a separate file. For
the tutorials they are together with the code so it's simpler to copy and paste
into your starter application.

## The model

The model represents the state of your application. Here you can store anything
that models your subject of interest. When the application starts, you need to
provide an initial model.

In the starter application, the model is simply a click counter.

```haskell
data AppModel = AppModel {
  _clickCount :: Int
} deriving (Eq, Show)
```

You can check the example applications to see some more complex models.

### Lenses

Monomer relies on the [lens](https://hackage.haskell.org/package/lens) library
to simplify the connection between the user model and the widgets that will be
displayed. You can find a short reference with enough information on what you
need to use the library [here](external/01-optics.md).

#### Can I avoid using lenses?

Yes! All the included widgets have two versions, one for lenses and one for
values (with a **V** suffix). When using the **V** versions, you need to provide
the current value and an event that will be generated when the value managed by
the widget changes. Once you receive the event, you can update your model using
your preferred mechanism (regular record update or optics). Since the widget
receives the current value as a parameter, if you always pass the same value
(i.e., you don't update the model) the widget will never change.

In general, unless you need to perform some kind of validation (or you really
don't like lenses), the non **V** version is simpler and avoids boilerplate.

## Events type

The events type represents the different actions your event handler can react
to. It is an algebraic data type whose values may take arguments, depending on
the event. A click event does not need arguments, but onChange events require
receiving an argument matching the type of the content the widget handles.

```haskell
data AppEvent
  = AppInit
  | AppIncrease
  deriving (Eq, Show)
```

## Creating the UI

The build UI function takes care of creating the widget tree. Whenever the model
changes, this function will be invoked and a new version of the widget tree will
be created. This new version of the widget tree will then be
[merged](./03-life-cycle.md#merge-process) with the previous version.

The starter application includes the following snippet:

```haskell
buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
```

First of all, it has the type signature. You don't really need to include it,
but in general it's preferable to do it in order to have clearer compiler
errors. Both `WidgetEnv` (environment information that can be used when building
the UI) and `WidgetNode` (the result of building the UI) receive the type of
your model and the type of your events as type parameters (sometimes referred to
as `s` and `e` in the tutorials when being general about the return type, as in
`WidgetNode s e`).

Next, it declares the parameters the function receives:

- **wenv**: short for Widget Environment, this includes information about the
  OS, window size, input status, focus and several other items.
- **model**: the current, user defined, state of the application.

Finally, a WidgetNode is returned, which can be a single widget or a more
complex layout.

Before moving forward, a quick clarification:

- A `Widget` implements the functions to initialize, merge, dispose and render a
  specific type of widget. For example, checkbox and textField. In case you need
  custom rendering, you will be implementing a `Widget`.
- A `WidgetNode` contains a widget instance and all the information related to
  its location, size, visibility, children, etc. When mentioning the _"widget
  tree"_, it really is the _"widget node tree"_. All the functions used
  throughout the tutorials to create `widgets` return `WidgetNode`s, to allow
  composing them into larger structures.

We'll explore some basic widgets next.

### Layout

The two most common widgets for layout are `hstack` and `vstack`. These allow
stacking widgets next to each other in horizontal or vertical position, trying
to satisfy the size requests of each of them (the h or v indicate the main
axis).

Back to the starter app, you can see both being used:

```haskell
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      label "Hello world",
      spacer,
      hstack [
        label $ "Click count: " <> showt (model ^. clickCount),
        spacer,
        button "Increase count" AppIncrease
      ]
    ] `styleBasic` [padding 10]
```

Stack will assign the maximum available space for the secondary axis. In the
example, the children of vstack will get same width vstack gets (the window
width, in this case), but they will be assigned vertical space according to what
they requested.

Inside hstack you'll notice the use of `spacer`. This just adds a small gap
between two widgets. Simple but very useful! In case you want to take as much
space as available (for example, you want one button on the left, one on the
right and space in the middle) you can use `filler`.

If you just want the same spacing between all the children in your container,
then you can instead use the `childSpacing` configuration combinator, which is
roughly equivalent to putting a `spacer` between all the children. You can
control the exact amount of spacing with the `childSpacing_ <pixels>` combinator:

```haskell
vstack_ [childSpacing_ 100] [
  label "good fences make",
  label "good neighbours"
]
```

### Basic widgets

In the example you can see `label` and `button`, two basic building blocks which
are useful in most applications.

#### Label

As expected, label is used to display text. More specifically, it displays
[Text](https://hackage.haskell.org/package/text) instances. There is also
`labelS`, which can be used for instances of `Show`, such as numbers or custom
types, without having to convert first to Text. If you need to display a
`String` instance, it's better to use `Text.pack` to avoid having `"` displayed.

Most widgets support a basic version, such as `label`, and a configurable
version which is denoted by a trailing `_`. In the case of `label_`, some of the
config options are:

- **multiline**: splits the text into multiple lines if width is not enough.
- **ellipsis**: shows ellipsis when text overflows instead of just cutting it.

For example:

```haskell
label_ "This is\nmultiline text" [multiline, ellipsis]
```

#### Button

The button widget provides a basic interaction block for users. To construct it,
it needs a caption and an event as defined in [Events type](#events-type).

It supports the same configuration options as label (multiline, ellipsis, etc)
plus some extra options for other possible events accessible with `button_`:

- onClick: in case you want to generate more than one event.
- onFocus: raises an event when the button gains focus.
- onBlur: raises an event when the button loses focus.

All widgets that can be focused provide the onFocus and onBlur events.

## Event handling

In the starter app, you can see the following event handler:

```haskell
handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppIncrease -> [Model (model & clickCount +~ 1)]
```

As in the build UI function, it's usually better to declare the types. Again we
have WidgetEnv and WidgetNode, but we now also have `AppEventResponse`, which
takes the same two type parameters (model and event types).

Looking at the parameters, we see:

- **wenv**: the Widget Environment.
- **node**: the current node. In general you will not use this parameter, but it
allows inspecting the underlying tree structure.
- **model**: the current state.
- **evt**: the event to handle.

The usual process consists on matching on the expected events (defined in your
events type) and returning a list of responses for the runtime to process.

In the example we use the `Model` response, which sets the new state of the
application (you can check the [lens](external/01-optics.md) tutorial to better
understand those operators). If the model changed, this will trigger a call to
the build UI function.

## Configuration

By default, the starter app sets a few configuration options, including window
title, theme, one font and an event to raise at startup.

```haskell
config = [
  appWindowTitle "Hello World",
  appTheme darkTheme,
  appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
  appInitEvent AppInit
  ]
```

You can check all the possible configuration options in the documentation.

<br/>

[Next: Styling](02-styling.md)
