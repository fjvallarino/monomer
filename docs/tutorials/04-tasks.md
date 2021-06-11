# Tasks

Since Haskell is a pure language, performing side effects is not possible unless
specifically denoted. Monomer works as much as possible as set of pure functions
but, in most cases, the user will need to consume external information or
perform actions which are inherently not pure. To allow for this, the concept of
tasks exist.

## Running asynchronous tasks

We've previously used `Model` as a response to an event. Another possible
response is `Task`, which receives an IO action that can run arbitrary code and
returns a valid user event. This task will be launched as a separate thread and,
when it finishes, its result will be fed back into the application. In case the
IO action crashes the application will not be notified; it is up to the task to
handle any kind of exception and report it with the appropriate event.

In the example, we can see the task calling `randomRIO`, which returns a random
number in the given range. This function uses a system seed, which makes it
impure and as such could not be called outside of IO. A typical use of Task
is to consume a REST API or similar.

```haskell
handleEvent wenv node model evt = case evt of
  AppGenRandom -> [Task $
      AppSaveRandom <$> randomRIO (1, 6)
    ]
  AppSaveRandom value -> [Model $ model & selected .~ value]
```

## New widgets

A few extra widgets are introduced in this tutorial.

### zstack

Useful whenever content needs to be laid out on top of each other. In this
example it is used to combine an image with text, but another typical use case
is when a dialog needs to be displayed while content is partially visible below.
Instead of using a vstack/hstack and alternating the visibility of both content
and dialog, using a zstack allows keeping content always visible and toggle the
dialog.

```haskell
pushLayers = zstack [
    image_ "./assets/images/red-button.png" [fitFill],
    label "Push!" `style` [textFont "Bold", textSize 20, textCenter]
  ]
```

### image

The image widget allows loading and displaying an image from the local
filesystem or from an HTTP/S resource. It also provides options for fitting the
image to the available space, and also for filtering/repeating patterns.

```haskell
image_ "./assets/images/red-button.png" [fitFill]
```

It also supports creating images from memory using a ByteString.

### scroll

A common situation is having a widget that needs more space than is available. In
this scenario, to avoid having the widget cut its content or display it in a less
than ideal format, a `scroll` can be used. There are three types of scroll:

- scroll: displays both scroll bars if required.
- hscroll: only displays the horizontal scroll bar, if needed. The contained
  widget's height is the same as what the scroll gets.
- vscroll: only displays the vertical scroll bar, if needed. The contained
  widget's width is the same as what the scroll gets.

```haskell
numberedImage url idx = scroll (image_ url [fitFill])
```

It provides several configuration options, including style of the scroll bars
and whether it should automatically scroll to the location of a widget receiving
focus.
