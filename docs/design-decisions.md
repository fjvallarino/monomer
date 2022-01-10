# Design decisions

## Why not FRP?

I wanted to create an easy to use and learn library, and I found FRP was harder
to explain than the Elm Architecture.

## Why do widgets use hidden internal state?

Some widgets need state to operate. For example, textFields need to know the
current cursor position and selection, while images need to have a reference to
the block of bytes representing the uncompressed image. There are two options
for handling this requirement:

### Make the state explicit

Users take care of storing and providing the state to the library. This is
similar to how the model's fields are associated to widgets.

The advantage is that this makes the library more functionally pure. It also
simplifies the internals and may improve performance since the merge process
could be simplified.

The disadvantage is that it requires more boilerplate from part of the user,
since fields for each of the widget's state need to be added to a global state
type (or mix it with the model).

### Keep the state internal

Widgets have an internal state that is saved/restored by the library.

The main advantage is that it's simpler for the user. Only widgets that handle
input require the user to provide them model fields, while their internal state
is transparent from the user's point of view.

The disadvantages are that it makes the library more complex and less
functionally pure than the other approach. It also makes creating custom widgets
more complicated, since the merge process needs to be considered.

### What this library does

As mentioned in the README, one of the main objectives of the library is being
easy to use. For this reason, and because it reduces boilerplate, hidden state
was chosen. The library takes care of saving/restoring state during the merge
process.

Sometimes, for performance reasons, you may want to use `mergeRequired` as the
[Books](examples/02-books.md#interesting-bits) example does.

## Are themes really necessary?

While it's true that you can create a customized version of a widget by writing
a regular function, and it is encouraged and used in the examples, in some cases
it is not enough.

Some widgets, such as `alert` and `confirm`, are built out of nested widgets and
do not expose styling options for all their sub widgets. In this scenario,
themes allow you to customize those nested widgets as needed.

Themes also have the nice property of simplifying color scheme switching.

## Why there is not a margin property, considering border and padding do exist?

Margin used to be part of the library but made mouse event handling more
complex. Since margin is just space outside the border, it can be emulated by
adding a wrapper widget. For example:

```haskell
box (label "Test" `styleBasic` [padding 10, border 1 black])
  `styleBasic` [padding 5]
```

In that case, the box provides the _margin_ around the label.

## Why is the exit event sent only to the top level widget?

Adding a SystemEvent was discarded because most widgets would not be interested
in handling it. With that in mind, relying on messages or regular events made
more sense.

Considering there is not broadcasting support for messages (they are sent to
a widget on a given path), restricting to the top-level widget seemed clearer.

## Why, except for render, is the widget interface non monadic?

I find using regular functions nicer than using bind or do syntax and, given
those functions do not rely on any effect, I preferred to keep them as such.

## Why is the return type of the render function IO and not some custom monad?

I didn't feel having a custom monad provided a benefit, considering the Widget
Environment parameter is already available (if it hadn't been a parameter, a
Reader stack may have been a good idea).

## Why is the return type of Tasks and Producers IO and not MonadIO?

There were some type mismatch issues down the road, plus the async library
requires using IO to launch asynchronous tasks. Maybe MonadBaseControl or
MonadUnliftIO could be used.

## Why does WidgetNode have a reference to children widget, considering the widget may not have any?

It made the development of container widgets more straightforward. It also gave
me the ability to create utility functions that can be used across the library
for inspecting the widget node tree.

## Why is the location of the optional config argument inconsistent between single and container widgets?

For single widgets having the optional config as the last argument makes sense,
since it is something extra compared to the default version.

For container widgets, the usage pattern is a bit different. While splitting a
complex UI into smaller parts is necessary for maintainability, seeing the
widget hierarchy at first glance is also helpful for understanding an
application. For this reason, sometimes, widgets are nested directly in their
parent instead of creating a separate list that is later provided to the parent
widget. In that scenario, having the config of the parent widget _after_ its
children causes confusion, because it's not clear whom the config belongs to.
Putting the config before the children is much clearer in this regard.

## Why records of functions instead of typeclasses?

There were two reasons for using records instead of typeclasses:

- Since all widgets end up inside a list, the instances of those typeclasses
  would need to be wrapped in an existential, losing their specific types in the
  process.
- Using typeclasses for widgets seemed to lead to needing more advanced type
  system features.

Because of the first reason, there were not advantages by using typeclasses
compared to using records. This situation of wrapping a typeclass in an
existential is referred to as _"Existential Typeclass Anti Pattern"_. For
example,
[here](https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass).

## Why do you use lawless typeclasses for combinators?

I wanted to provide a standardized interface for configuring widgets. For
example, all widgets which generate events when clicked should expose a
configuration option using the same name (in this case, `onClick`). I think the
only options to achieve this are typeclasses and labels, and I chose typeclasses
because I know them better.

This choice may also cause namespace issues that require using qualified
imports. I tried to avoid _reserving_ common names to reduce the number of times
this is needed.

If you know a better option to solve this, please let me know!

## Why Lens and not MicroLens?

I needed to store references to lenses in custom data types, which is not
possible with the standard `Lens` type. The lens library provides `ALens`, which
allows for that use case. I don't think microlens has something similar, plus
I'm currently using more things from lens.

## Why Lens and not Optics?

I started this project in one form or another in mid-2018, while the Optics
library was released in September 2019. I have not had a strong reason for
replacing Lens with Optics, and a migration would take a lot of effort.

## Why nanovg? The project's repository says it's not actively maintained.

[NanoVG](https://github.com/memononen/nanovg) is an easy to use vector graphics
library that uses OpenGL, making it GPU accelerated. It provides a nice set of
drawing operations that cover all the current use cases of Monomer. When I
started the project, NanoVG was actively maintained and was a very reasonable
option.

NanoVG not being maintained is not ideal, but the feature set used by Monomer
has been stable for quite a while. It's also a kind of small library, which
makes porting it to other backends feasible (a version for Metal, which I have
not tested, can be found [here](https://github.com/ollix/MetalNanoVG)).

I have this situation on the back of my mind since I also worry at some point
Apple will discontinue OpenGL support, but it's not on my list of priorities. I
have taken a look at these options:

- [Skia](https://skia.org) has lots of features and is used by Chrome and other
  major applications, but the C bindings are marked as not stable, and it does
  not look like an easy to distribute library (plus compiling it takes a long
  time).
- ImGui's [ImDrawList](https://github.com/ocornut/imgui/issues/1878), the option
  I like the most, supports several backends and is an easy to embed library. I
  haven't validated if something Monomer uses is missing.
