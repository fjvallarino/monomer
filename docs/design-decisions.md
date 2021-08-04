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
simplify the internals and improve performance, since the merge process can be
removed.

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
a bit more complex, since the merge process needs to be considered.

### What this library does

As mentioned in the README, one of the main objectives of the library is being
easy to use. For this reason, and because it reduces boilerplate, hidden state
was chosen. The library takes care of saving/restoring state during the merge
process.

## Why did you add themes? Customized versions of widgets can be created by writing simple functions.

While it's true that you can create a customized version of a widget using
functions, and it is in fact encouraged and used in the examples, in some cases
it is not enough.

Some widgets, such as dialogs, are built out of nested widgets and do not expose
style options for all their sub widgets. In this scenario, themes allow you to
customize those nested widgets as needed.

Themes also have the nice property of simplifying color scheme switching.

## Why is there not a margin property, considering border and padding do exist?

It used to be part of the library, but caused issues and was confusing. Since
margin is just space outside the border, it can be emulated by adding a wrapper
widget. For example:

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
a widget on a given path), restricting to the top level widget seemed clearer.

## Why, except for render, is the widget interface non monadic?

Limiting side effects to the render function made the intention of each of the
functions in the widget interface clearer. It also simplified the runtime of the
library.

## Why is the return type of render IO and not some custom monad?

I didn't feel having a custom monad provided a benefit, considering the Widget
Environment parameter as already available (if it wasn't, a Reader stack may
have been a good idea).

## Why is the return type of Tasks and Producers IO and not some generic Monad?

There were some type mismatch issues down the road plus the async library
requires using IO to launch asynchronous tasks.

## Why does Widget have a reference to children widget, considering the Widget may not have any?

It made development of container widgets simpler. It also gave me the ability to
create utility functions that can be used across the library for inspecting the
widget node tree.

## Why do containers have the optional config argument before their children? It's inconsistent with where single widgets have it.

This was a decision I made based on usage. Splitting complex UIs into smaller
parts is a great idea but, at the same time, it's nice to be able to quickly see
the hierarchy of widgets. This means that, quite frequently, widgets are nested
directly in their parent. Having the config of the parent widget _after_ its
children caused confusion, not being clear whom it belonged to. Putting the
config before the children is much clearer in this regard.

## Why records of functions instead of typeclasses?

There were two reasons for using records instead of typeclasses:

- Since all widgets end up inside a list, the instances of those typeclasses
  would end up wrapped in an existential, losing their specific types in the
  process.
- Using typeclasses for widgets seemed to lead to needing more advanced type
  system features.

Because of the first reason, there was no advantage compared to using records
directly. This situation of wrapping in an existential is referred by some as
_"Existential Typeclass anti pattern"_. For example,
[here](https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass).

## Why do you use lawless typeclasses for combinators?

I wanted to provide a standardized interface for configuring widgets. For
example, I wanted that all widgets which generate events when clicked expose a
configuration option of the same name (in this case `onClick`). I think the only
options for this are typeclasses and labels. I chose typeclasses because I knew
them better.

I'm aware about the namespace issues caused by typeclasses, which is why I tried
to avoid _reserving_ common names. I also know lawless typeclasses are frowned
upon by the Haskell community.

If you know a better option to solve this, please let me know!

## Why Lens and not MicroLens?

I needed to store references to lenses, which are provided by the `ALens` type
in the lens library; I don't think microlens provides something similar.
