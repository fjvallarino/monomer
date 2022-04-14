# Life cycle

Widgets in Monomer have functions associated to their life cycle. The two most
important ones are:

- Init: called the first time a node is added to the widget tree.
- Merge: called after a new version of the UI is created.

## Initialization

When the application starts, the view is created from scratch by calling buildUI
and providing the initial model given to startApp. During this process, each
widget has its init function called.

From that point on, whenever the model changes buildUI will be called with the
current model, although the new version of the view will not be used directly.
The reason is widgets in the old view may have internal state that needs to be
kept around to be used by their new versions. The merge process takes care of
this.

## Merge process

The merge process starts at the root node, which is the main node returned by
buildUI. From there it checks each of its children, which in turn do the same
with their own children, and applies these criteria:

- If the widget type and widget key match (if both old and new don't have a key
  defined, it's also considered a match), the new widget is merged with the old
  one. How the merge works depends on the widget type but, in general, the old
  state is kept but using the new configuration and style.
- If the check failed, the old widget and its state is discarded and the new
  widget is initialized.

In general, this process is transparent and you don't need to worry about it.
The only context where you need to be careful is if the position of your widgets
change. This can happen if you add items to the beginning of a list (pushing the
previous items one position deeper), remove an item or reorder them. Just to
clarify: this only is a concern if there are widgets with state involved,
although it's probably safer to just always handle it.

As a final note and as is shown in the example, you only need a key at the top
level item whose position changes, not on each of its children.

In the example, a `nodeKey` is associated to each row. It is used infix as the
style related functions, and receives a Text argument:

```haskell
listItem idx item = hstack [
    label (item ^. text) `styleBasic` [width 100],
    textField (items . singular (ix idx) . text),
    spacer,
    button "Delete" (RemoveItem idx)
  ] `nodeKey` showt (item ^. ts) `styleBasic` [paddingT 5]
```

In the case of a `textField`, the internal state contains the current cursor
position and selection. When you add a new item by clicking on the "Add" button
(or pressing Enter), a new row is added. Each row has an editable textField, in
which you can, as usual, move around with the arrow keys. If you add a few more
items and use tab to navigate to a different textField, you will notice that
each textField keeps the cursor position it had before losing focus, even after
their position in the list changed. Just for fun, try removing the `nodeKey`
from that function: you will see that the textField was not _moved_ to the
correct position in the list, as the cursor position indicates.

While this example may look uninteresting, it is important to keep this
situation in mind. Animation widgets also have state, and changing their
position without using a nodeKey would cause erratic behavior.

### Setting focus

Keys are not only used for merging. In this example, we use them to set focus on
the `description` textField whenever a new item is added.

We will see in the Producers example that we can also use keys for sending
messages to widgets.

You can explore the [Todo](../examples/01-todo.md) example for a more elaborate
use of the concepts we've seen so far.

## Notes

### Keystroke

You may have noticed a `keystroke` widget is used. A Monomer application (which
turns out to be a Composite, as we'll see in more advanced tutorials) can only
handle the events defined by the user. This is a difference compared to custom
widgets, which can handle any kind of low level event. Although this can sound
like a limitation, you can always have a widget that converts those low level
events to the event type your application can handle.

This is the case of keystroke. It receives a list of mappings from a keystroke
combination, provided as Text, and the event to raise when that combination is
detected. In this example, it just listens for the `Enter` key and raises the
same event as if the user had clicked the "Add" button. These combinations can
be more complex, of course. Check the
[documentation](https://hackage.haskell.org/package/monomer/docs/Monomer-Widgets-Containers-Keystroke.html)
of the widget for more information.

### The _singular_ function

This is explained in the [optics](external/01-optics.md##singular) introduction.

<br/>

[Next: Tasks](04-tasks.md)
