# Styling

## Text

Before you can display a label, you need to load fonts into the system. This can
be done using the application's config. The monomer-starter project includes the
Roboto fonts for Regular, Medium, Bold and Italic, and they are loaded this way:

```haskell
config = [
  ...,
  appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
  appFontDef "Medium" "./assets/fonts/Roboto-Medium.ttf",
  appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
  appFontDef "Italic" "./assets/fonts/Roboto-Italic.ttf",
  ...
  ]
```

Once these declarations are in place, you can choose which font to use for any
label or widget that displays text.

```haskell
label text `styleBasic` [textFont "Medium", textSize 20]
```

A widget node can be assigned different styles depending on its current status.
These styles can be set using the `style...` family of functions, and they
receive a list of attributes. These functions are generally used infix. They
are:

- `styleBasic`: Default style of a widget. It serves as the base for all the
  other style states when an attribute is not overriden.
- `styleHover`: Used when the widget is hovered with a pointing device. In
  general an alternative background color is used.
- `styleFocus`: Used when the widget has keyboard focus. In general a border is
  displayed.
- `styleFocusHover`: Used when the widget is both focused and hovered. In this
  situation the attributes defined in focus and hover will be combined, with
  focus attributes taking precedence. This style state allows for better control
  in cases when the combination of focus and hover styles do not match
  expectations.
- `styleActive`: Used when a mouse press was started in the widget and the
  pointer is inside its boundaries.
- `styleDisabled`: Used when the `nodeEnabled` attribute has been set to False.

Styles belong to a specific widget and are not cascade down to its children. If
you have several labels inside a container and want them to have the same font
size, you need to assign the style to each of them individually.

Considering the previous example, it would be nice to avoid duplicating the
styling code all over the application. An easy way to avoid this is to create a
function that returns the label styled as needed:

```haskell
titleText text = label text
  `styleBasic` [textFont "Medium", textSize 20]
```

If you apply `styleBasic` to the result of `titleText`, the attributes will be
combined. This means that:

```haskell
newNode = titleText "Title"
  `styleBasic` [textSize 40, textColor red]
```

will result in:

```haskell
[textFont "Medium", textSize 40, textColor red]
```

If you want to replace the style state in a node, you can use `styleBasicSet`.
All the `style...` functions have an associated `style...Set` version.

In general, all components which display text support the following styles:

- `textFont`
- `textSize`
- `textColor`
- `textUnderline`/`textOverline`/`textThroughline`
- `textCenter`/`textLeft`/`textRight`
- `textMiddle`/`textTop`/`textBottom`

All dimensions in Monomer are expressed in pixels.

### Note: Possible compilation issue

When creating a custom widget using a function similar to `titleText`, you may
run into a compilation issue along the lines of _*"No instance for (Typeable s)
arising from a use of"*_ (or the same with `e`). This will happen if your
function uses `box`, `button` or any other widget that generates events,
especially if the function is written in point free format. For example, this
will fail unless it's used in the same file where it is declared:

```haskell
boxedLabel = box . label
```

The solution in those cases is having a explicit type signature. If you use
`AppModel` and `AppEvent`, or the corresponding names for your application,
a type signature similar to the one of `buildUI` is good. For example, this
will work:

```haskell
boxedLabel :: Text -> WidgetNode AppModel AppEvent
boxedLabel = box . label
```

Sometimes you don't want to tie your utility functions to specific types,
because you want to reuse them in other parts of the application that may use
different model/events. In those cases you can return a more general type, as
long as you satisfy the constraints for the `s` and `e` arguments of
`WidgetNode` (sometimes, being explicit only about `e` is enough). Monomer
exports two types (`WidgetModel` and `WidgetEvent`) that are just aliases of
`Typeable`, but have a more specific name and the extra import for `Typeable` is
not needed. Extending the previous example, this will work too:

```haskell
boxedLabel :: (WidgetModel s, WidgetEvent e) => Text -> WidgetNode s e
boxedLabel = box . label
```

## Padding, border and radius

Besides using spacer and filler for having some room between widgets, you can
also use `padding`. Using padding without any suffix applies the same value to
all sides, but there are also `paddingL`, `paddingR`, `paddingB` and `paddingT`.
Both `paddingH` and `paddingV` are just combinations for those primitives.

Similarly exists `border` to apply on all sides, receiving width and color, plus
`borderL`, `borderR`, `borderT` and `borderB`. Width and color do not need to be
the same on all sides.

Padding and border are somehow related:

- Border is rendered at the outermost location of the widget's viewport.
- Padding is space between the border and the content.

In case you want to have space before the border (similar to what CSS' `margin`
does), you can wrap your widget node in a `box` with padding applied to it.

Finally, there is also `radius`, which sets the same radius to all corners to
make them rounded instead of squared. In this case, the alternatives are
`radiusTL`, `radiusTR`, `radiusBR` and `radiusBL`. Setting radius affects both
the border and background of a widget.

Given that style settings are provided as a list, the combination logic gives
priority to the latest value set. This allows, for example, to set all sides to
one padding size and leave the right side with a different value:

```haskell
node `styleBasic` [padding 10, paddingR 0]
```

This concept of overriding values in the provided list also applies to regular
widget configuration (the rightmost value takes precedence).

## Color

Besides `textColor`, there are a few configuration options for setting colors:

- `bgColor`: sets the background color of a widget.
- `fgColor`: sets the foreground color of a widget, although it depends on the
widget how this color is used. For example, slider uses it to render the top
most layer, to indicate the current value.
- `sndColor`: secondary color of a widget. Also depends on the widget, if it 
is used at all. In the case of slider, it is used for drawing background of the
slider (note: this is not the same as the background of the widget).
- `hlColor`: highlight color of a widget. Again, how it is used depends on the
widget. In the case of slider, it is used for drawing the (optional) thumb.

For reference, the colors mentioned by name in the examples come from
`Monomer.Graphics.ColorTable`.

## Enabled and visible

Besides `styleBasic`, other two common attributes of a node you may be interested in
controlling are `nodeEnabled` and `nodeVisible`, both boolean.

```haskell
colorPicker fontColor `nodeVisible` (model ^. showPicker)
```

## New widgets

A few input widgets are used in this tutorial:

### Radio

A radio button allows choosing between mutually exclusive options. To construct
it, the value it represents must be provided, plus a lens to the associated
field. The value can be of any type, as long as it matches with its target
field's type.

By default, a `radio` does not have a caption, but it can be provided with a
label widget (generally, inside an hstack or vstack). Alternatively
`labeledRadio` can be used, which handles clicks on the label as if they were
performed on the radio widget.

### Checkbox

A checkbox allows handling boolean fields. Given only two options are available,
a `checkbox` only needs a lens to a boolean field.

There is also a `labeledCheckbox` version.

### Textfield

A `textField` allows editing free text in a single line. Several configuration
options exist to limit length, placeholder, display character (for password
fields) are available. On its most basic configuration, it requires a lens to
a text field. In case you need to support multiline editing, `textArea` is
available too.

There are specific versions for some data types, named `numericField`,
`dateField` and `timeField`, which provide options for valid ranges, formats and
also some editing conveniences (mouse wheel or shift-dragging allow for changing
the field's value).

## Model updates

Finally, in this example we can see how using lenses simplifies creating a UI.
All of the interactions are just consequence of the model being updated and the
UI rebuilt by the build UI function, without handling any specific event.

<br/>

[Next: Life cycle](03-life-cycle.md)
