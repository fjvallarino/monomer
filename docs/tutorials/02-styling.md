# Styling

## Text

Before you can display a label, you need to load fonts into the system. This can
be done using the application's config. The monomer-starter project includes the
Roboto fonts for Regular, Bold and Italic, and they are loaded this way:

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

There are several functions you can use to alter widget node. These functions
are generally used infix, since the intention is clearer that way. In
particular, the `styleBasic` function allows providing different style options
with a list.

Since this style is used a few times in the example, it would be nice to avoid
duplicating the code all over the example. An easy way to avoid this is to
create a function that returns the label styled as needed:

```haskell
titleText text = label text `styleBasic` [textFont "Medium", textSize 20]
```

In general, all components which display text support the following styles:

- textFont
- textSize
- textColor
- textUnderline/textOverline/textThroughline
- textCenter/textLeft/textRight
- textMiddle/textTop/textBottom

All dimensions in Monomer are expressed in pixels.

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
