# Themes

Themes provide a way of customizing the base style of widgets across the entire
application. While in general you can customize the style of widgets on a per
instance basis, and you can create regular functions that return widgets styled
the way you need, themes provide a couple of useful features:

- They provide a quick way of switching the active theme on a given section of
  the application. This is specially useful for changing the color scheme.
- They allow customizing widgets that you may not have direct control over, such
  as the scrollbar of a textArea or the background color of a dialog.

## Switching themes

The `themeSwitcher` widget, which receives a theme instance, optional config and
a single child widget, allows replacing the main application theme with a new
one for a specific section.

As shown in the accompanying example, one use case for themes is to allow
changing the style of an application at runtime. Since the example uses the
switcher at the top level, and the background color plays an important role, we
enable the corresponding config flag (disabled by default).

```haskell
themeSwitch_ theme [themeClearBg] $
  vstack [...]
```

Another use case for theme switching is when we want to customize composite
widgets, such as dialogs, which do not expose style configuration options. In
those cases, theme switching allows us to alter the default styles of child
widgets such as buttons, scrolls, etc.

## BaseTheme

The two provided themes, `lightTheme` and `darkTheme`, are created from the same
styling configuration. Creating a theme that only customizes the colors from the
base theme is relatively easy, at least in configuration terms.

The `baseTheme` function receives a `BaseThemeColors` instance, which defines
colors for different widgets in the corresponding style states. It also includes
the `clearColor` (background) and `sectionColor`, which can be used to highlight
parts of the UI.

```haskell
data BaseThemeColors = BaseThemeColors {
  clearColor :: Color,
  sectionColor :: Color,
  btnFocusBorder :: Color,
  btnBgBasic :: Color,
  btnBgHover :: Color,
  btnBgFocus :: Color,
  btnBgActive :: Color,
  btnBgDisabled :: Color,
  btnText :: Color,
  btnTextDisabled :: Color,
  ...
}
```

Both `lightTheme` and `darkTheme` can serve as an idea of how to create a custom
version. They have their `BaseThemeColors` instances exported, named
`lightThemeColors` and `darkThemeColors`.

```haskell
customTheme :: Theme
customTheme = baseTheme darkThemeColors {
  btnMainBgBasic = rgbHex "#EE9000",
  btnMainBgHover = rgbHex "#FFB522",
  btnMainBgFocus = rgbHex "#FFA500",
  btnMainBgActive = rgbHex "#DD8000",
  btnMainBgDisabled = rgbHex "#BB8800",
  btnMainText = rgbHex "000000"
}
```

## General Theme type

If you want to change the default padding, corners, etc, you need to create a
custom `Theme`. Similarly to `Style`, the `Theme` type has an attribute for each
of the `ThemeState` you need to provide.

You can start from scratch or you can make modifications to the provided themes.
There is not an automatic way for updating all the theme states at once, so you
will need to provide the configuration for each of them individually. You can
use `baseTheme` as a reference on how to create your own theme from scratch.

```haskell
data Theme = Theme {
  _themeClearColor :: Color,
  _themeSectionColor :: Color,
  _themeUserColorMap :: M.Map String Color,
  _themeBasic :: ThemeState,
  _themeHover :: ThemeState,
  _themeFocus :: ThemeState,
  _themeFocusHover :: ThemeState,
  _themeActive :: ThemeState,
  _themeDisabled :: ThemeState
}
```

### User colors

Besides the widgets provided by the library, you may have created your own
widgets that need theming support. You might also want to define colors for
specific parts of the UI that do not belong to a specific state. For these cases
you have:

- userColorMap: in the theme type and in each of the theme states. It is used in
  some of the examples to define colors for parts of the UI that may benefit
  from having colors tailored to the active theme.
- userStyleMap: in each of the theme states. It can be used to define custom
  styles for your own widgets in the same way they are defined for the stock
  ones.

## Using themes from widgets

Both `Single` and `Container` types support a `GetBaseStyle` function. In
general, the implementation will be similar to:

```haskell
getBaseStyle :: WidgetEnv s e -> WidgetNode s e -> Maybe Style
getBaseStyle wenv node = Just style where
  style = collectUserTheme wenv "myWidget"
```

Returning Nothing from this function means the widget does not have a default
style applied. Customizations always take precedence over the theme style.
