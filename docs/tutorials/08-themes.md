# Themes

Themes provide a way of customizing the base style of widgets across the entire
application. While in general you can customize the style of widgets on a per
instance basis, and you can create regular functions that return widgets styled
the way you need, themes provide a couple of useful features:

- They provide a quick way of switching the active theme on a given section of
  the application. This is especially useful for changing colors.
- They allow customizing widgets that you may not have direct control over, such
  as the scrollbar of a textArea or the background color of a dialog.

## Switching themes

The `themeSwitcher` widget, which receives a theme instance, optional config and
a single child widget, allows replacing the main application theme with a new
one for a specific section.

As shown in the accompanying application, one use case for themes is to allow
changing the style of an application at runtime. Since the example uses the
switcher at the top level, and the background color plays an important role, we
enable the corresponding config flag (disabled by default).

```haskell
themeSwitch_ theme [themeClearBg] childNode
```

Another use case for theme switching is when we want to customize composite
widgets, such as dialogs, which do not expose style configuration options. In
those cases, theme switching allows us to alter the default styles of child
widgets such as buttons, scrolls, etc.

## BaseTheme

## General Theme type
