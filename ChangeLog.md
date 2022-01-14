## 1.3.0.0 (in development)

### Fixed

- Widgets that receive polymorphic types now append the handled type to their `WidgetType`. This
  is done to avoid issues if the handled type is later changed (https://github.com/fjvallarino/monomer/issues/46).
- If the `WidgetType` of the root item in a `Composite` changes during `merge`, initialize the new widget instead
  of merging with the old one (https://github.com/fjvallarino/monomer/issues/50).
- The arrow position in `dropdown` is now correct when a dropdown is taller than one line (thanks @Dretch!).
- The middle button click is now handled by `convertEvents`, and in turn reported to widgets.

### Added

- Export `drawArrowUp` from `Drawing` module.
- The `image` widget now supports a `fitEither` option (thanks @Kyarigwo!).
- The `scroll` widget now raises `onChange` events, providing the current `ScrollStatus`.

### Changed

## 1.2.0.1

### Fixed

- The `scroll` and `split` widgets now avoid unexpected behaviour when visibility is toggled.

## 1.2.0.0

### Added

- Add `customModelBuilder` in Composite, for custom models support. These can consume information
  from the parent model.
- Add `containerCreateContainerFromModel` to workaround issue when updating offset during merge.
- Add `appDisableCompositing` to allow requesting compositing to be disabled on startup.
- Add `optionButton` and `toggleButton` widgets.
- Add `SetFocusOnKey` and `MoveFocusFromKey` actions in `Composite`. Deprecate `setFocusOnKey` function.
  This function depended on information in `WidgetEnv`, which can become stale if several actions are
  returned at once. This change reduces confusion regarding order of operations and widget tree state.

### Fixed

- Keep old Composite root if model has not changed. This does not affect previous code,
  it is only relevant with new features.
- Generate `IgnoreParentEvents` request from widgets that handle Wheel event (avoids issues with scroll widget moving the content).
- Do not run tests which depend on SDL's video subsystem to be available unless an environment variable is defined.
  This allows for (hopefully) running tests on Hackage and, later on, deploying to Stackage.

### Changed

- Composite requests `RenderOnce` when model changes.
- Composite now renders decorations if a style is set.
- ZStack's `onlyTopActive` now follows the same pattern as other boolean combinators.
- Shortened labels for `ColorPicker`.
- Changed `_weFindByPath` to `_weFindBranchByPath`, now returning the complete branch up to the given path.
- Change SDL's default of requesting compositing to be disabled on startup (compositing is now left unchanged).
- Filter following `TextInput` event if a single letter binding matched previously on `keystroke`.

## 1.1.1.0

### Added

- `appInvertWheelX` and `appInvertWheelY` configuration options.

### Fixed

- Horizontal wheel/trackpad scrolling on Linux.
- Scroll: do not use direction argument to modify wheel/trackpad direction (event provides correct value).
- Only replace composite model with user model on init and merge.

## 1.1.0.0

### Added

- `sizeUpdater` helpers. Support multiple handlers in box, grid and stack.
- `dpr` field to `WidgetEnv`.
- `RunInRenderThread` to support initialization of low level OpenGL resources.
- OpenGL example.
- `containerDrawDecorations`. Simplify button/externalLink internals.
- `ThemeState` entries for future `optionButton` and `toggleButton` widgets.
- `singleDrawDecorations`. Make it consistent with Container.

### Fixed

- Reduce memory usage by sharing wreq session among image widget instances.
- Set correct `WidgetEnv` viewport from scroll (it looked good because of scissoring).
- Fix issue with scrollbars using child coordinates for detecting clicks.

## 1.0.0.3

### Added

- Add Nix and GitHub Actions support (thanks @smunix!).

### Fixed

- Consume and forward all available messages from Producers on each cycle.
- Fix space leak when rebuilding the UI or handling events.

## 1.0.0.2

### Added

- Add `appRenderOnMainThread` option.

### Changed

- Use the recently published nanovg-0.8.0.0 from Hackage, instead of the version from the PR's commit.

## 1.0.0.1

### Fixed

- Fix Haddocks for widget configuration types.

## 1.0.0.0

### Initial public release

- Supports Windows, Linux and macOS
