## 1.2.0.0

### Added

- Add `customModelBuilder` in Composite, for custom models support. These can consume information
  from the parent model.

### Fixed

- Keep old Composite root if model has not changed. This did not affect previous code,
  it is only relevant with new features.

### Changed

- Composite requests `RenderOnce` when model changes.
- Composite now renders decorations if a style is set.
- ZStack's `onlyTopActive` now follows the same pattern as other boolean combinators.
- Shortened labels for `ColorPicker`.
- Changed `_weFindByPath` to `_weFindBranchByPath`, now returning the complete branch up to the given path.
- Add `containerCreateContainerFromModel` to workaround issue when updating offset during merge.

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
