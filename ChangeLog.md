## 1.1.0.1

### Fixed

- Horizontal wheel/trackpad scrolling on Linux.
- Scroll: do not use direction argument to modify wheel/trackpad direction (event provides correct value).
- Only replace composite model with user model on init and merge.

## 1.1.0.0

### Added

- Add sizeUpdater helpers. Support multiple handlers in box, grid and stack.
- Add `dpr` field to `WidgetEnv`.
- Add `RunInRenderThread` to support initialization of low level OpenGL resources.
- Add OpenGL example.
- Add `containerDrawDecorations`. Simplify button/externalLink internals.
- Add ThemeState entries for optionButton and toggleButton widgets.
- Add `singleDrawDecorations`. Make it consistent with Container.

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
