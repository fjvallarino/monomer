### 1.1.0.1

- Fix horizontal wheel/trackpad scrolling on Linux.
- Scroll: do not use direction argument to modify wheel/trackpad direction (event provides correct value).

### 1.1.0.0

- Reduce memory usage by sharing wreq session among image widget instances.
- Add sizeUpdater helpers. Support multiple handlers in box, grid and stack.
- Add `dpr` field to `WidgetEnv`.
- Add `RunInRenderThread` to support initialization of low level OpenGL resources.
- Set correct `WidgetEnv` viewport from scroll (it looked good because of scissoring).
- Fix issue with scrollbars using child coordinates for detecting clicks.
- Add OpenGL example.
- Add `containerDrawDecorations`. Simplify button/externalLink internals.
- Add ThemeState entries for optionButton and toggleButton widgets.
- Add `singleDrawDecorations`. Make it consistent with Container.

### 1.0.0.3

- Consume and forward all available messages from Producers on each cycle.
- Add Nix and GitHub Actions support (thanks @smunix!).
- Fix space leak when rebuilding the UI or handling events.

### 1.0.0.2

- Use the recently published nanovg-0.8.0.0 from Hackage, instead of the version from the PR's commit.
- Add `appRenderOnMainThread` option.

### 1.0.0.1

- Fix Haddocks for widget configuration types.

### 1.0.0.0

Initial public release

- Supports Windows, Linux and macOS
