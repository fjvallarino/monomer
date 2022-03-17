## 1.4.0.0 (in development)

### Fixed

- Properly handle `SetFocusOnKey` for `textArea` ([#80](https://github.com/fjvallarino/monomer/issues/80)).
- Lens tutorial sample code ([PR #95](https://github.com/fjvallarino/monomer/pull/95) and [PR #98](https://github.com/fjvallarino/monomer/pull/98)). Thanks @Clindbergh!

### Added

- Utility functions `rectFromPoints`, `nodeInfoFromKey`, `nodeInfoFromPath` and `findParentNodeInfoByType`.
- Allow setting the window icon via AppConfig ([PR #79](https://github.com/fjvallarino/monomer/pull/79)). Thanks @Dretch!
- Support for breaking text lines at character boundaries ([PR #86](https://github.com/fjvallarino/monomer/pull/86)). Thanks @toku-sa-n!
- Read-only mode for `textField`, `numericField`, `dateField`, `timeField` and `textArea` ([PR #93](https://github.com/fjvallarino/monomer/pull/93)). Thanks @Dretch!
- The `scroll` widget now supports a `thumbMinSize` configuration option that allows setting a minimum thumb size ([PR #100](https://github.com/fjvallarino/monomer/pull/100)).

### Changed

- `Composite`'s `onChange` event is now sent to its `handleEvent` function, not to its parent; the type of the
  generated event was updated to reflect this change. The rationale is that since `onInit` is sent to
  `handleEvent`, having `onChange` sent to its parent was confusing. At the same time there was not an easy way
  in `handleEvent` to know when the model changed. Widgets that want to report model changes to its parent can
  use `Report`/`RequestParent`; an example can be found in `ColorPicker` ([PR #71](https://github.com/fjvallarino/monomer/pull/71)).
- The `keystroke` widget now supports the `Backspace` key ([PR #74](https://github.com/fjvallarino/monomer/pull/74)).

### Renamed

- Utility functions for retrieving `WidgetNode` information ([PR #75](https://github.com/fjvallarino/monomer/pull/75))
  - `findWidgetByPath` -> `findChildNodeInfoByPath`.
  - `findWidgetBranchByPath` -> `findChildBranchByPath`.
  - `findWidgetIdFromPath` -> `widgetIdFromPath`.

### Removed

- Dependencies on `OpenGL`, `Safe`, `scientific`, `unordered-containers`, `directory`, `HUnit` and `silently` ([PR #70](https://github.com/fjvallarino/monomer/pull/70)).

## 1.3.0.0

### Added

- Export `drawArrowUp` from `Drawing` module.
- The `image` widget now supports a `fitEither` option ([PR #56](https://github.com/fjvallarino/monomer/pull/56)). Thanks @Kyarigwo!
- The `scroll` widget now raises `onChange` events, providing the current `ScrollStatus` ([PR #51](https://github.com/fjvallarino/monomer/pull/51)).
- The `grid`, `stack`, `labeledCheckbox` and `labeledRadio` widgets now support a `childSpacing`/`childSpacing_` option
  ([PR #67](https://github.com/fjvallarino/monomer/pull/67)). Thanks @Dretch!

### Fixed

- Widgets that receive polymorphic types now append the handled type to their `WidgetType`. This
  is done to avoid issues if the handled type is later changed ([#46](https://github.com/fjvallarino/monomer/issues/46)).
- If the `WidgetType` of the root item in a `Composite` changes during `merge`, initialize the new widget instead
  of merging with the old one ([#50](https://github.com/fjvallarino/monomer/issues/50)).
- The arrow position in `dropdown` is now correct when a dropdown is taller than one line ([PR #55](https://github.com/fjvallarino/monomer/pull/55)). Thanks @Dretch!
- The middle button click is now handled by `convertEvents`, and in turn reported to widgets ([#63](https://github.com/fjvallarino/monomer/issues/63)).
- Add tolerance to width comparison in text clipping functions ([#54](https://github.com/fjvallarino/monomer/issues/54)).
- Call `pumpEvents` before `pollEvents`. The `pumpEvents` call is implied by `pollEvent`, but starting on
  SDL2 `2.0.20` it seems to be required to call it explicitly ([#66](https://github.com/fjvallarino/monomer/issues/66)). Thanks @JD95!

### Changed

- Relaxed upper bounds of dependencies for Stackage inclusion.

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
