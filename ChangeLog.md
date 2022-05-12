## 1.5.0.0 (in development)

### Added

- Support for switching vertical wheel scrolling to horizontal in scroll widget by pressing the shift key ([PR #137](https://github.com/fjvallarino/monomer/pull/137)).
- Drawing and theme utility functions ([PR #138](https://github.com/fjvallarino/monomer/pull/138)).
- `boxFilterEvent` config option, exposing Container's filterEvent functionality ([PR #146](https://github.com/fjvallarino/monomer/pull/146)).

### Fixed

- Bug where memory based image widget would not render their new state after a merge ([PR #147](https://github.com/fjvallarino/monomer/pull/147)). Thanks @CamdenKuwahara!
- Fix memory leak in NanoVGRenderer's image initialization. Dispose unused images in image widget ([PR #149](https://github.com/fjvallarino/monomer/pull/149)).

## 1.4.0.0

### Breaking changes

- Added `style...Set` family of functions ([PR #104](https://github.com/fjvallarino/monomer/pull/104)).
- `Composite`'s `onChange` event is now sent to its `handleEvent` function, not to its parent; the type of the
  generated event was updated to reflect this change. The rationale is that since `onInit` is sent to
  `handleEvent`, having `onChange` sent to its parent was confusing. At the same time there was not an easy way
  in `handleEvent` to know when the model changed. Widgets that want to report model changes to its parent can
  use `Report`/`RequestParent`; an example can be found in `ColorPicker` ([PR #71](https://github.com/fjvallarino/monomer/pull/71)).
- `Timestamp` is now a newtype. Enforce use of this type instead of `Int` when appropriate ([PR #103](https://github.com/fjvallarino/monomer/pull/103)).
- `Timestamp` was renamed to `Millisecond`. The rationale is that since both timestamps and durations are used frequently in calculations (and in the context of Monomer timestamps and durations indeed represent time in milliseconds), having separate types for Timestamp and Duration caused more harm than good ([PR #107](https://github.com/fjvallarino/monomer/pull/107)).
- `compositeMergeModel` (previously `customModelBuilder`) now receives `WidgetEnv` as its first parameter ([PR #114](https://github.com/fjvallarino/monomer/pull/114)).
- `compositeMergeReqs` now receives `parentModel` and `oldModel` too ([PR #114](https://github.com/fjvallarino/monomer/pull/114)).
- `mergeRequired` now receives an extra value as its first parameter, usually `WidgetEnv` ([PR #122](https://github.com/fjvallarino/monomer/pull/122)).

### Fixed

- Properly handle `SetFocusOnKey` for `textArea` ([#80](https://github.com/fjvallarino/monomer/issues/80)).
- Lens tutorial sample code ([PR #95](https://github.com/fjvallarino/monomer/pull/95) and [PR #98](https://github.com/fjvallarino/monomer/pull/98)). Thanks @Clindbergh!
- ColorPicker's numericFields vertical alignment ([PR #108](https://github.com/fjvallarino/monomer/pull/108)).
- Differences in glyphs positions used by `FontManager` and nanovg; temporary workaround ([PR #105](https://github.com/fjvallarino/monomer/pull/105)).
- `nodeInfoFromKey` relies on `nodeInfoFromPath` to retrieve information instead of fetching it directly from `WidgetEnv`'s `widgetKeyMap`, which can be stale ([PR #110](https://github.com/fjvallarino/monomer/pull/110)).
- Glyph positioning issues in `FontManager`; removed workaround added in #105 ([PR #125](https://github.com/fjvallarino/monomer/pull/125)).
- Will attempt to fall back to rendering on the main thread if threaded rendering setup fails ([PR #131](https://github.com/fjvallarino/monomer/pull/131)).
- Space leak in StyleUtil's mergeNodeStyleState ([PR #132](https://github.com/fjvallarino/monomer/pull/132)).

### Added

- Utility functions `rectFromPoints`, `nodeInfoFromKey`, `nodeInfoFromPath` and `findParentNodeInfoByType`.
- Allow setting the window icon via AppConfig ([PR #79](https://github.com/fjvallarino/monomer/pull/79)). Thanks @Dretch!
- Support for breaking text lines at character boundaries ([PR #86](https://github.com/fjvallarino/monomer/pull/86)). Thanks @toku-sa-n!
- Read-only mode for `textField`, `numericField`, `dateField`, `timeField` and `textArea` ([PR #93](https://github.com/fjvallarino/monomer/pull/93)). Thanks @Dretch!
- The `scroll` widget now supports a `thumbMinSize` configuration option that allows setting a minimum thumb size ([PR #100](https://github.com/fjvallarino/monomer/pull/100)).
- New field `_weAppStartTs` in `WidgetEnv`, complementary to `_weTimestamp`, representing the time in milliseconds when the application started. Added utility function `currentTimeMs` that returns their sum with a polymorphic type ([PR #103](https://github.com/fjvallarino/monomer/pull/103)).
- Several sizeReq helpers ([PR #106](https://github.com/fjvallarino/monomer/pull/106)).
- `compositeMergeEvents`, for completeness ([PR #114](https://github.com/fjvallarino/monomer/pull/114)).
- Support for symbols and other keys in `keystroke` ([PR #117](https://github.com/fjvallarino/monomer/pull/117)).
- New constructor (`buttonD_`) and `ignoreParentEvts` configuration option to `button` ([PR #123](https://github.com/fjvallarino/monomer/pull/123)).
- Allow disabling auto scale detection with `appDisableAutoScale` ([PR #128](https://github.com/fjvallarino/monomer/pull/128)).

### Changed

- The `keystroke` widget now supports the `Backspace` key ([PR #74](https://github.com/fjvallarino/monomer/pull/74)).
- `style...` family of functions now combine new attributes with the existing ones ([PR #104](https://github.com/fjvallarino/monomer/pull/104)).
- `radio` and `optionButton` now only trigger `onChange` when their value changes. `onClick` was can be used to replicate the previous `onChange` behavior ([PR #134](https://github.com/fjvallarino/monomer/pull/134)).

### Renamed

- Utility functions for retrieving `WidgetNode` information ([PR #75](https://github.com/fjvallarino/monomer/pull/75))
  - `findWidgetByPath` -> `findChildNodeInfoByPath`.
  - `findWidgetBranchByPath` -> `findChildBranchByPath`.
  - `findWidgetIdFromPath` -> `widgetIdFromPath`.
- Composite merge related ([PR #114](https://github.com/fjvallarino/monomer/pull/114))
  - `customModelBuilder` -> `compositeMergeModel`
  - `CompositeCustomModelBuilder` -> `MergeModelHandler`.

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
