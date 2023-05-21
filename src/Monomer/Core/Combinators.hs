{-|
Module      : Monomer.Core.Combinators
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Combinator typeclasses used for style and widget configuration. The reason for
using typeclasses is for the ability to reuse names such as onClick.

Boolean combinators in general have two versions:

- combinatorName: uses the default value, normally True, and is derived from the
combinator with _ suffix.
- combinatorName_: receives a boolean parameter. This is the function that needs
to be overridden in widgets.
-}
{-# LANGUAGE FunctionalDependencies #-}

module Monomer.Core.Combinators where

import Control.Lens (ALens')
import Data.Text (Text)

import Monomer.Core.StyleTypes
import Monomer.Core.WidgetTypes
import Monomer.Event.Types
import Monomer.Graphics.Types

{-|
Given two values, usually model, checks if merge is required for a given widget.

The first parameter usually corresponds to the current 'WidgetEnv', the second
to the old value/model, and the third to the new/model.

This is used, for example, by _composite_ and _box_.
-}
class CmbMergeRequired t w s | t -> w s where
  mergeRequired :: (w -> s -> s -> Bool) -> t

{-|
Listener for the validation status of a user input field using a lens.

Allows associating a flag to know if the input of a field with validation
settings is valid. This can be used with _textField_, _numericField_,
_dateField_ and _timeField_.

The flag can be used for styling the component according to the current status.
Beyond styling, its usage is needed for validation purposes. Taking
_numericField_ as an example, one can bind a 'Double' record field to it and set
a valid range from 0 to 100. When the user inputs 100, the record field will
reflect the correct value. If the user adds a 0 (the numericField showing 1000),
the record field will still have 100 because it's the last valid value. Since
there is not a way of indicating errors when using primitive types (a 'Double'
is just a number), we can rely on the flag to check its validity.
-}
class CmbValidInput t s | t -> s where
  validInput :: ALens' s Bool -> t

{-|
Listener for the validation status of a user input field using an event handler,
avoiding the need of a lens.

Check 'CmbValidInput' for details.
-}
class CmbValidInputV t e | t -> e where
  validInputV :: (Bool -> e) -> t

-- | Defines whether a widget selects all its content when receiving focus.
class CmbSelectOnFocus t where
  selectOnFocus :: t
  selectOnFocus = selectOnFocus_ True
  selectOnFocus_ :: Bool -> t

{-|
Defines whether a widget prevents the user from changing the value. Note that, in
contrast to a disabled widget, a read-only widget can still be focused and
still allows selecting and copying the value.
-}
class CmbReadOnly t where
  readOnly :: t
  readOnly = readOnly_ True
  readOnly_ :: Bool -> t

-- | Defines whether a widget changes its size when the model changes.
class CmbResizeOnChange t where
  resizeOnChange :: t
  resizeOnChange = resizeOnChange_ True
  resizeOnChange_ :: Bool -> t

-- | Defines whether animation should start automatically.
class CmbAutoStart t where
  autoStart :: t
  autoStart = autoStart_ True
  autoStart_ :: Bool -> t

-- | Defines the animation length.
class CmbDuration t a | t -> a where
  duration :: a -> t

-- | Title caption of a widget, usually a dialog.
class CmbTitleCaption t where
  titleCaption :: Text -> t

-- | Accept caption of a widget, usually a button.
class CmbAcceptCaption t where
  acceptCaption :: Text -> t

-- | Cancel caption of a widget, usually a button.
class CmbCancelCaption t where
  cancelCaption :: Text -> t

-- | Close caption of a widget, usually a button.
class CmbCloseCaption t where
  closeCaption :: Text -> t

-- | Minimum value of a widget, usually numeric.
class CmbMinValue t a | t -> a where
  minValue :: a -> t

-- | Maximum value of a widget, usually numeric.
class CmbMaxValue t a | t -> a where
  maxValue :: a -> t

-- | Drag rate of a widget, usually numeric.
class CmbDragRate t a | t -> a where
  dragRate :: a -> t

-- | Wheel rate of a widget, usually numeric or scrollable.
class CmbWheelRate t a | t -> a where
  wheelRate :: a -> t

-- | Whether to ignore pointer events where no widget exists.
class CmbIgnoreEmptyArea t where
  ignoreEmptyArea :: t
  ignoreEmptyArea = ignoreEmptyArea_ True
  ignoreEmptyArea_ :: Bool -> t

-- | How many decimals a numeric widget accepts.
class CmbDecimals t where
  decimals :: Int -> t

-- | Max length a widget accepts.
class CmbMaxLength t where
  maxLength :: Int -> t

-- | Max lines a widget accepts.
class CmbMaxLines t where
  maxLines :: Int -> t

-- | Whether a widget accepts tab key.
class CmbAcceptTab t where
  acceptTab :: t
  acceptTab = acceptTab_ True
  acceptTab_ :: Bool -> t

-- | Whether a text based widget is multiline.
class CmbMultiline t where
  multiline :: t
  multiline = multiline_ True
  multiline_ :: Bool -> t

-- | Whether to use ellipsis or not.
class CmbEllipsis t where
  ellipsis :: t
  ellipsis = ellipsis_ True
  ellipsis_ :: Bool -> t

-- | Whether to trim spaces or not.
class CmbTrimSpaces t where
  trimSpaces :: t
  trimSpaces = trimSpaces_ True
  trimSpaces_ :: Bool -> t

-- | Whether to automatically select a value on blur (for example, dropdown).
class CmbSelectOnBlur t where
  selectOnBlur :: t
  selectOnBlur = selectOnBlur_ True
  selectOnBlur_ :: Bool -> t

-- | Placeholder to use when main value is empty.
class CmbPlaceholder t a | t -> a where
  placeholder :: a -> t

-- | Width of the caret in a text widget.
class CmbCaretWidth t a | t -> a where
  caretWidth :: a -> t

-- | Blink period of the caret in a text widget.
class CmbCaretMs t a | t -> a where
  caretMs :: a -> t

-- | Text font.
class CmbTextFont t where
  textFont :: Font -> t

-- | Text size.
class CmbTextSize t where
  textSize :: Double -> t

-- | Horizontal text spacing.
class CmbTextSpaceH t where
  textSpaceH :: Double -> t

-- | Vertical text spacing.
class CmbTextSpaceV t where
  textSpaceV :: Double -> t

-- | Text color.
class CmbTextColor t where
  textColor :: Color -> t

-- | Align text to the left.
class CmbTextLeft t where
  textLeft :: t
  textLeft = textLeft_ True
  textLeft_ :: Bool -> t

-- | Align text to the center.
class CmbTextCenter t where
  textCenter :: t
  textCenter = textCenter_ True
  textCenter_ :: Bool -> t

-- | Align text to the right.
class CmbTextRight t where
  textRight :: t
  textRight = textRight_ True
  textRight_ :: Bool -> t

-- | Align text to the top.
class CmbTextTop t where
  textTop :: t
  textTop = textTop_ True
  textTop_ :: Bool -> t

-- | Align text to the vertical middle based on the line height.
class CmbTextMiddle t where
  textMiddle :: t
  textMiddle = textMiddle_ True
  textMiddle_ :: Bool -> t

-- | Align text to the vertical middle based on the ascender.
class CmbTextAscender t where
  textAscender :: t
  textAscender = textAscender_ True
  textAscender_ :: Bool -> t

-- | Align text to the vertical middle based on the x height.
class CmbTextLowerX t where
  textLowerX :: t
  textLowerX = textLowerX_ True
  textLowerX_ :: Bool -> t

-- | Align text to the bottom.
class CmbTextBottom t where
  textBottom :: t
  textBottom = textBottom_ True
  textBottom_ :: Bool -> t

-- | Align text to the baseline.
class CmbTextBaseline t where
  textBaseline :: t
  textBaseline = textBaseline_ True
  textBaseline_ :: Bool -> t

-- | Display a line under the text.
class CmbTextUnderline t where
  textUnderline :: t
  textUnderline = textUnderline_ True
  textUnderline_ :: Bool -> t

-- | Display a line above the text.
class CmbTextOverline t where
  textOverline :: t
  textOverline = textOverline_ True
  textOverline_ :: Bool -> t

-- | Display a line over the text.
class CmbTextThroughline t where
  textThroughline :: t
  textThroughline = textThroughline_ True
  textThroughline_ :: Bool -> t

-- | How to break texts into lines.
class CmbTextLineBreak t where
  textLineBreak :: LineBreak -> t

-- | Does not apply any kind of resizing to fit to container.
class CmbFitNone t where
  fitNone :: t

-- | Fits to use all the container's space.
class CmbFitFill t where
  fitFill :: t

-- | Fits to use all the container's width.
class CmbFitWidth t where
  fitWidth :: t

-- | Fits to use all the container's height.
class CmbFitHeight t where
  fitHeight :: t

-- | Either fitWidth or fitHeight such that image does not overflow viewport.
class CmbFitEither t where
  fitEither :: t

-- | Applies nearest filtering when stretching an image.
class CmbImageNearest t where
  imageNearest :: t

-- | Applies horizontal repetition when stretching an image.
class CmbImageRepeatX t where
  imageRepeatX :: t

-- | Applies vertical repetition when stretching an image.
class CmbImageRepeatY t where
  imageRepeatY :: t

-- | The color of a bar, for example in a scroll.
class CmbBarColor t where
  barColor :: Color -> t

-- | The hover color of a bar, for example in a scroll.
class CmbBarHoverColor t where
  barHoverColor :: Color -> t

-- | The width of a bar, for example in a scroll.
class CmbBarWidth t where
  barWidth :: Double -> t

-- | The color of a thumb, for example in a scroll.
class CmbThumbColor t where
  thumbColor :: Color -> t

-- | The hover color of a thumb, for example in a scroll.
class CmbThumbHoverColor t where
  thumbHoverColor :: Color -> t

{-|
The thumb factor. For example, in slider this makes the thumb proportional to
the width of the slider.
-}
class CmbThumbFactor t where
  thumbFactor :: Double -> t

-- | The radius of a thumb's rect, for example in a scroll.
class CmbThumbRadius t where
  thumbRadius :: Double -> t

-- | Whether the thumb is visible, for example in a slider.
class CmbThumbVisible t where
  thumbVisible :: t
  thumbVisible = thumbVisible_ True
  thumbVisible_ :: Bool -> t

-- | The width of a thumb, for example in a scroll.
class CmbThumbWidth t where
  thumbWidth :: Double -> t

-- | The minimum size of a thumb, for example in a scroll.
class CmbThumbMinSize t where
  thumbMinSize :: Double -> t

-- | Whether to show an alpha channel, for instance in color selector.
class CmbShowAlpha t where
  showAlpha :: t
  showAlpha = showAlpha_ True
  showAlpha_ :: Bool -> t

{-|
Whether to ignore children events.

By default low-level events (keyboard, mouse, clipboard, etc) traverse the whole
branch where the target widget is located in the widget tree, giving the chance
to each widget along the line to respond to the event.

In some cases it is desirable to restrict which widgets can handle an event. Two
different 'WidgetRequest's, which can be returned during event handling, exist
for this:

- 'IgnoreChildrenEvents': parent widgets always have the priority. If a widget
  returns this 'WidgetRequest' during event handling, its children widgets
  response will be ignored. For example, the _keystroke_ widget can be
  configured to return this when a keystroke combination matches.
- 'IgnoreParentEvents': if no parent widget requested 'IgnoreChildrenEvents', a
  widget can respond with 'IgnoreParentEvents' to have its response being the
  only one taking place. This is used, for example, by the _textArea_ widget to
  handle the tab key; without this, the default handler would pass focus to the
  next widget down the line.

Some of the stock widgets allow configuring this behavior (e.g, keystroke and
button).
-}
class CmbIgnoreChildrenEvts t where
  ignoreChildrenEvts :: t
  ignoreChildrenEvts = ignoreChildrenEvts_ True
  ignoreChildrenEvts_ :: Bool -> t

-- | Whether to ignore parent events. Check 'CmbIgnoreChildrenEvts'.
class CmbIgnoreParentEvts t where
  ignoreParentEvts :: t
  ignoreParentEvts = ignoreParentEvts_ True
  ignoreParentEvts_ :: Bool -> t

-- | On init event.
class CmbOnInit t e | t -> e where
  onInit :: e -> t

-- | On init WidgetRequest.
class CmbOnInitReq t s e | t -> s e where
  onInitReq :: WidgetRequest s e -> t

-- | On dispose event.
class CmbOnDispose t e | t -> e where
  onDispose :: e -> t

-- | On dispose WidgetRequest.
class CmbOnDisposeReq t s e | t -> s e where
  onDisposeReq :: WidgetRequest s e -> t

-- | On resize event.
class CmbOnResize t e a | t -> e a where
  onResize :: (a -> e) -> t

-- | On focus event.
class CmbOnFocus t e a | t -> e a where
  onFocus :: (a -> e) -> t

-- | On focus WidgetRequest.
class CmbOnFocusReq t s e a | t -> s e a where
  onFocusReq :: (a -> WidgetRequest s e) -> t

-- | On blur event.
class CmbOnBlur t e a | t -> e a where
  onBlur :: (a -> e) -> t

-- | On blur WidgetRequest.
class CmbOnBlurReq t s e a | t -> s e a where
  onBlurReq :: (a -> WidgetRequest s e) -> t

-- | On enter event.
class CmbOnEnter t e | t -> e where
  onEnter :: e -> t

-- | On enter WidgetRequest.
class CmbOnEnterReq t s e | t -> s e where
  onEnterReq :: WidgetRequest s e -> t

-- | On leave event.
class CmbOnLeave t e | t -> e where
  onLeave :: e -> t

-- | On leave WidgetRequest.
class CmbOnLeaveReq t s e | t -> s e where
  onLeaveReq :: WidgetRequest s e -> t

-- | On click event.
class CmbOnClick t e | t -> e where
  onClick :: e -> t

-- | On click WidgetRequest.
class CmbOnClickReq t s e | t -> s e where
  onClickReq :: WidgetRequest s e -> t

-- | On click empty event, where supported (box, for example).
class CmbOnClickEmpty t e | t -> e where
  onClickEmpty :: e -> t

-- | On click empty WidgetRequest, where supported (box, for example).
class CmbOnClickEmptyReq t s e | t -> s e where
  onClickEmptyReq :: WidgetRequest s e -> t

-- | On button pressed event.
class CmbOnBtnPressed t e | t -> e where
  onBtnPressed :: (Button -> Int -> e) -> t

-- | On button pressed WidgetRequest.
class CmbOnBtnPressedReq t s e | t -> s e where
  onBtnPressedReq :: (Button -> Int -> WidgetRequest s e) -> t

-- | On button released event.
class CmbOnBtnReleased t e | t -> e where
  onBtnReleased :: (Button -> Int -> e) -> t

-- | On button released WidgetRequest.
class CmbOnBtnReleasedReq t s e | t -> s e where
  onBtnReleasedReq :: (Button -> Int -> WidgetRequest s e) -> t

-- | On enabled change event.
class CmbOnEnabledChange t e | t -> e where
  onEnabledChange :: e -> t

-- | On visible change event.
class CmbOnVisibleChange t e | t -> e where
  onVisibleChange :: e -> t

-- | On change event.
class CmbOnChange t a e | t -> e where
  onChange :: (a -> e) -> t

-- | On change event, including index.
class CmbOnChangeIdx t e a | t -> e a where
  onChangeIdx :: (Int -> a -> e) -> t

-- | On change WidgetRequest.
class CmbOnChangeReq t s e a | t -> s e a where
  onChangeReq :: (a -> WidgetRequest s e) -> t

-- | On change WidgetRequest, including index.
class CmbOnChangeIdxReq t s e a | t -> s e a where
  onChangeIdxReq :: (Int -> a -> WidgetRequest s e) -> t

-- | On load error event.
class CmbOnLoadError t e a | t -> e a where
  onLoadError :: (a -> e) -> t

-- | On finished event.
class CmbOnFinished t e | t -> e where
  onFinished :: e -> t

-- | On finished WidgetRequest.
class CmbOnFinishedReq t s e | t -> s e where
  onFinishedReq :: WidgetRequest s e -> t

-- | Width combinator.
class CmbWidth t where
  width :: Double -> t

-- | Height combinator.
class CmbHeight t where
  height :: Double -> t

-- | Flex width combinator.
class CmbFlexWidth t where
  flexWidth :: Double -> t

-- | Flex height combinator.
class CmbFlexHeight t where
  flexHeight :: Double -> t

-- | Min width combinator.
class CmbMinWidth t where
  minWidth :: Double -> t

-- | Min height combinator.
class CmbMinHeight t where
  minHeight :: Double -> t

-- | Max width combinator.
class CmbMaxWidth t where
  maxWidth :: Double -> t

-- | Max height combinator.
class CmbMaxHeight t where
  maxHeight :: Double -> t

-- | Expand width combinator.
class CmbExpandWidth t where
  expandWidth :: Double -> t

-- | Expand height combinator.
class CmbExpandHeight t where
  expandHeight :: Double -> t

-- | Range width combinator.
class CmbRangeWidth t where
  rangeWidth :: Double -> Double -> t

-- | Range height combinator.
class CmbRangeHeight t where
  rangeHeight :: Double -> Double -> t

-- | Custom SizeReq width combinator.
class CmbSizeReqW t where
  sizeReqW :: SizeReq -> t

-- | Custom SizeReq height combinator.
class CmbSizeReqH t where
  sizeReqH :: SizeReq -> t

-- | SizeReq updater. Useful to make modifications to widget SizeReqs without
--   completely overriding them.
class CmbSizeReqUpdater t where
  sizeReqUpdater :: ((SizeReq, SizeReq) -> (SizeReq, SizeReq)) -> t

-- | Resize factor combinator. A value of 0 represents fixed size.
class CmbResizeFactor t where
  resizeFactor :: Double -> t

-- | Resize factor combinator for individual w and h components. A value of 0
--   represents fixed size.
class CmbResizeFactorDim t where
  resizeFactorW :: Double -> t
  resizeFactorH :: Double -> t

-- Style
infixl 5 `styleBasic`
infixl 5 `styleBasicSet`

infixl 5 `styleHover`
infixl 5 `styleHoverSet`

infixl 5 `styleFocus`
infixl 5 `styleFocusSet`

infixl 5 `styleFocusHover`
infixl 5 `styleFocusHoverSet`

infixl 5 `styleActive`
infixl 5 `styleActiveSet`

infixl 5 `styleDisabled`
infixl 5 `styleDisabledSet`

{-|
Basic style combinator, mainly used infix with widgets.

Represents the default style of a widget. It serves as the base for all the
other style states when an attribute is not overridden.
-}
class CmbStyleBasic t where
  -- | Merges the new basic style states with the existing ones.
  styleBasic :: t -> [StyleState] -> t
  -- | Sets the new basic style states overriding the existing ones.
  styleBasicSet :: t -> [StyleState] -> t

{-|
Hover style combinator, mainly used infix with widgets.

Used when the widget is hovered with a pointing device.
-}
class CmbStyleHover t where
  -- | Merges the new hover style states with the existing ones.
  styleHover :: t -> [StyleState] -> t
  -- | Sets the new hover style states overriding the existing ones.
  styleHoverSet :: t -> [StyleState] -> t

{-|
Focus style combinator, mainly used infix with widgets.

Used when the widget has keyboard focus.
-}
class CmbStyleFocus t where
  -- | Merges the new focus style states with the existing ones.
  styleFocus :: t -> [StyleState] -> t
  -- | Sets the new focus style states overriding the existing ones.
  styleFocusSet :: t -> [StyleState] -> t

{-|
Focus Hover style combinator, mainly used infix with widgets.

Used when the widget is both focused and hovered. In this situation the
attributes defined in focus and hover will be combined, with focus attributes
taking precedence. This style state allows for better control in cases when the
combination of focus and hover styles do not match expectations.
-}
class CmbStyleFocusHover t where
  -- | Merges the new focus hover style states with the existing ones.
  styleFocusHover :: t -> [StyleState] -> t
  -- | Sets the new focus hover style states overriding the existing ones.
  styleFocusHoverSet :: t -> [StyleState] -> t

{-|
Active style combinator, mainly used infix with widgets.

Used when a mouse press was started in the widget and the pointer is inside its
boundaries.
-}
class CmbStyleActive t where
  -- | Merges the new active style states with the existing ones.
  styleActive :: t -> [StyleState] -> t
  -- | Sets the new active style states overriding the existing ones.
  styleActiveSet :: t -> [StyleState] -> t

{-|
Disabled style combinator, mainly used infix with widgets.

Used when the _nodeEnabled_ attribute has been set to False.
-}
class CmbStyleDisabled t where
  -- | Merges the new disabled style states with the existing ones.
  styleDisabled :: t -> [StyleState] -> t
  -- | Sets the new disabled style states overriding the existing ones.
  styleDisabledSet :: t -> [StyleState] -> t

-- | Ignore theme settings and start with blank style.
class CmbIgnoreTheme t where
  ignoreTheme :: t
  ignoreTheme = ignoreTheme_ True
  ignoreTheme_ :: Bool -> t

-- | Background color.
class CmbBgColor t where
  bgColor :: Color -> t

-- | Foreground color.
class CmbFgColor t where
  fgColor :: Color -> t

-- | Secondary color.
class CmbSndColor t where
  sndColor :: Color -> t

-- | Highlight color.
class CmbHlColor t where
  hlColor :: Color -> t

-- | Transparency level.
class CmbTransparency t where
  transparency :: Double -> t

-- | Cursor icons.
class CmbCursorIcon t where
  cursorArrow :: t
  cursorArrow = cursorIcon CursorArrow
  cursorHand :: t
  cursorHand = cursorIcon CursorHand
  cursorIBeam :: t
  cursorIBeam = cursorIcon CursorIBeam
  cursorInvalid :: t
  cursorInvalid = cursorIcon CursorInvalid
  cursorSizeH :: t
  cursorSizeH = cursorIcon CursorSizeH
  cursorSizeV :: t
  cursorSizeV = cursorIcon CursorSizeV
  cursorDiagTL :: t
  cursorDiagTL = cursorIcon CursorDiagTL
  cursorDiagTR :: t
  cursorDiagTR = cursorIcon CursorDiagTR
  cursorSizeAll :: t
  cursorSizeAll = cursorIcon CursorSizeAll
  cursorWait :: t
  cursorWait = cursorIcon CursorWait
  cursorWaitArrow :: t
  cursorWaitArrow = cursorIcon CursorWaitArrow
  cursorCrosshair :: t
  cursorCrosshair = cursorIcon CursorCrosshair
  cursorIcon :: CursorIcon -> t

-- | Basic style for each item of a list.
class CmbItemBasicStyle t s | t -> s where
  itemBasicStyle :: s -> t

-- | Hover style for an item of a list.
class CmbItemHoverStyle t s | t -> s where
  itemHoverStyle :: s -> t

-- | Selected style for an item of a list.
class CmbItemSelectedStyle t s | t -> s where
  itemSelectedStyle :: s -> t

-- | Align object to the left (not text).
class CmbAlignLeft t where
  alignLeft :: t
  alignLeft = alignLeft_ True
  alignLeft_ :: Bool -> t

-- | Align object to the horizontal center (not text).
class CmbAlignCenter t where
  alignCenter :: t
  alignCenter = alignCenter_ True
  alignCenter_ :: Bool -> t

-- | Align object to the right (not text).
class CmbAlignRight t where
  alignRight :: t
  alignRight = alignRight_ True
  alignRight_ :: Bool -> t

-- | Align object to the top (not text).
class CmbAlignTop t where
  alignTop :: t
  alignTop = alignTop_ True
  alignTop_ :: Bool -> t

-- | Align object to the vertical middle (not text).
class CmbAlignMiddle t where
  alignMiddle :: t
  alignMiddle = alignMiddle_ True
  alignMiddle_ :: Bool -> t

-- | Align object to the bottom (not text).
class CmbAlignBottom t where
  alignBottom :: t
  alignBottom = alignBottom_ True
  alignBottom_ :: Bool -> t

-- | Set padding to the same size on all sides.
class CmbPadding t where
  padding :: Double -> t

-- | Set padding for the left side.
class CmbPaddingL t where
  paddingL :: Double -> t

-- | Set padding for the right side.
class CmbPaddingR t where
  paddingR :: Double -> t

-- | Set padding for the top side.
class CmbPaddingT t where
  paddingT :: Double -> t

-- | Set padding for the bottom side.
class CmbPaddingB t where
  paddingB :: Double -> t

-- | Set border to the same style on all sides.
class CmbBorder t where
  border :: Double -> Color -> t

-- | Set border for the left side.
class CmbBorderL t where
  borderL :: Double -> Color -> t

-- | Set border for the right side.
class CmbBorderR t where
  borderR :: Double -> Color -> t

-- | Set border for the top side.
class CmbBorderT t where
  borderT :: Double -> Color -> t

-- | Set border for the bottom side.
class CmbBorderB t where
  borderB :: Double -> Color -> t

-- | Set radius to the same size on all corners.
class CmbRadius t where
  radius :: Double -> t

-- | Set radius for the top left corner.
class CmbRadiusTL t where
  radiusTL :: Double -> t

-- | Set radius for the top right corner.
class CmbRadiusTR t where
  radiusTR :: Double -> t

-- | Set radius for the bottom left corner.
class CmbRadiusBL t where
  radiusBL :: Double -> t

-- | Set radius for the bottom right corner.
class CmbRadiusBR t where
  radiusBR :: Double -> t

-- | Set the spacing between the children of the container.
class CmbChildSpacing t where
  -- | Set the spacing to the default amount (may vary by container).
  childSpacing :: t
  childSpacing = childSpacing_ 10
  -- | Set the spacing to the specified amount.
  childSpacing_ :: Double -> t
