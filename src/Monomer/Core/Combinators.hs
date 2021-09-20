{-|
Module      : Monomer.Core.Combinators
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Combinator typeclasses used for style and widget configutation. The reason for
using typeclasses is for the ability to reuse names such as onClick.

Boolean combinators in general have two versions:

- combinatorName: uses the default value, normally True, and is derived from the
combinator with _ suffix.
- combinatorName_: receives a boolean parameter. This is the function that needs
to be overriden in widgets.
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
The first parameter corresponds to the old value, and the second to the new.
-}
class CmbMergeRequired t s | t -> s where
  mergeRequired :: (s -> s -> Bool) -> t

-- | Listener for the validation status of a field using a lens.
class CmbValidInput t s | t -> s where
  validInput :: ALens' s Bool -> t

-- | Listener for the validation status of a field using an event handler.
class CmbValidInputV t e | t -> e where
  validInputV :: (Bool -> e) -> t

-- | Defines whether a widget selects all its content when receiving focus.
class CmbSelectOnFocus t where
  selectOnFocus :: t
  selectOnFocus = selectOnFocus_ True
  selectOnFocus_ :: Bool -> t

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
The thumb factor. For example, in slider this makes the thumb proportional
to the width of the slider.
-}
class CmbThumbFactor t where
  thumbFactor :: Double -> t

-- | The radius of a thumb's rect, for example in a scroll.
class CmbThumbRadius t where
  thumbRadius :: Double -> t

-- | Whether the thumb is visible, for example in a scroll.
class CmbThumbVisible t where
  thumbVisible :: t
  thumbVisible = thumbVisible_ True
  thumbVisible_ :: Bool -> t

-- | The width color of a thumb, for example in a scroll.
class CmbThumbWidth t where
  thumbWidth :: Double -> t

-- | Whether to show an alpha channel, for instance in color selector.
class CmbShowAlpha t where
  showAlpha :: t
  showAlpha = showAlpha_ True
  showAlpha_ :: Bool -> t

-- | Whether to ignore children events.
class CmbIgnoreChildrenEvts t where
  ignoreChildrenEvts :: t
  ignoreChildrenEvts = ignoreChildrenEvts_ True
  ignoreChildrenEvts_ :: Bool -> t

-- | On init event.
class CmbOnInit t e | t -> e where
  onInit :: e -> t

-- | On dispose event.
class CmbOnDispose t e | t -> e where
  onDispose :: e -> t

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
infixl 5 `styleHover`
infixl 5 `styleFocus`
infixl 5 `styleFocusHover`
infixl 5 `styleActive`
infixl 5 `styleDisabled`

-- | Basic style combinator, used mainly infix for widgets as a list.
class CmbStyleBasic t where
  styleBasic :: t -> [StyleState] -> t

-- | Hover style combinator, used mainly infix for widgets as a list.
class CmbStyleHover t where
  styleHover :: t -> [StyleState] -> t

-- | Focus style combinator, used mainly infix for widgets as a list.
class CmbStyleFocus t where
  styleFocus :: t -> [StyleState] -> t

-- | Focus Hover style combinator, used mainly infix for widgets as a list.
class CmbStyleFocusHover t where
  styleFocusHover :: t -> [StyleState] -> t

-- | Active style combinator, used mainly infix for widgets as a list.
class CmbStyleActive t where
  styleActive :: t -> [StyleState] -> t

-- | Disabled style combinator, used mainly infix for widgets as a list.
class CmbStyleDisabled t where
  styleDisabled :: t -> [StyleState] -> t

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

-- | Align object to the center (not text).
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

-- | Align object to the middle (not text).
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
