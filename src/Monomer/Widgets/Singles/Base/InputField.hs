{-|
Module      : Monomer.Widgets.Singles.Base.InputField
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Base single line text editing field. Extensible for handling specific textual
representations of other types, such as numbers and dates. It is not meant for
direct use, but to create custom widgets using it.

See 'NumericField', 'DateField' and 'TimeField'.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Monomer.Widgets.Singles.Base.InputField (
  InputFieldCfg(..),
  InputFieldState(..),
  inputField_
) where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Lens
  (ALens', (&), (.~), (?~), (%~), (^.), (^?), _2, _Just, cloneLens, non)
import Data.Default
import Data.Maybe
import Data.Sequence (Seq(..), (|>))
import Data.Text (Text)
import Data.Typeable
import GHC.Generics

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Helper
import Monomer.Widgets.Single

import qualified Monomer.Lens as L

-- | Constaints for a value handled by input field.
type InputFieldValue a = (Eq a, Show a, Typeable a)

{-|
Handler for wheel events. Useful for values on which increase/decrease makes
sense.
-}
type InputWheelHandler a
  = InputFieldState a        -- ^ The state of the input field
  -> Point                   -- ^ The mouse position.
  -> Point                   -- ^ The wheel movement along x/y.
  -> WheelDirection          -- ^ Whether movement is normal or inverted.
  -> (Text, Int, Maybe Int)  -- ^ New text, cursor position and selection start.

{-|
Handler for drag events. Useful for values on which increase/decrease makes
sense.
-}
type InputDragHandler a
  = InputFieldState a        -- ^ The state of the input field
  -> Point                   -- ^ The mouse position.
  -> Point                   -- ^ The wheel movement along x/y.
  -> (Text, Int, Maybe Int)  -- ^ New text, cursor position and selection start.

{-|
Configuration options for an input field. These options are not directly exposed
to users; each derived widget should expose its own options.
-}
data InputFieldCfg s e a = InputFieldCfg {
  -- | Placeholder text to show when input is empty.
  _ifcPlaceholder :: Maybe Text,
  -- | Initial value for the input field, before retrieving from model.
  _ifcInitialValue :: a,
  -- | Where to get current data from.
  _ifcValue :: WidgetData s a,
  -- | Flag to indicate if the field is valid or not, using a lens.
  _ifcValid :: Maybe (WidgetData s Bool),
  -- | Flag to indicate if the field is valid or not, using an event handler.
  _ifcValidV :: [Bool -> e],
  -- | Whether to put cursor at the end of input on init. Defaults to False.
  _ifcDefCursorEnd :: Bool,
  -- | Default width of the input field.
  _ifcDefWidth :: Double,
  -- | Caret width.
  _ifcCaretWidth :: Maybe Double,
  -- | Caret blink period.
  _ifcCaretMs :: Maybe Int,
  -- | Character to display as text replacement. Useful for passwords.
  _ifcDisplayChar :: Maybe Char,
  -- | Whether input causes ResizeWidgets requests. Defaults to False.
  _ifcResizeOnChange :: Bool,
  -- | If all input should be selected when focus is received.
  _ifcSelectOnFocus :: Bool,
  -- | Conversion from text to the expected value. Failure returns Nothing.
  _ifcFromText :: Text -> Maybe a,
  -- | Conversion from a value to text. Cannot fail.
  _ifcToText :: a -> Text,
  {-|
  Whether to accept the current input status. The conversion fromText may still
  fail, but input still will be accepted. This is used, for instance, in date
  fields when input is not complete and a valid date cannot be created.
  -}
  _ifcAcceptInput :: Text -> Bool,
  {-|
  Whether the current text is valid input. Valid input means being able to
  convert to the expected type, and after that conversion the value matches the
  expected constraints (for instance, a well formed number between 1 and 100).
  -}
  _ifcIsValidInput :: Text -> Bool,
  -- | Base style retrieved from the active theme.
  _ifcStyle :: Maybe (ALens' ThemeState StyleState),
  -- | Handler for wheel events.
  _ifcWheelHandler :: Maybe (InputWheelHandler a),
  -- | Handler for drag events.
  _ifcDragHandler :: Maybe (InputDragHandler a),
  -- | Cursor to display on drag events.
  _ifcDragCursor :: Maybe CursorIcon,
  -- | WidgetRequest to generate when focus is received.
  _ifcOnFocusReq :: [Path -> WidgetRequest s e],
  -- | WidgetRequest to generate when focus is lost.
  _ifcOnBlurReq :: [Path -> WidgetRequest s e],
  -- | WidgetRequest to generate when value changes.
  _ifcOnChangeReq :: [a -> WidgetRequest s e]
}

data HistoryStep a = HistoryStep {
  _ihsValue :: a,
  _ihsText :: !Text,
  _ihsCursorPos :: !Int,
  _ihsSelStart :: Maybe Int,
  _ihsOffset :: !Double
} deriving (Eq, Show, Generic)

initialHistoryStep :: a -> HistoryStep a
initialHistoryStep value = HistoryStep {
  _ihsValue = value,
  _ihsText = "",
  _ihsCursorPos = 0,
  _ihsSelStart = Nothing,
  _ihsOffset = 0
}

-- | Current state of the input field. Provided to some event handlers.
data InputFieldState a = InputFieldState {
  {-|
  The placeholder text to show when input is empty. Does not depend on cursor
  position.
  -}
  _ifsPlaceholder :: Seq TextLine,
  -- | The latest valid value.
  _ifsCurrValue :: a,
  -- | The latest accepted input text.
  _ifsCurrText :: !Text,
  -- | The current cursor position.
  _ifsCursorPos :: !Int,
  -- | The selection start. Once selection begins, it doesn't change until done.
  _ifsSelStart :: Maybe Int,
  -- | The value when drag event started.
  _ifsDragSelValue :: a,
  -- | The glyphs of the current text.
  _ifsGlyphs :: Seq GlyphPos,
  -- | The offset of the current text, given cursor position and text length.
  _ifsOffset :: !Double,
  -- | The rect of the current text, given cursor position and text length.
  _ifsTextRect :: Rect,
  -- | Text metrics of the current font and size.
  _ifsTextMetrics :: TextMetrics,
  -- | Edit history of the current field. Supports undo and redo.
  _ifsHistory :: Seq (HistoryStep a),
  -- | Current index into history.
  _ifsHistIdx :: Int
} deriving (Eq, Show, Typeable, Generic)

initialState :: a -> InputFieldState a
initialState value = InputFieldState {
  _ifsPlaceholder = Seq.empty,
  _ifsCurrValue = value,
  _ifsCurrText = "",
  _ifsGlyphs = Seq.empty,
  _ifsCursorPos = 0,
  _ifsSelStart = Nothing,
  _ifsDragSelValue = value,
  _ifsOffset = 0,
  _ifsTextRect = def,
  _ifsTextMetrics = def,
  _ifsHistory = Seq.empty,
  _ifsHistIdx = 0
}

defCaretW :: Double
defCaretW = 2

defCaretMs :: Int
defCaretMs = 500

-- | Creates an instance of an input field, with customizations in config.
inputField_
  :: (InputFieldValue a, WidgetEvent e)
  => WidgetType
  -> InputFieldCfg s e a
  -> WidgetNode s e
inputField_ widgetType config = node where
  value = _ifcInitialValue config
  widget = makeInputField config (initialState value)
  node = defaultWidgetNode widgetType widget
    & L.info . L.focusable .~ True

makeInputField
  :: (InputFieldValue a, WidgetEvent e)
  => InputFieldCfg s e a
  -> InputFieldState a
  -> Widget s e
makeInputField config state = widget where
  widget = createSingle state def {
    singleFocusOnBtnPressed = False,
    singleUseCustomCursor = True,
    singleUseScissor = True,
    singleGetBaseStyle = getBaseStyle,
    singleInit = init,
    singleMerge = merge,
    singleDispose = dispose,
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleResize = resize,
    singleRender = render
  }

  -- Simpler access to state members
  currPlaceholder = _ifsPlaceholder state
  currVal = _ifsCurrValue state
  currText = _ifsCurrText state
  currGlyphs = _ifsGlyphs state
  currPos = _ifsCursorPos state
  currSel = _ifsSelStart state
  currOffset = _ifsOffset state
  currHistory = _ifsHistory state
  currHistIdx = _ifsHistIdx state
  -- Text/value conversion functions
  caretW = fromMaybe defCaretW (_ifcCaretWidth config)
  caretMs = fromMaybe defCaretMs (_ifcCaretMs config)
  fromText = _ifcFromText config
  toText = _ifcToText config
  getModelValue wenv = widgetDataGet (_weModel wenv) (_ifcValue config)
  -- Mouse select handling options
  wheelHandler = _ifcWheelHandler config
  dragHandler = _ifcDragHandler config
  dragCursor = _ifcDragCursor config

  getBaseStyle wenv node = _ifcStyle config >>= handler where
    handler lstyle = Just $ collectTheme wenv (cloneLens lstyle)

  init wenv node = result where
    newValue = getModelValue wenv
    txtValue = toText newValue
    txtPos
      | _ifcDefCursorEnd config = T.length txtValue
      | otherwise = 0
    newFieldState = newTextState wenv node state config
    newState = newFieldState newValue txtValue txtPos Nothing
    newNode = node
      & L.widget .~ makeInputField config newState
    parsedVal = fromText (toText newValue)
    reqs = setModelValid config (isJust parsedVal)
    result = resultReqs newNode reqs

  merge wenv node oldNode oldState = resultReqs newNode reqs where
    oldInfo = node ^. L.info
    oldValue = _ifsCurrValue oldState
    oldText = _ifsCurrText oldState
    oldPos = _ifsCursorPos oldState
    oldSel = _ifsSelStart oldState
    value = getModelValue wenv
    newText
      | oldValue /= getModelValue wenv = toText value
      | otherwise = oldText
    newTextL = T.length newText
    newPos
      | oldText == newText = oldPos
      | oldText /= "" && oldPos <= newTextL = oldPos
      | _ifcDefCursorEnd config = newTextL
      | otherwise = 0
    newSelStart
      | isNothing oldSel || newTextL < fromJust oldSel = Nothing
      | otherwise = oldSel
    newFieldState = newTextState wenv node oldState config
    newState = newFieldState value newText newPos newSelStart
    newNode = node
      & L.widget .~ makeInputField config newState
    parsedVal = fromText newText
    oldPath = oldInfo ^. L.path
    oldWid = oldInfo ^. L.widgetId
    newPath = node ^. L.info . L.path
    newWid = node ^. L.info . L.widgetId
    updateFocus = wenv ^. L.focusedPath == oldPath && oldPath /= newPath
    renderReqs
      | updateFocus = [RenderStop oldWid, RenderEvery newWid caretMs Nothing]
      | otherwise = []
    reqs = setModelValid config (isJust parsedVal) ++ renderReqs

  dispose wenv node = resultReqs node reqs where
    widgetId = node ^. L.info . L.widgetId
    reqs = [ RenderStop widgetId ]

  handleKeyPress wenv mod code
    | isDelBackWordNoSel = Just $ moveCursor removeWord prevWordStartIdx Nothing
    | isDelBackWord = Just $ moveCursor removeText minTpSel Nothing
    | isBackspace && emptySel = Just $ moveCursor removeText (tp - 1) Nothing
    | isBackspace = Just $ moveCursor removeText minTpSel Nothing
    | isMoveLeft = Just $ moveCursor txt (tp - 1) Nothing
    | isMoveRight = Just $ moveCursor txt (tp + 1) Nothing
    | isMoveWordL = Just $ moveCursor txt prevWordStartIdx Nothing
    | isMoveWordR = Just $ moveCursor txt nextWordEndIdx Nothing
    | isMoveLineL = Just $ moveCursor txt 0 Nothing
    | isMoveLineR = Just $ moveCursor txt txtLen Nothing
    | isSelectAll = Just $ moveCursor txt 0 (Just txtLen)
    | isSelectLeft = Just $ moveCursor txt (tp - 1) (Just tp)
    | isSelectRight = Just $ moveCursor txt (tp + 1) (Just tp)
    | isSelectWordL = Just $ moveCursor txt prevWordStartIdx (Just tp)
    | isSelectWordR = Just $ moveCursor txt nextWordEndIdx (Just tp)
    | isSelectLineL = Just $ moveCursor txt 0 (Just tp)
    | isSelectLineR = Just $ moveCursor txt txtLen (Just tp)
    | isDeselectLeft = Just $ moveCursor txt minTpSel Nothing
    | isDeselectRight = Just $ moveCursor txt maxTpSel Nothing
    | otherwise = Nothing
    where
      txt = currText
      txtLen = T.length txt
      tp = currPos
      emptySel = isNothing currSel
      (part1, part2) = T.splitAt currPos currText
      currSelVal = fromMaybe 0 currSel
      activeSel = isJust currSel
      minTpSel = min tp currSelVal
      maxTpSel = max tp currSelVal
      prevWordStart = T.dropWhileEnd (not . delim) $ T.dropWhileEnd delim part1
      prevWordStartIdx = T.length prevWordStart
      nextWordEnd = T.dropWhile (not . delim) $ T.dropWhile delim part2
      nextWordEndIdx = txtLen - T.length nextWordEnd
      isShift = _kmLeftShift mod
      isLeft = isKeyLeft code
      isRight = isKeyRight code
      isHome = isKeyHome code
      isEnd = isKeyEnd code
      isWordMod
        | isMacOS wenv = _kmLeftAlt mod
        | otherwise = _kmLeftCtrl mod
      isLineMod
        | isMacOS wenv = _kmLeftCtrl mod || _kmLeftGUI mod
        | otherwise = _kmLeftAlt mod
      isAllMod
        | isMacOS wenv = _kmLeftGUI mod
        | otherwise = _kmLeftCtrl mod
      isBackspace = isKeyBackspace code && (tp > 0 || isJust currSel)
      isDelBackWord = isBackspace && isWordMod
      isDelBackWordNoSel = isDelBackWord && emptySel
      isMove = not isShift && not isWordMod && not isLineMod
      isMoveWord = not isShift && isWordMod && not isLineMod
      isMoveLine = not isShift && isLineMod && not isWordMod
      isSelect = isShift && not isWordMod && not isLineMod
      isSelectWord = isShift && isWordMod && not isLineMod
      isSelectLine = isShift && isLineMod && not isWordMod
      isMoveLeft = isMove && not activeSel && isLeft
      isMoveRight = isMove && not activeSel && isRight
      isMoveWordL = isMoveWord && isLeft
      isMoveWordR = isMoveWord && isRight
      isMoveLineL = (isMoveLine && isLeft) || (not isShift && isHome)
      isMoveLineR = (isMoveLine && isRight) || (not isShift && isEnd)
      isSelectAll = isAllMod && isKeyA code
      isSelectLeft = isSelect && isLeft
      isSelectRight = isSelect && isRight
      isSelectWordL = isSelectWord && isLeft
      isSelectWordR = isSelectWord && isRight
      isSelectLineL = (isSelectLine && isLeft) || (isShift && isHome)
      isSelectLineR = (isSelectLine && isRight) || (isShift && isEnd)
      isDeselectLeft = isMove && activeSel && isLeft
      isDeselectRight = isMove && activeSel && isRight
      removeText
        | isJust currSel = replaceText txt ""
        | otherwise = T.init part1 <> part2
      removeWord
        | isJust currSel = replaceText txt ""
        | otherwise = prevWordStart <> part2
      moveCursor txt newPos newSel
        | isJust currSel && isNothing newSel = (txt, fixedPos, Nothing)
        | isJust currSel && Just fixedPos == currSel = (txt, fixedPos, Nothing)
        | isJust currSel = (txt, fixedPos, currSel)
        | Just fixedPos == fixedSel = (txt, fixedPos, Nothing)
        | otherwise = (txt, fixedPos, fixedSel)
        where
          fixedPos = fixIdx newPos
          fixedSel = fmap fixIdx newSel
      fixIdx idx
        | idx < 0 = 0
        | idx >= txtLen = txtLen
        | otherwise = idx

  handleEvent wenv node target evt = case evt of
    -- Begin regular text selection
    ButtonAction point btn BtnPressed clicks
      | dragSelectText btn && clicks == 1 -> Just result where
        style = activeStyle wenv node
        contentArea = getContentArea style node
        newPos = findClosestGlyphPos state point
        newState = newFieldState currVal currText newPos Nothing
        newNode = node
          & L.widget .~ makeInputField config newState
        newReqs = [ SetFocus widgetId | not (isNodeFocused wenv node) ]
        result = resultReqs newNode newReqs

    -- Begin custom drag
    ButtonAction point btn BtnPressed clicks
      | dragHandleExt btn && clicks == 1 -> Just (resultNode newNode) where
        newState = state { _ifsDragSelValue = currVal }
        newNode = node
          & L.widget .~ makeInputField config newState

    -- Select one word if clicked twice in a row
    ButtonAction point btn BtnReleased clicks
      | dragSelectText btn && clicks == 2 -> Just result where
        (part1, part2) = T.splitAt currPos currText
        txtLen = T.length currText
        wordStart = T.dropWhileEnd (not . delim) part1
        wordStartIdx = T.length wordStart
        wordEnd = T.dropWhile (not . delim) part2
        wordEndIdx = txtLen - T.length wordEnd
        newPos = wordStartIdx
        newSel = Just wordEndIdx
        newState = newFieldState currVal currText newPos newSel
        newNode = node
          & L.widget .~ makeInputField config newState
        result = resultReqs newNode [RenderOnce]

    -- Select all if clicked three times in a row
    ButtonAction point btn BtnReleased clicks
      | dragSelectText btn && clicks == 3 -> Just result where
        newPos = 0
        newSel = Just (T.length currText)
        newState = newFieldState currVal currText newPos newSel
        newNode = node
          & L.widget .~ makeInputField config newState
        result = resultReqs newNode [RenderOnce]

    -- If a custom drag handler is used, generate onChange events and history
    ButtonAction point btn BtnReleased clicks
      | dragHandleExt btn && clicks == 0 -> Just result where
        reqs = [RenderOnce]
        result = genInputResult wenv node True currText currPos currSel reqs

    -- Handle regular text selection
    Move point
      | isNodePressed wenv node && not shiftPressed -> Just result where
        style = activeStyle wenv node
        contentArea = getContentArea style node
        newPos = findClosestGlyphPos state point
        newSel = currSel <|> Just currPos
        newState = newFieldState currVal currText newPos newSel
        newNode = node
          & L.widget .~ makeInputField config newState
        result = resultReqs newNode (RenderOnce : changeCursorReq validCursor)

    -- Handle custom drag
    Move point
      | isNodePressed wenv node && shiftPressed -> Just result where
        (_, stPoint) = fromJust $ wenv ^. L.mainBtnPress
        handlerRes = fromJust dragHandler state stPoint point
        (newText, newPos, newSel) = handlerRes
        reqs = RenderOnce : changeCursorReq validCursor
        result = genInputResult wenv node True newText newPos newSel reqs

    -- Sets the correct cursor icon
    Move point -> Just (resultReqs node reqs) where
      reqs = changeCursorReq validCursor

    -- Handle wheel
    WheelScroll point move dir
      | isJust wheelHandler -> Just result where
        handlerRes = fromJust wheelHandler state point move dir
        (newText, newPos, newSel) = handlerRes
        reqs = [RenderOnce]
        result = genInputResult wenv node True newText newPos newSel reqs

    -- Handle keyboard shortcuts and possible cursor changes
    KeyAction mod code KeyPressed
      | isKeyboardCopy wenv evt
          -> Just $ resultReqs node [SetClipboard (ClipboardText selectedText)]
      | isKeyboardPaste wenv evt
          -> Just $ resultReqs node [GetClipboard widgetId]
      | isKeyboardCut wenv evt -> cutTextRes wenv node
      | isKeyboardUndo wenv evt -> moveHistory wenv node state config (-1)
      | isKeyboardRedo wenv evt -> moveHistory wenv node state config 1
      | otherwise -> fmap handleKeyRes keyRes <|> cursorRes where
          keyRes = handleKeyPress wenv mod code
          handleKeyRes (newText, newPos, newSel) = result where
            result = genInputResult wenv node False newText newPos newSel []
          cursorReq = changeCursorReq validCursor
          cursorRes
            | not (null cursorReq) = Just (resultReqs node cursorReq)
            | otherwise = Nothing

    -- Handle possible cursor reset
    KeyAction mod code KeyReleased
      | (pressed || hovered) && not (null reqs) -> result where
        pressed = isNodePressed wenv node
        hovered = isNodeHovered wenv node
        reqs = changeCursorReq validCursor
        result = Just (resultReqs node reqs)

    -- Text input has unicode already processed (it's not the same as KeyAction)
    TextInput newText -> result where
      result = insertTextRes wenv node newText

    -- Paste clipboard contents
    Clipboard (ClipboardText newText) -> result where
      result = insertTextRes wenv node newText

    -- Handle focus, maybe select all and disable custom drag handlers
    Focus prev -> Just result where
      newState
        | _ifcSelectOnFocus config && T.length currText > 0 = state {
            _ifsSelStart = Just 0,
            _ifsCursorPos = T.length currText
          }
        | otherwise = state
      reqs = [RenderEvery widgetId caretMs Nothing, StartTextInput viewport]
      newNode = node
        & L.widget .~ makeInputField config newState
      newResult = resultReqs newNode reqs
      focusRs = handleFocusChange (_ifcOnFocusReq config) prev newNode
      result = maybe newResult (newResult <>) focusRs

    -- Handle blur and disable custom drag handlers
    Blur next -> Just result where
      reqs = [RenderStop widgetId, StopTextInput]
      newResult = resultReqs node reqs
      blurResult = handleFocusChange (_ifcOnBlurReq config) next node
      result = maybe newResult (newResult <>) blurResult

    _ -> Nothing
    where
      widgetId = node ^. L.info . L.widgetId
      viewport = node ^. L.info . L.viewport
      newFieldState = newTextState wenv node state config
      shiftPressed = wenv ^. L.inputStatus . L.keyMod . L.leftShift
      dragSelectText btn
        = wenv ^. L.mainButton == btn
        && not shiftPressed
      dragHandleExt btn
        = wenv ^. L.mainButton == btn
        && shiftPressed
        && isJust dragHandler
      validCursor
        | not shiftPressed = CursorIBeam
        | otherwise = fromMaybe CursorArrow dragCursor
      changeCursorReq newCursor = reqs where
        cursorMatch = wenv ^? L.cursor . _Just . _2 == Just newCursor
        reqs
          | not cursorMatch = [SetCursorIcon widgetId newCursor]
          | otherwise = []

  insertTextRes wenv node addedText = Just result where
    addedLen = T.length addedText
    newText = replaceText currText addedText
    newPos
      | isJust currSel = addedLen + min currPos (fromJust currSel)
      | otherwise = addedLen + currPos
    result = genInputResult wenv node True newText newPos Nothing []

  cutTextRes wenv node = Just result where
    tmpResult = fromMaybe (resultNode node) (insertTextRes wenv node "")
    result = tmpResult
      & L.requests %~ (|> SetClipboard (ClipboardText selectedText))

  replaceText txt newTxt
    | isJust currSel = T.take start txt <> newTxt <> T.drop end txt
    | otherwise = T.take currPos txt <> newTxt <> T.drop currPos txt
    where
      start = min currPos (fromJust currSel)
      end = max currPos (fromJust currSel)

  selectedText
    | isJust currSel = T.take (end - start) $ T.drop start currText
    | otherwise = ""
    where
      start = min currPos (fromJust currSel)
      end = max currPos (fromJust currSel)

  genInputResult wenv node textAdd newText newPos newSel newReqs = result where
    acceptInput = _ifcAcceptInput config newText
    isValid = _ifcIsValidInput config newText
    newVal = fromText newText
    stVal
      | isValid = fromMaybe currVal newVal
      | otherwise = currVal
    tempState = newTextState wenv node state config stVal newText newPos newSel
    newOffset = _ifsOffset tempState
    history = _ifsHistory tempState
    histIdx = _ifsHistIdx tempState
    newStep = HistoryStep stVal newText newPos newSel newOffset
    newState
      | currText == newText = tempState
      | length history == histIdx = tempState {
          _ifsHistory = history |> newStep,
          _ifsHistIdx = histIdx + 1
        }
      | otherwise = tempState {
          _ifsHistory = Seq.take (histIdx - 1) history |> newStep,
          _ifsHistIdx = histIdx
        }
    newNode = node
      & L.widget .~ makeInputField config newState
    (reqs, events) = genReqsEvents config state newText newReqs
    result
      | acceptInput || not textAdd = resultReqsEvts newNode reqs events
      | otherwise = resultReqsEvts node reqs events

  getSizeReq wenv node = sizeReq where
    defWidth = _ifcDefWidth config
    resizeOnChange = _ifcResizeOnChange config
    currText
      | _ifsCurrText state /= "" = _ifsCurrText state
      | otherwise = fromMaybe "" (_ifcPlaceholder config)
    style = activeStyle wenv node
    Size w h = getTextSize wenv style currText
    targetW
      | resizeOnChange = max w 100
      | otherwise = defWidth
    factor = 1
    sizeReq = (expandSize targetW factor, fixedSize h)

  resize wenv node viewport = resultNode newNode where
    -- newTextState depends on having correct viewport in the node
    tempNode = node
      & L.info . L.viewport .~ viewport
    newFieldState = newTextState wenv tempNode state config
    newState = newFieldState currVal currText currPos currSel
    newNode = tempNode
      & L.widget .~ makeInputField config newState

  render wenv node renderer = do
    when (isJust currSel) $
      drawRect renderer selRect (Just selColor) Nothing

    when (currText == "" && not (null currPlaceholder)) $
      drawInTranslation renderer (Point cx cy) $
        forM_ currPlaceholder (drawTextLine renderer placeholderStyle)

    renderContent renderer state style (getDisplayText config currText)

    when caretRequired $
      drawRect renderer caretRect (Just caretColor) Nothing
    where
      style = activeStyle wenv node
      placeholderStyle = style
        & L.text . non def . L.fontColor .~ style ^. L.sndColor
      carea = getContentArea style node
      Rect cx cy _ _ = carea
      ts = _weTimestamp wenv
      selColor = styleHlColor style
      caretRequired = isNodeFocused wenv node && even (ts `div` caretMs)
      caretColor = styleFontColor style
      caretRect = getCaretRect config state style carea
      selRect = getSelRect state style

textOffsetY :: TextMetrics -> StyleState -> Double
textOffsetY (TextMetrics ta td tl tlx) style = offset where
  offset = case styleTextAlignV style of
    ATBaseline -> -td
    _ -> 0

renderContent :: Renderer -> InputFieldState a -> StyleState -> Text -> IO ()
renderContent renderer state style currText = do
  setFillColor renderer tsFontColor
  renderText renderer textPos tsFont tsFontSize tsFontSpcH currText
  where
    Rect tx ty tw th = _ifsTextRect state
    textMetrics = _ifsTextMetrics state
    textPos = Point tx (ty + th + textOffsetY textMetrics style)
    textStyle = fromMaybe def (_sstText style)
    tsFont = styleFont style
    tsFontSize = styleFontSize style
    tsFontSpcH = styleFontSpaceH style
    tsFontColor = styleFontColor style

getCaretH :: InputFieldState a -> Double
getCaretH state = ta - td * 2 where
  TextMetrics ta td _ _ = _ifsTextMetrics state

getCaretRect
  :: InputFieldCfg s e a
  -> InputFieldState a
  -> StyleState
  -> Rect
  -> Rect
getCaretRect config state style carea = caretRect where
  Rect cx cy cw ch = carea
  Rect tx ty tw th = _ifsTextRect state
  caretW = fromMaybe defCaretW (_ifcCaretWidth config)
  textMetrics = _ifsTextMetrics state
  glyphs = _ifsGlyphs state
  pos = _ifsCursorPos state
  caretPos
    | pos == 0 || null glyphs = 0
    | pos >= length glyphs = _glpXMax (seqLast glyphs)
    | otherwise = _glpXMin (Seq.index glyphs pos)
  caretX tx = max 0 $ min (cx + cw - caretW) (tx + caretPos)
  caretY = ty + textOffsetY textMetrics style
  caretRect = Rect (caretX tx) caretY caretW (getCaretH state)

getSelRect :: InputFieldState a -> StyleState -> Rect
getSelRect state style = selRect where
  Rect tx ty tw th = _ifsTextRect state
  textMetrics = _ifsTextMetrics state
  glyphs = _ifsGlyphs state
  pos = _ifsCursorPos state
  sel = _ifsSelStart state
  caretY = ty + textOffsetY textMetrics style
  caretH = getCaretH state
  glyph idx = Seq.index glyphs (min idx (length glyphs - 1))
  gx idx = _glpXMin (glyph idx)
  gw start end = abs $ _glpXMax (glyph end) - _glpXMin (glyph start)
  mkSelRect end
    | pos > end = Rect (tx + gx end) caretY (gw end (pos - 1)) caretH
    | otherwise = Rect (tx + gx pos) caretY (gw pos (end - 1)) caretH
  selRect = maybe def mkSelRect sel

findClosestGlyphPos :: InputFieldState a -> Point -> Int
findClosestGlyphPos state point = newPos where
  Point x y = point
  textRect = _ifsTextRect state
  localX = x - _rX textRect
  textLen = getGlyphsMax (_ifsGlyphs state)
  glyphs
    | Seq.null (_ifsGlyphs state) = Seq.empty
    | otherwise = _ifsGlyphs state |> GlyphPos ' ' textLen 0 0 0 0 0
  glyphStart i g = (i, abs (_glpXMin g - localX))
  pairs = Seq.mapWithIndex glyphStart glyphs
  cpm (_, g1) (_, g2) = compare g1 g2
  diffs = Seq.sortBy cpm pairs
  newPos = maybe 0 fst (Seq.lookup 0 diffs)

genReqsEvents
  :: (Eq a)
  => InputFieldCfg s e a
  -> InputFieldState a
  -> Text
  -> [WidgetRequest s e]
  -> ([WidgetRequest s e], [e])
genReqsEvents config state newText newReqs = result where
  resizeOnChange = _ifcResizeOnChange config
  fromText = _ifcFromText config
  setModelValue = widgetDataSet (_ifcValue config)
  currVal = _ifsCurrValue state
  currText = _ifsCurrText state
  accepted = _ifcAcceptInput config newText
  isValid = _ifcIsValidInput config newText
  newVal = fromText newText
  stateVal = fromMaybe currVal newVal
  txtChanged = newText /= currText
  valChanged = stateVal /= currVal
  evtValid
    | txtChanged = fmap ($ isValid) (_ifcValidV config)
    | otherwise = []
  reqValid = setModelValid config isValid
  reqUpdateModel
    | accepted && valChanged = setModelValue stateVal
    | otherwise = []
  reqResize
    | resizeOnChange && valChanged = [ResizeWidgets]
    | otherwise = []
  reqOnChange
    | accepted && valChanged = fmap ($ stateVal) (_ifcOnChangeReq config)
    | otherwise = []
  reqs = newReqs ++ reqUpdateModel ++ reqValid ++ reqResize ++ reqOnChange
  result = (reqs, evtValid)

moveHistory
  :: (InputFieldValue a, WidgetEvent e)
  => WidgetEnv s e
  -> WidgetNode s e
  -> InputFieldState a
  -> InputFieldCfg s e a
  -> Int
  -> Maybe (WidgetResult s e)
moveHistory wenv node state config steps = result where
  historyStep = initialHistoryStep (_ifcInitialValue config)
  currHistory = _ifsHistory state
  currHistIdx = _ifsHistIdx state
  lenHistory = length currHistory
  reqHistIdx
    | steps == -1 && currHistIdx == lenHistory = currHistIdx - 2
    | otherwise = currHistIdx + steps
  histStep = Seq.lookup reqHistIdx currHistory
  result
    | null currHistory || reqHistIdx < 0 = Just (createResult historyStep)
    | otherwise = fmap createResult histStep
  createResult histStep = resultReqsEvts newNode reqs evts where
    (reqs, evts) = genReqsEvents config state (_ihsText histStep) []
    tempState = newStateFromHistory wenv node state config histStep
    newState = tempState {
      _ifsHistIdx = clamp 0 lenHistory reqHistIdx
    }
    newNode = node & L.widget .~ makeInputField config newState

newStateFromHistory
  :: WidgetEnv s e
  -> WidgetNode s e
  -> InputFieldState a
  -> InputFieldCfg s e a
  -> HistoryStep a
  -> InputFieldState a
newStateFromHistory wenv node oldState config inputHist = newState where
  HistoryStep hValue hText hPos hSel hOffset = inputHist
  tempState = oldState { _ifsOffset = hOffset }
  newState = newTextState wenv node oldState config hValue hText hPos hSel

newTextState
  :: WidgetEnv s e
  -> WidgetNode s e
  -> InputFieldState a
  -> InputFieldCfg s e a
  -> a
  -> Text
  -> Int
  -> Maybe Int
  -> InputFieldState a
newTextState wenv node oldState config value text cursor sel = newState where
  style = activeStyle wenv node
  contentArea = getContentArea style node
  caretW = fromMaybe defCaretW (_ifcCaretWidth config)
  Rect cx cy cw ch = contentArea
  alignH = inputFieldAlignH style
  alignV = inputFieldAlignV style
  alignL = alignH == ATLeft
  alignR = alignH == ATRight
  alignC = alignH == ATCenter
  cursorL = cursor == 0
  cursorR = cursor == T.length text
  !textMetrics = getTextMetrics wenv style
  !textRect = getTextRect wenv style contentArea alignH alignV text
  Rect tx ty tw th = textRect
  textFits = cw >= tw
  glyphs = getTextGlyphs wenv style (getDisplayText config text)
  glyphStart = maybe 0 _glpXMax $ Seq.lookup (cursor - 1) glyphs
  glyphOffset = getGlyphsMin glyphs
  glyphX = glyphStart - glyphOffset
  curX = tx + glyphX
  oldOffset = _ifsOffset oldState
  newOffset
    | round cw == 0 = 0
    | textFits && alignR = -caretW
    | textFits = 0
    | alignL && cursorL = cx - tx + caretW
    | alignL && curX + oldOffset > cx + cw = cx + cw - curX
    | alignL && curX + oldOffset < cx = cx - curX
    | alignR && cursorR = -caretW
    | alignR && curX + oldOffset > cx + cw = tw - glyphX
    | alignR && curX + oldOffset < cx = tw - cw - glyphX
    | alignC && curX + oldOffset > cx + cw = cx + cw - curX
    | alignC && curX + oldOffset < cx = cx - curX
    | otherwise = oldOffset
  justSel = fromJust sel
  newSel
    | Just cursor == sel = Nothing
    | isJust sel && (justSel < 0 || justSel > T.length text) = Nothing
    | otherwise = sel
  tmpState = updatePlaceholder wenv node oldState config
  newState = tmpState {
    _ifsCurrValue = value,
    _ifsCurrText = text,
    _ifsCursorPos = cursor,
    _ifsSelStart = newSel,
    _ifsGlyphs = glyphs,
    _ifsOffset = newOffset,
    _ifsTextRect = textRect & L.x .~ tx + newOffset,
    _ifsTextMetrics = textMetrics
  }

updatePlaceholder
  :: WidgetEnv s e
  -> WidgetNode s e
  -> InputFieldState a
  -> InputFieldCfg s e a
  -> InputFieldState a
updatePlaceholder wenv node state config = newState where
  fontMgr = wenv ^. L.fontManager
  style = activeStyle wenv node
  Rect cx cy cw ch = getContentArea style node
  carea = Rect 0 0 cw ch
  size = Size cw ch
  -- Placeholder style
  pstyle = style
    & L.text . non def . L.alignH ?~ inputFieldAlignH style
    & L.text . non def . L.alignV ?~ inputFieldAlignV style
  text = _ifcPlaceholder config
  fitText = fitTextToSize fontMgr pstyle Ellipsis MultiLine KeepSpaces Nothing
  lines
    | isJust text = fitText size (fromJust text)
    | otherwise = Seq.empty
  newState = state {
    _ifsPlaceholder = alignTextLines pstyle carea lines
  }

setModelValid :: InputFieldCfg s e a -> Bool -> [WidgetRequest s e]
setModelValid config
  | isJust (_ifcValid config) = widgetDataSet (fromJust $ _ifcValid config)
  | otherwise = const []

inputFieldAlignH :: StyleState -> AlignTH
inputFieldAlignH style = fromMaybe ATLeft alignH where
  alignH = style ^? L.text . _Just . L.alignH . _Just

inputFieldAlignV :: StyleState -> AlignTV
inputFieldAlignV style = fromMaybe ATLowerX alignV where
  alignV = style ^? L.text . _Just . L.alignV . _Just

getDisplayText :: InputFieldCfg s e a -> Text -> Text
getDisplayText config text = displayText where
  displayChar = T.singleton <$> _ifcDisplayChar config
  displayText
    | isJust displayChar = T.replicate (T.length text) (fromJust displayChar)
    | otherwise = text

delim :: Char -> Bool
delim c = c `elem` [' ', '.', ',', '/', '-', ':']
