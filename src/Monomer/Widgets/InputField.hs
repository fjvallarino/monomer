{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Monomer.Widgets.InputField (
  InputFieldCfg(..),
  InputFieldState(..),
  inputField_,
  makeInputField
) where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Lens (ALens', (&), (.~), (%~), (^.), (^?), _Just, cloneLens, non)
import Data.Default
import Data.Maybe
import Data.Sequence (Seq(..), (|>))
import Data.Text (Text)
import Data.Typeable

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Widgets.Single

import qualified Monomer.Lens as L

type InputFieldValue a = (Eq a, Show a, Default a, Typeable a)

type InputDragHandler a
  = InputFieldState a
  -> Point
  -> Point
  -> (Text, Int, Maybe Int)

data InputFieldCfg s e a = InputFieldCfg {
  _ifcValue :: WidgetData s a,
  _ifcValid :: Maybe (WidgetData s Bool),
  _ifcSelectOnFocus :: Bool,
  _ifcSelectDragOnlyFocused :: Bool,
  _ifcFromText :: Text -> Maybe a,
  _ifcToText :: a -> Text,
  _ifcAcceptInput :: Text -> Bool,
  _ifcStyle :: Maybe (ALens' ThemeState StyleState),
  _ifcDragHandler :: Maybe (InputDragHandler a),
  _ifcDragCursor :: Maybe CursorIcon,
  _ifcOnFocus :: [e],
  _ifcOnFocusReq :: [WidgetRequest s],
  _ifcOnBlur :: [e],
  _ifcOnBlurReq :: [WidgetRequest s],
  _ifcOnChange :: [a -> e],
  _ifcOnChangeReq :: [WidgetRequest s]
}

data HistoryStep a = HistoryStep {
  _ihsValue :: a,
  _ihsText :: !Text,
  _ihsCursorPos :: !Int,
  _ihsSelStart :: Maybe Int,
  _ihsOffset :: !Double
} deriving (Eq, Show, Typeable)

instance Default a => Default (HistoryStep a) where
  def = HistoryStep {
  _ihsValue = def,
  _ihsText = "",
  _ihsCursorPos = 0,
  _ihsSelStart = Nothing,
  _ihsOffset = 0
}

data InputFieldState a = InputFieldState {
  _ifsCurrValue :: a,
  _ifsCurrText :: !Text,
  _ifsCursorPos :: !Int,
  _ifsSelStart :: Maybe Int,
  _ifsDragSelActive :: Bool,
  _ifsDragSelValue :: a,
  _ifsGlyphs :: Seq GlyphPos,
  _ifsOffset :: !Double,
  _ifsTextRect :: Rect,
  _ifsTextMetrics :: TextMetrics,
  _ifsHistory :: Seq (HistoryStep a),
  _ifsHistIdx :: Int
} deriving (Eq, Show, Typeable)

instance Default a => Default (InputFieldState a) where
  def = InputFieldState {
    _ifsCurrValue = def,
    _ifsCurrText = "",
    _ifsGlyphs = Seq.empty,
    _ifsCursorPos = 0,
    _ifsSelStart = Nothing,
    _ifsDragSelActive = False,
    _ifsDragSelValue = def,
    _ifsOffset = 0,
    _ifsTextRect = def,
    _ifsTextMetrics = def,
    _ifsHistory = Seq.empty,
    _ifsHistIdx = 0
  }

caretWidth :: Double
caretWidth = 2

caretMs :: Int
caretMs = 500

inputField_
  :: InputFieldValue a => WidgetType -> InputFieldCfg s e a -> WidgetNode s e
inputField_ widgetType config = node where
  widget = makeInputField config def
  node = defaultWidgetNode widgetType widget
    & L.info . L.focusable .~ True

makeInputField
  :: InputFieldValue a => InputFieldCfg s e a -> InputFieldState a -> Widget s e
makeInputField config state = widget where
  widget = createSingle def {
    singleStyleChangeCfg = def & L.cursorIgnore .~ True,
    singleFocusOnPressedBtn = False,
    singleGetBaseStyle = getBaseStyle,
    singleInit = init,
    singleGetState = makeState state,
    singleMerge = merge,
    singleDispose = dispose,
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleResize = resize,
    singleRender = render
  }

  -- Simpler access to state members
  currVal = _ifsCurrValue state
  currText = _ifsCurrText state
  currGlyphs = _ifsGlyphs state
  currPos = _ifsCursorPos state
  currSel = _ifsSelStart state
  currOffset = _ifsOffset state
  currHistory = _ifsHistory state
  currHistIdx = _ifsHistIdx state
  -- Text/value conversion functions
  fromText = _ifcFromText config
  toText = _ifcToText config
  getModelValue wenv = widgetDataGet (_weModel wenv) (_ifcValue config)
  -- Mouse select handling options
  selectDragOnlyFocused = _ifcSelectDragOnlyFocused config
  dragHandler = _ifcDragHandler config
  dragCursor = _ifcDragCursor config
  dragSelActive
    = _ifsDragSelActive state
    || not selectDragOnlyFocused
    || isNothing dragHandler

  getBaseStyle wenv node = _ifcStyle config >>= handler where
    handler lstyle = Just $ collectTheme wenv (cloneLens lstyle)

  init wenv node = result where
    newValue = getModelValue wenv
    newState = newTextState wenv node state newValue (toText newValue) 0 Nothing
    newNode = node
      & L.widget .~ makeInputField config newState
    parsedVal = fromText (toText newValue)
    reqs = setModelValid config (isJust parsedVal)
    result = resultReqs newNode reqs

  merge wenv oldState oldNode node = resultReqs newNode reqs where
    currState = fromMaybe state (useState oldState)
    oldValue = _ifsCurrValue currState
    oldText = _ifsCurrText currState
    oldPos = _ifsCursorPos currState
    oldSel = _ifsSelStart currState
    value = getModelValue wenv
    newText
      | oldValue /= getModelValue wenv = toText value
      | otherwise = oldText
    newTextL = T.length newText
    newPos
      | newTextL < oldPos = newTextL
      | otherwise = oldPos
    newSelStart
      | isNothing oldSel || newTextL < fromJust oldSel = Nothing
      | otherwise = oldSel
    newState = newTextState wenv node currState value newText newPos newSelStart
    newNode = node
      & L.widget .~ makeInputField config newState
    parsedVal = fromText newText
    oldPath = oldNode ^. L.info . L.path
    newPath = node ^. L.info . L.path
    updateFocus = isFocused wenv oldNode && oldPath /= newPath
    renderReqs
      | updateFocus = [ RenderStop oldPath, RenderEvery newPath caretMs ]
      | otherwise = []
    reqs = setModelValid config (isJust parsedVal) ++ renderReqs

  dispose wenv node = resultReqs node reqs where
    path = node ^. L.info . L.path
    reqs = [ RenderStop path ]

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

  handleEvent wenv target evt node = case evt of
    Enter point -> Just (resultReqs node reqs) where
      cursorIcon
        | dragSelActive = CursorIBeam
        | otherwise = fromMaybe CursorArrow dragCursor
      reqs = [SetCursorIcon cursorIcon]

    -- Enter regular edit mode if widget has custom drag handler
    DblClick point btn
      | dragHandleExt btn -> Just (resultReqs node reqs) where
        focusReq = [SetFocus path | not (isFocused wenv node)]
        reqs = SetCursorIcon CursorIBeam : focusReq

    -- Begin regular text selection
    ButtonAction point btn PressedBtn clicks
      | dragSelectText btn && clicks == 1 -> Just result where
        style = activeStyle wenv node
        contentArea = getContentArea style node
        newPos = findClosestGlyphPos state point
        newState = newTextState wenv node state currVal currText newPos Nothing
        newNode = node
          & L.widget .~ makeInputField config newState
        newReqs = [ SetFocus path | not (isFocused wenv node) ]
        result = resultReqs newNode newReqs

    -- Begin custom drag
    ButtonAction point btn PressedBtn clicks
      | dragHandleExt btn && clicks == 1 -> Just (resultWidget newNode) where
        newState = state { _ifsDragSelValue = currVal }
        newNode = node
          & L.widget .~ makeInputField config newState

    -- Select one word if clicked twice in a row
    ButtonAction point btn ReleasedBtn clicks
      | dragSelectText btn && clicks == 2 -> Just result where
        (part1, part2) = T.splitAt currPos currText
        txtLen = T.length currText
        wordStart = T.dropWhileEnd (not . delim) part1
        wordStartIdx = T.length wordStart
        wordEnd = T.dropWhile (not . delim) part2
        wordEndIdx = txtLen - T.length wordEnd
        newPos = wordStartIdx
        newSel = Just wordEndIdx
        newState = newTextState wenv node state currVal currText newPos newSel
        newNode = node
          & L.widget .~ makeInputField config newState
        result = resultReqs newNode [RenderOnce]

    -- Select all if clicked three times in a row
    ButtonAction point btn ReleasedBtn clicks
      | dragSelectText btn && clicks == 3 -> Just result where
        newPos = 0
        newSel = Just (T.length currText)
        newState = newTextState wenv node state currVal currText newPos newSel
        newNode = node
          & L.widget .~ makeInputField config newState
        result = resultReqs newNode [RenderOnce]

    -- If a custom drag handler is used, generate onChange events and history
    ButtonAction point btn ReleasedBtn clicks
      | dragHandleExt btn && clicks == 1 -> Just result where
        reqs = [RenderOnce]
        result = genInputResult wenv node True currText currPos currSel reqs

    -- Handle regular text selection
    Move point
      | isPressed wenv node && dragSelActive -> Just result where
        style = activeStyle wenv node
        contentArea = getContentArea style node
        newPos = findClosestGlyphPos state point
        newSel = currSel <|> Just currPos
        newState = newTextState wenv node state currVal currText newPos newSel
        newNode = node
          & L.widget .~ makeInputField config newState
        result = resultReqs newNode [RenderOnce]

    -- Handle custom drag
    Move point
      | isPressed wenv node && not dragSelActive -> Just result where
        (_, stPoint) = fromJust $ wenv ^. L.mainBtnPress
        handlerRes = fromJust dragHandler state stPoint point
        (newText, newPos, newSel) = handlerRes
        newState = newTextState wenv node state currVal newText newPos newSel
        newNode = node
          & L.widget .~ makeInputField config newState
        result = resultReqs newNode [RenderOnce]

    KeyAction mod code KeyPressed
      | isKeyboardCopy wenv evt
          -> Just $ resultReqs node [SetClipboard (ClipboardText selectedText)]
      | isKeyboardPaste wenv evt
          -> Just $ resultReqs node [GetClipboard path]
      | isKeyboardCut wenv evt -> cutTextRes wenv node
      | isKeyboardUndo wenv evt -> moveHistory wenv node state config (-1)
      | isKeyboardRedo wenv evt -> moveHistory wenv node state config 1
      | otherwise -> fmap handleKeyRes keyRes where
          keyRes = handleKeyPress wenv mod code
          handleKeyRes (newText, newPos, newSel) = result where
            result = genInputResult wenv node False newText newPos newSel []

    -- Text input has unicode already processed (it's not the same as KeyAction)
    TextInput newText -> result where
      result = insertTextRes wenv node newText

    -- Paste clipboard contents
    Clipboard (ClipboardText newText) -> result where
      result = insertTextRes wenv node newText

    -- Handle focus, maybe select all and disable custom drag handlers
    Focus -> Just result where
      tmpState
        | _ifcSelectOnFocus config && T.length currText > 0 = state {
            _ifsSelStart = Just 0,
            _ifsCursorPos = T.length currText
          }
        | otherwise = state
      newState = tmpState { _ifsDragSelActive = True }
      newNode = node
        & L.widget .~ makeInputField config newState
      reqs = [RenderEvery path caretMs, StartTextInput viewport]
      newResult = resultReqs newNode reqs
      focusResult = handleFocusChange _ifcOnFocus _ifcOnFocusReq config newNode
      result = maybe newResult (newResult <>) focusResult

    -- Handle blur and disable custom drag handlers
    Blur -> Just result where
      newState = state { _ifsDragSelActive = False }
      newNode = node & L.widget .~ makeInputField config newState
      reqs = [RenderStop path, StopTextInput]
      newResult = resultReqs newNode reqs
      blurResult = handleFocusChange _ifcOnBlur _ifcOnBlurReq config newNode
      result = maybe newResult (newResult <>) blurResult

    _ -> Nothing
    where
      path = node ^. L.info . L.path
      viewport = node ^. L.info . L.viewport
      focused = isFocused wenv node
      dragSelectText btn
        = wenv ^. L.mainButton == btn
        && dragSelActive
      dragHandleExt btn
        = wenv ^. L.mainButton == btn
        && not dragSelActive
        && not focused

  insertTextRes wenv node addedText = Just result where
    addedLen = T.length addedText
    newText = replaceText currText addedText
    newPos
      | isJust currSel = addedLen + min currPos (fromJust currSel)
      | otherwise = addedLen + currPos
    result = genInputResult wenv node True newText newPos Nothing []

  cutTextRes wenv node = Just result where
    tmpResult = fromMaybe (resultWidget node) (insertTextRes wenv node "")
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
    isValid = _ifcAcceptInput config newText
    newVal = fromText newText
    stateVal = fromMaybe currVal newVal
    tempState = newTextState wenv node state stateVal newText newPos newSel
    newOffset = _ifsOffset tempState
    history = _ifsHistory tempState
    histIdx = _ifsHistIdx tempState
    newStep = HistoryStep stateVal newText newPos newSel newOffset
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
      | isValid || not textAdd = resultReqsEvts newNode reqs events
      | otherwise = resultReqsEvts node reqs events

  getSizeReq wenv node = sizeReq where
    style = activeStyle wenv node
    Size w h = getTextSize wenv style currText
    targetW = max w 100
    factor = 1
    sizeReq = (FlexSize targetW factor, FixedSize h)

  resize wenv viewport renderArea node = newNode where
    -- newTextState depends on having correct viewport/renderArea in the node
    tempNode = node
      & L.info . L.viewport .~ viewport
      & L.info . L.renderArea .~ renderArea
    newState = newTextState wenv tempNode state currVal currText currPos currSel
    newNode = tempNode
      & L.widget .~ makeInputField config newState

  render renderer wenv node = do
    when (selRequired && isJust currSel) $
      drawRect renderer selRect (Just selColor) Nothing

    renderContent renderer state style currText

    when caretRequired $
      drawRect renderer caretRect (Just caretColor) Nothing
    where
      style = activeStyle wenv node
      contentArea = getContentArea style node
      Rect cx cy cw ch = contentArea
      textRect = _ifsTextRect state
      textMetrics = _ifsTextMetrics state
      Rect tx ty tw th = textRect
      TextMetrics ta td tl = textMetrics
      selRect = maybe def mkSelRect currSel
      mkSelRect end
        | currPos > end = Rect (tx + gx end) (ty - td) (gw end (currPos - 1)) th
        | otherwise = Rect (tx + gx currPos) (ty - td) (gw currPos (end - 1)) th
      gx idx = _glpXMin (glyph idx)
      gw start end = abs $ _glpXMax (glyph end) - _glpXMin (glyph start)
      nglyphs = Seq.length currGlyphs
      glyph idx = Seq.index currGlyphs (min idx (nglyphs - 1))
      ts = _weTimestamp wenv
      selRequired = isFocused wenv node
      selColor = styleHlColor style
      caretRequired = isFocused wenv node && ts `mod` 1000 < 500
      caretColor = styleFontColor style
      caretPos
        | currPos == 0 = 0
        | currPos == nglyphs = _glpXMax (glyph $ currPos - 1)
        | otherwise = _glpXMin (glyph currPos)
      caretX tx = max 0 $ min (cx + cw - caretWidth) (tx + caretPos)
      caretRect = Rect (caretX tx) (ty - td) caretWidth th

renderContent
  :: (Eq a, Default a, Typeable a)
  => Renderer -> InputFieldState a -> StyleState -> Text -> IO ()
renderContent renderer state style currText = do
  setFillColor renderer tsFontColor
  renderText renderer textPos tsFont tsFontSize currText
  where
    Rect tx ty tw th = _ifsTextRect state
    TextMetrics ta td tl = _ifsTextMetrics state
    textPos = Point tx (ty + th)
    textStyle = fromMaybe def (_sstText style)
    tsFont = styleFont style
    tsFontSize = styleFontSize style
    tsFontColor = styleFontColor style

delim :: Char -> Bool
delim c = c == ' ' || c == '.' || c == ','

setModelValid :: InputFieldCfg s e a -> Bool -> [WidgetRequest s]
setModelValid config
  | isJust (_ifcValid config) = widgetDataSet (fromJust $ _ifcValid config)
  | otherwise = const []

genReqsEvents
  :: (Eq a)
  => InputFieldCfg s e a
  -> InputFieldState a
  -> Text
  -> [WidgetRequest s]
  -> ([WidgetRequest s], [e])
genReqsEvents config state newText newReqs = result where
  fromText = _ifcFromText config
  setModelValue = widgetDataSet (_ifcValue config)
  currVal = _ifsCurrValue state
  currText = _ifsCurrText state
  isValid = _ifcAcceptInput config newText
  hasChanged = currText /= newText
  newVal = fromText newText
  stateVal = fromMaybe currVal newVal
  events
    | isValid && stateVal /= currVal = fmap ($ stateVal) (_ifcOnChange config)
    | otherwise = []
  reqValid
    | isValid = setModelValid config (isJust newVal)
    | otherwise = []
  reqUpdateModel
    | isValid && hasChanged && isJust newVal = setModelValue (fromJust newVal)
    | otherwise = []
  reqOnChange
    | stateVal /= currVal = _ifcOnChangeReq config
    | otherwise = []
  reqs = newReqs ++ reqValid ++ reqUpdateModel ++ reqOnChange
  result = (reqs, events)

moveHistory
  :: InputFieldValue a
  => WidgetEnv s e
  -> WidgetNode s e
  -> InputFieldState a
  -> InputFieldCfg s e a
  -> Int
  -> Maybe (WidgetResult s e)
moveHistory wenv node state config steps = result where
  currHistory = _ifsHistory state
  currHistIdx = _ifsHistIdx state
  lenHistory = length currHistory
  reqHistIdx
    | steps == -1 && currHistIdx == lenHistory = currHistIdx - 2
    | otherwise = currHistIdx + steps
  histStep = Seq.lookup reqHistIdx currHistory
  result
    | null currHistory || reqHistIdx < 0 = Just (createResult def)
    | otherwise = fmap createResult histStep
  createResult histStep = resultReqsEvts newNode reqs evts where
    (reqs, evts) = genReqsEvents config state (_ihsText histStep) []
    tempState = newStateFromHistory wenv node state histStep
    newState = tempState {
      _ifsHistIdx = clamp 0 lenHistory reqHistIdx
    }
    newNode = node & L.widget .~ makeInputField config newState

findClosestGlyphPos :: InputFieldState a -> Point -> Int
findClosestGlyphPos state point = newPos where
  Point x y = point
  textRect = _ifsTextRect state
  localX = x - _rX textRect
  textLen = getGlyphsMax (_ifsGlyphs state)
  glyphs
    | Seq.null (_ifsGlyphs state) = Seq.empty
    | otherwise = _ifsGlyphs state |> GlyphPos ' ' textLen 0 0
  glyphStart i g = (i, abs (_glpXMin g - localX))
  pairs = Seq.mapWithIndex glyphStart glyphs
  cpm (_, g1) (_, g2) = compare g1 g2
  diffs = Seq.sortBy cpm pairs
  newPos = maybe 0 fst (Seq.lookup 0 diffs)

newStateFromHistory
  :: (Eq a, Default a)
  => WidgetEnv s e
  -> WidgetNode s e
  -> InputFieldState a
  -> HistoryStep a
  -> InputFieldState a
newStateFromHistory wenv node oldState inputHist = newState where
  HistoryStep hValue hText hPos hSel hOffset = inputHist
  tempState = oldState { _ifsOffset = hOffset }
  newState = newTextState wenv node oldState hValue hText hPos hSel

newTextState
  :: (Eq a, Default a)
  => WidgetEnv s e
  -> WidgetNode s e
  -> InputFieldState a
  -> a
  -> Text
  -> Int
  -> Maybe Int
  -> InputFieldState a
newTextState wenv node oldState value text cursor selection = newState where
  style = activeStyle wenv node
  contentArea = getContentArea style node
  !(Rect cx cy cw ch) = contentArea
  textStyle = fromMaybe def (_sstText style)
  alignH = fromMaybe ALeft (_txsAlignH textStyle)
  alignV = fromMaybe def (_txsAlignV textStyle)
  align = Align alignH alignV
  alignL = alignH == ALeft
  alignR = alignH == ARight
  alignC = alignH == ACenter
  cursorL = cursor == 0
  cursorR = cursor == T.length text
  !textMetrics = getTextMetrics wenv style
  !textRect = getTextRect wenv style contentArea align text
  Rect _ _ tw th = textRect
  textFits = cw >= tw
  TextMetrics ta ts tl = textMetrics
  Rect tx ty _ _ = textRect
  glyphs = getTextGlyphs wenv style text
  glyphStart = maybe 0 _glpXMax $ Seq.lookup (cursor - 1) glyphs
  glyphOffset = getGlyphsMin glyphs
  glyphX = glyphStart - glyphOffset
  curX = tx + glyphX
  oldOffset = _ifsOffset oldState
  newOffset
    | round cw == 0 = 0
    | textFits && alignR = -caretWidth
    | textFits = 0
    | alignL && cursorL = cx - tx + caretWidth
    | alignL && curX + oldOffset > cx + cw = cx + cw - curX
    | alignL && curX + oldOffset < cx = cx - curX
    | alignR && cursorR = -caretWidth
    | alignR && curX + oldOffset > cx + cw = tw - glyphX
    | alignR && curX + oldOffset < cx = tw - cw - glyphX
    | alignC && curX + oldOffset > cx + cw = cx + cw - curX
    | alignC && curX + oldOffset < cx = cx - curX
    | otherwise = oldOffset
  justSel = fromJust selection
  newSel
    | Just cursor == selection = Nothing
    | isJust selection && (justSel < 0 || justSel > T.length text) = Nothing
    | otherwise = selection
  newState = oldState {
    _ifsCurrValue = value,
    _ifsCurrText = text,
    _ifsCursorPos = cursor,
    _ifsSelStart = newSel,
    _ifsGlyphs = glyphs,
    _ifsOffset = newOffset,
    _ifsTextRect = textRect & L.x .~ tx + newOffset,
    _ifsTextMetrics = textMetrics
  }
