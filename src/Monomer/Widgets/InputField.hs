{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Monomer.Widgets.InputField (
  InputFieldCfg(..),
  inputField_,
  makeInputField
) where

import Control.Monad
import Control.Lens (ALens', (&), (.~), (^.), (^?), _Just, cloneLens, non)
import Data.Default
import Data.Maybe
import Data.Sequence (Seq(..), (|>))
import Data.Text (Text)
import Data.Typeable

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Widgets.Single

import qualified Monomer.Lens as L

data InputFieldCfg s e a = InputFieldCfg {
  _ifcValue :: WidgetData s a,
  _ifcValid :: Maybe (WidgetData s Bool),
  _ifcSelectOnFocus :: Bool,
  _ifcFromText :: Text -> Maybe a,
  _ifcToText :: a -> Text,
  _ifcAcceptInput :: Text -> Bool,
  _ifcStyle :: Maybe (ALens' ThemeState StyleState),
  _ifcOnChange :: [a -> e],
  _ifcOnChangeReq :: [WidgetRequest s]
}

data InputFieldState a = InputFieldState {
  _ifsCurrValue :: a,
  _ifsCurrText :: !Text,
  _ifsGlyphs :: Seq GlyphPos,
  _ifsCursorPos :: !Int,
  _ifsSelStart :: Maybe Int,
  _ifsOffset :: !Double,
  _ifsTextRect :: Rect,
  _ifsTextMetrics :: TextMetrics
} deriving (Eq, Show, Typeable)

inputFieldState :: Default a => InputFieldState a
inputFieldState = InputFieldState {
  _ifsCurrValue = def,
  _ifsCurrText = "",
  _ifsGlyphs = Seq.empty,
  _ifsCursorPos = 0,
  _ifsSelStart = Nothing,
  _ifsOffset = 0,
  _ifsTextRect = def,
  _ifsTextMetrics = def
}

caretWidth :: Double
caretWidth = 1

inputField_
  :: (Eq a, Default a, Typeable a)
  => WidgetType
  -> InputFieldCfg s e a
  -> WidgetInstance s e
inputField_ widgetType config = inst where
  widget = makeInputField config inputFieldState
  inst = (defaultWidgetInstance widgetType widget) {
    _wiFocusable = True
  }

makeInputField
  :: (Eq a, Default a, Typeable a)
  => InputFieldCfg s e a -> InputFieldState a -> Widget s e
makeInputField config state = widget where
  widget = createSingle def {
    singleGetBaseStyle = getBaseStyle,
    singleInit = init,
    singleGetState = makeState state,
    singleMerge = merge,
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleResize = resize,
    singleRender = render
  }

  currVal = _ifsCurrValue state
  currText = _ifsCurrText state
  currGlyphs = _ifsGlyphs state
  currPos = _ifsCursorPos state
  currSel = _ifsSelStart state

  fromText = _ifcFromText config
  toText = _ifcToText config
  getModelValue wenv = widgetDataGet (_weModel wenv) (_ifcValue config)
  setModelValue = widgetDataSet (_ifcValue config)
  setModelValid
    | isJust (_ifcValid config) = widgetDataSet (fromJust $ _ifcValid config)
    | otherwise = const []

  getBaseStyle wenv inst = _ifcStyle config >>= handler where
    handler lstyle = Just $ collectTheme wenv (cloneLens lstyle)

  init wenv inst = resultReqs reqs newInstance where
    newValue = getModelValue wenv
    newState = newTextState wenv inst state newValue (toText newValue) 0 Nothing
    newInstance = inst {
      _wiWidget = makeInputField config newState
    }
    parsedVal = fromText (toText newValue)
    reqs = setModelValid (isJust parsedVal)

  merge wenv oldState inst = resultReqs reqs newInstance where
    oldTextState = fromMaybe state (useState oldState)
    oldValue = _ifsCurrValue oldTextState
    oldText = _ifsCurrText oldTextState
    oldPos = _ifsCursorPos oldTextState
    oldSel = _ifsSelStart oldTextState
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
    newState = newTextState wenv inst oldTextState value newText newPos newSelStart
    newInstance = inst {
      _wiWidget = makeInputField config newState
    }
    parsedVal = fromText newText
    reqs = setModelValid (isJust parsedVal)

  handleKeyPress wenv mod code
    | isBackspace && emptySel = Just $ moveCursor removeText (tp - 1) Nothing
    | isBackspace = Just $ moveCursor removeText (min currSelVal tp) Nothing
    | isMoveLeft = Just $ moveCursor txt (tp - 1) Nothing
    | isMoveRight = Just $ moveCursor txt (tp + 1) Nothing
    | isMoveWordL = Just $ moveCursor txt prevWordStartIdx Nothing
    | isMoveWordR = Just $ moveCursor txt nextWordEndIdx Nothing
    | isSelectLeft = Just $ moveCursor txt (tp - 1) (Just tp)
    | isSelectRight = Just $ moveCursor txt (tp + 1) (Just tp)
    | isSelectWordL = Just $ moveCursor txt prevWordStartIdx (Just tp)
    | isSelectWordR = Just $ moveCursor txt nextWordEndIdx (Just tp)
    | otherwise = Nothing
    where
      txt = currText
      txtLen = T.length txt
      tp = currPos
      emptySel = isNothing currSel
      (part1, part2) = T.splitAt currPos currText
      prevWordStart = T.dropWhileEnd (not . delim) $ T.dropWhileEnd delim part1
      prevWordStartIdx = T.length prevWordStart
      nextWordEnd = T.dropWhile (not . delim) $ T.dropWhile delim part2
      nextWordEndIdx = txtLen - T.length nextWordEnd
      isShift = _kmLeftShift mod
      isWordMod
        | isMacOS wenv = _kmLeftAlt mod
        | otherwise = _kmLeftCtrl mod
      isBackspace = isKeyBackspace code && (tp > 0 || isJust currSel)
      isMove = not isShift && not isWordMod
      isMoveWord = not isShift && isWordMod
      isSelect = isShift && not isWordMod
      isSelectWord = isShift && isWordMod
      isMoveLeft = isMove && isKeyLeft code
      isMoveRight = isMove && isKeyRight code
      isMoveWordL = isMoveWord && isKeyLeft code
      isMoveWordR = isMoveWord && isKeyRight code
      isSelectLeft = isSelect && isKeyLeft code
      isSelectRight = isSelect && isKeyRight code
      isSelectWordL = isSelectWord && isKeyLeft code
      isSelectWordR = isSelectWord && isKeyRight code
      delim c = c == ' ' || c == '.' || c == ','
      currSelVal = fromMaybe 0 currSel
      removeText
        | isJust currSel = replaceText txt ""
        | otherwise = T.init part1 <> part2
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

  handleEvent wenv target evt inst = case evt of
    Click (Point x y) _ -> result where
      style = activeStyle wenv inst
      contentArea = getContentArea style inst
      localX = x - _rX contentArea + _ifsOffset state
      textLen = getGlyphsMax (_ifsGlyphs state)
      glyphs
        | Seq.null (_ifsGlyphs state) = Seq.empty
        | otherwise = _ifsGlyphs state |> GlyphPos ' ' textLen 0 0
      glyphStart i g = (i, abs (_glpXMin g - localX))
      pairs = Seq.mapWithIndex glyphStart glyphs
      cpm (_, g1) (_, g2) = compare g1 g2
      diffs = Seq.sortBy cpm pairs
      newPos = maybe 0 fst (Seq.lookup 0 diffs)
      newState = newTextState wenv inst state currVal currText newPos Nothing
      newInst = inst {
        _wiWidget = makeInputField config newState
      }
      result
        | isFocused wenv inst = Just $ resultWidget newInst
        | otherwise = Just $ resultReqs [SetFocus $ _wiPath inst] newInst

    KeyAction mod code KeyPressed -> result where
      result = handleKeyRes <$> handleKeyPress wenv mod code
      handleKeyRes (newText, newPos, newSel) = result where
        isPaste = isClipboardPaste wenv evt
        isCopy = isClipboardCopy wenv evt
        reqGetClipboard = [GetClipboard (_wiPath inst) | isPaste]
        reqSetClipboard = [SetClipboard (ClipboardText copyText) | isCopy]
        reqs = reqGetClipboard ++ reqSetClipboard
        result = genInputResult wenv inst False newText newPos newSel reqs

    TextInput newText -> insertText wenv inst newText

    Clipboard (ClipboardText newText) -> insertText wenv inst newText

    Focus -> Just $ resultReqs [StartTextInput (_wiViewport inst)] newInst where
      newState = state {
        _ifsSelStart = Just 0,
        _ifsCursorPos = T.length currText
      }
      newInst
        | _ifcSelectOnFocus config && T.length currText > 0 = inst {
            _wiWidget = makeInputField config newState
          }
        | otherwise = inst

    Blur -> Just $ resultReqs [StopTextInput] inst

    _ -> Nothing

  insertText wenv inst addedText = Just result where
    addedLen = T.length addedText
    newText = replaceText currText addedText
    newPos
      | isJust currSel = addedLen + min currPos (fromJust currSel)
      | otherwise = addedLen + currPos
    result = genInputResult wenv inst True newText newPos Nothing []

  replaceText txt newTxt
    | isJust currSel = T.take start txt <> newTxt <> T.drop end txt
    | otherwise = T.take currPos txt <> newTxt <> T.drop currPos txt
    where
      start = min currPos (fromJust currSel)
      end = max currPos (fromJust currSel)

  copyText
    | isJust currSel = T.take (end - start) $ T.drop start currText
    | otherwise = ""
    where
      start = min currPos (fromJust currSel)
      end = max currPos (fromJust currSel)

  genInputResult wenv inst textAdd newText newPos newSel newReqs = result where
    isValid = _ifcAcceptInput config newText
    hasChanged = currText /= newText
    newVal = fromText newText
    stateVal = fromMaybe currVal newVal
    onChangeEvts
      | stateVal /= currVal = fmap ($ stateVal) (_ifcOnChange config)
      | otherwise = []
    events = onChangeEvts
    reqValid
      | isValid = setModelValid (isJust newVal)
      | otherwise = []
    reqUpdateModel
      | isValid && hasChanged && isJust newVal = setModelValue (fromJust newVal)
      | otherwise = []
    reqOnChange
      | stateVal /= currVal = _ifcOnChangeReq config
      | otherwise = []
    reqs = newReqs ++ reqValid ++ reqUpdateModel ++ reqOnChange
    newState = newTextState wenv inst state stateVal newText newPos newSel
    newInstance = inst {
      _wiWidget = makeInputField config newState
    }
    result
      | isValid || not textAdd = resultReqsEvents reqs events newInstance
      | otherwise = resultReqsEvents reqs events inst

  getSizeReq wenv inst = sizeReq where
    style = activeStyle wenv inst
    Size w h = getTextSize wenv style currText
    factor = 1
    sizeReq = (FlexSize w factor, FixedSize h)

  resize wenv viewport renderArea inst = newInst where
    -- newTextState depends on having correct viewport/renderArea
    tempInst = inst {
      _wiViewport = viewport,
      _wiRenderArea = renderArea
    }
    newState = newTextState wenv tempInst state currVal currText currPos currSel
    newInst = tempInst {
      _wiWidget = makeInputField config newState
    }

  render renderer wenv inst = do
    setScissor renderer contentArea

    when (selRequired && isJust currSel) $
      drawRect renderer selRect (Just selColor) Nothing

    renderContent renderer state style currText

    when caretRequired $
      drawRect renderer caretRect (Just caretColor) Nothing

    resetScissor renderer
    where
      style = activeStyle wenv inst
      contentArea = getContentArea style inst
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
      selRequired = isFocused wenv inst
      selColor = styleHlColor style
      caretRequired = isFocused wenv inst && ts `mod` 1000 < 500
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

newTextState
  :: (Eq a, Default a)
  => WidgetEnv s e
  -> WidgetInstance s e
  -> InputFieldState a
  -> a
  -> Text
  -> Int
  -> Maybe Int
  -> InputFieldState a
newTextState wenv inst oldState value text cursor selection = newState where
  style = activeStyle wenv inst
  contentArea = getContentArea style inst
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
    | textFits = 0
    | alignL && cursorL = cx - tx + caretWidth
    | alignL && curX + oldOffset > cx + cw = cx + cw - curX
    | alignL && curX + oldOffset < cx = cx - curX
    | alignR && cursorR = 0
    | alignR && curX + oldOffset > cx + cw = tw - glyphX
    | alignR && curX + oldOffset < cx = tw - cw - glyphX
    | alignC && curX + oldOffset > cx + cw = cx + cw - curX
    | alignC && curX + oldOffset < cx = cx - curX
    | otherwise = oldOffset
  newState = InputFieldState {
    _ifsCurrValue = value,
    _ifsCurrText = text,
    _ifsGlyphs = glyphs,
    _ifsCursorPos = cursor,
    _ifsSelStart = selection,
    _ifsOffset = newOffset,
    _ifsTextRect = textRect & L.x .~ tx + newOffset,
    _ifsTextMetrics = textMetrics
  }
