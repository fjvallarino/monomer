{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.InputField (
  InputFieldCfg(..),
  inputFieldCfg,
  inputField_,
  makeInputField
) where

import Control.Monad
import Control.Lens (ALens', (&), (.~), (^.), (^?), _Just, non)
import Data.Default
import Data.Maybe
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import Data.Typeable

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.StyleUtil
import Monomer.Event.Keyboard
import Monomer.Event.Types
import Monomer.Graphics.Drawing
import Monomer.Graphics.Types
import Monomer.Widget.BaseSingle
import Monomer.Widget.Types
import Monomer.Widget.Util

import qualified Monomer.Common.LensStyle as S

data InputFieldCfg s e a = InputFieldCfg {
  _ifcValue :: WidgetValue s a,
  _ifcValid :: Maybe (WidgetValue s Bool),
  _ifcFromText :: Text -> Maybe a,
  _ifcToText :: a -> Text,
  _ifcAcceptInput :: Text -> Bool,
  _ifcOnChange :: [a -> e],
  _ifcOnChangeReq :: [WidgetRequest s],
  _ifcCaretWidth :: Double
}

data InputFieldState a = InputFieldState {
  _ifsCurrValue :: a,
  _ifsCurrText :: !Text,
  _ifsGlyphs :: Seq GlyphPos,
  _ifsCursorPos :: !Int,
  _ifsSelStart :: Maybe Int,
  _ifsOffset :: !Double,
  _ifsTextRect :: Rect
} deriving (Eq, Show, Typeable)

inputFieldCfg
  :: WidgetValue s a -> (Text -> Maybe a) -> (a -> Text) -> InputFieldCfg s e a
inputFieldCfg value fromText toText = InputFieldCfg {
  _ifcValue = value,
  _ifcValid = Nothing,
  _ifcFromText = fromText,
  _ifcToText = toText,
  _ifcAcceptInput = const True,
  _ifcOnChange = [],
  _ifcOnChangeReq = [],
  _ifcCaretWidth = 2
}

inputFieldState :: Default a => InputFieldState a
inputFieldState = InputFieldState {
  _ifsCurrValue = def,
  _ifsCurrText = "",
  _ifsGlyphs = Seq.empty,
  _ifsCursorPos = 0,
  _ifsSelStart = Nothing,
  _ifsOffset = 0,
  _ifsTextRect = def
}

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
    singleInit = init,
    singleGetState = makeState state,
    singleMerge = merge,
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleResize = resize,
    singleRender = render
  }

  InputFieldState currVal currText currGlyphs currPos currSel _ _ = state
  fromText = _ifcFromText config
  toText = _ifcToText config
  getModelValue wenv = widgetValueGet (_weModel wenv) (_ifcValue config)
  setModelValue = widgetValueSet (_ifcValue config)
  setModelValid
    | isJust (_ifcValid config) = widgetValueSet (fromJust $ _ifcValid config)
    | otherwise = const []

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
      | newTextL < oldPos = T.length newText
      | otherwise = oldPos
    newSelStart
      | isNothing oldSel || newTextL < fromJust oldSel = Nothing
      | otherwise = oldSel
    newState = newTextState wenv inst state value newText newPos newSelStart
    newInstance = inst {
      _wiWidget = makeInputField config newState
    }
    parsedVal = fromText newText
    reqs = setModelValid (isJust parsedVal)

  handleKeyPress wenv mod code
    | isBackspace = moveCursor removeText (tp - 1) Nothing
    | isMoveLeft = moveCursor txt (tp - 1) Nothing
    | isMoveRight = moveCursor txt (tp + 1) Nothing
    | isMoveWordL = moveCursor txt prevWordStartIdx Nothing
    | isMoveWordR = moveCursor txt nextWordEndIdx Nothing
    | isSelectLeft = moveCursor txt (tp - 1) (Just tp)
    | isSelectRight = moveCursor txt (tp + 1) (Just tp)
    | isSelectWordL = moveCursor txt prevWordStartIdx (Just tp)
    | isSelectWordR = moveCursor txt nextWordEndIdx (Just tp)
    | otherwise = moveCursor txt tp currSel
    where
      txt = currText
      txtLen = T.length txt
      tp = currPos
      (part1, part2) = T.splitAt currPos currText
      prevWordStart = T.dropWhileEnd (not . delim) $ T.dropWhileEnd delim part1
      prevWordStartIdx = T.length prevWordStart
      nextWordEnd = T.dropWhile (not . delim) $ T.dropWhile delim part2
      nextWordEndIdx = txtLen - T.length nextWordEnd
      isShift = _kmLeftShift mod
      isWordMod
        | isMacOS wenv = _kmLeftAlt mod
        | otherwise = _kmLeftCtrl mod
      isBackspace = isKeyBackspace code && tp > 0
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
        | isJust currSel && isNothing newSel = (txt, tp, Nothing)
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
      rect = getContentRect style inst
      localX = x - _rX rect + _ifsOffset state
      textLen = glyphsLength (_ifsGlyphs state)
      glyphs = _ifsGlyphs state |> GlyphPos textLen 0 0
      zipper i g = (i, abs (_glpXMin g - localX))
      idxs = Seq.fromList [0..length glyphs]
      pairs = Seq.zipWith zipper idxs glyphs
      cpm (_, g1) (_, g2) = compare g1 g2
      diffs = Seq.sortBy cpm pairs
      newPos = maybe 0 fst (Seq.lookup 0 diffs)
      newState = newTextState wenv inst state currVal currText newPos Nothing
      newInst = inst {
        _wiWidget = makeInputField config newState
      }
      result
        | isFocused wenv inst = Just $ resultWidget newInst
        | otherwise = Just $ resultReqs [SetFocus $ _wiPath inst] inst

    KeyAction mod code KeyPressed -> Just result where
      (newText, newPos, newSel) = handleKeyPress wenv mod code
      isPaste = isClipboardPaste wenv evt
      isCopy = isClipboardCopy wenv evt
      reqGetClipboard = [GetClipboard (_wiPath inst) | isPaste]
      reqSetClipboard = [SetClipboard (ClipboardText copyText) | isCopy]
      reqs = reqGetClipboard ++ reqSetClipboard
      result = genInputResult wenv inst False newText newPos newSel reqs

    TextInput newText -> insertText wenv inst newText

    Clipboard (ClipboardText newText) -> insertText wenv inst newText

    Focus -> Just $ resultReqs [StartTextInput (_wiViewport inst)] inst

    Blur -> Just $ resultReqs [StopTextInput] inst

    _ -> Nothing

  insertText wenv inst addedText = Just result where
    newText = replaceText currText addedText
    newPos
      | isJust currSel = 1 + min currPos (fromJust currSel)
      | otherwise = currPos + T.length addedText
    result = genInputResult wenv inst True newText newPos Nothing []

  genInputResult wenv inst textAdd newText newPos newSel newReqs = result where
    isValid = _ifcAcceptInput config newText
    hasChanged = currText /= newText
    newVal = fromText newText
    stateVal = fromMaybe currVal newVal
    onChangeEvts
      | stateVal /= currVal = fmap ($ stateVal) (_ifcOnChange config)
      | otherwise = []
    events = onChangeEvts
    reqValid = setModelValid (isJust newVal)
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

  getSizeReq wenv inst = sizeReq where
    theme = activeTheme wenv inst
    style = activeStyle wenv inst
    size = getTextSize wenv theme style currText
    sizeReq = SizeReq size FlexibleSize StrictSize

  resize wenv viewport renderArea inst = newInst where
    tempInst = inst {
      _wiViewport = viewport,
      _wiRenderArea = renderArea
    }
    newState = newTextState wenv tempInst state currVal currText currPos currSel
    newInst = tempInst {
      _wiWidget = makeInputField config newState
    }

  render renderer wenv inst = do
    setScissor renderer contentRect

    when (isJust currSel) $
      drawRect renderer selRect (Just selColor) Nothing

    renderContent renderer textRect style currText

    when caretRequired $
      drawRect renderer caretRect (Just caretColor) Nothing

    resetScissor renderer
    where
      WidgetInstance{..} = inst
      style = instanceStyle wenv inst
      contentRect = getContentRect style inst
      Rect cx cy cw ch = contentRect
      textRect = _ifsTextRect state
      Rect tx ty tw th = textRect
      selRect = maybe def mkSelRect currSel
      mkSelRect end
        | currPos <= end = Rect (tx + gx currPos) ty (gw currPos (end - 1)) th
        | otherwise = Rect (tx + gx end) ty (gw end (currPos - 1)) th
      gx idx = _glpXMin (glyph idx)
      gw start end = abs $ _glpXMax (glyph end) - _glpXMin (glyph start)
      nglyphs = Seq.length currGlyphs
      glyph idx = Seq.index currGlyphs (min idx (nglyphs - 1))
      ts = _weTimestamp wenv
      selColor = instanceHlColor wenv inst
      caretRequired = isFocused wenv inst && ts `mod` 1000 < 500
      caretColor = instanceFontColor wenv inst
      caretWidth = _ifcCaretWidth config
      caretPos
        | currPos == 0 = 0
        | currPos == nglyphs = _glpXMax (glyph $ currPos - 1)
        | otherwise = _glpXMin (glyph currPos)
      caretX tx = max 0 $ min (cx + cw - caretWidth) (tx + caretPos)
      caretRect = Rect (caretX tx) ty caretWidth th

renderContent :: Renderer -> Rect -> StyleState -> Text -> IO Rect
renderContent renderer textRect style currText =
  drawText renderer textRect tsColor tsFont tsFontSize tsAlign currText
  where
    textStyle = fromJust (_sstText style)
    tsFont = fromJust (_txsFont textStyle)
    tsFontSize = fromJust (_txsFontSize textStyle)
    tsColor = fromJust (_txsFontColor textStyle)
    tsAlignV = fromMaybe AMiddle (_txsAlignV textStyle)
    tsAlign = Align ALeft tsAlignV

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
  theme = activeTheme wenv inst
  style = instanceStyle wenv inst
  contentRect = getContentRect style inst
  glyphs = getTextGlyphs wenv theme style text
  curX = maybe 0 _glpXMax $ Seq.lookup (cursor - 1) glyphs
  oldOffset = _ifsOffset oldState
  textStyle = fromJust (_sstText style)
  alignH = fromMaybe ALeft (_txsAlignH textStyle)
  alignV = fromMaybe def (_txsAlignV textStyle)
  align = Align alignH alignV
  Rect cx cy cw ch = contentRect
  Rect tx ty tw th = getTextRect wenv theme style contentRect align text
  textW = glyphsLength glyphs
  textFits = cw >= textW
  newOffset
    | textFits && alignH == ALeft = 0
    | textFits && alignH == ACenter = (cw - textW) / 2
    | textFits && alignH == ARight = cw - textW
    | curX + oldOffset > cw = cw - curX
    | curX + oldOffset < 0 = -curX
    | otherwise = oldOffset
  newState = InputFieldState {
    _ifsCurrValue = value,
    _ifsCurrText = text,
    _ifsGlyphs = glyphs,
    _ifsCursorPos = cursor,
    _ifsSelStart = selection,
    _ifsOffset = newOffset,
    _ifsTextRect = Rect cx ty textW th
  }
