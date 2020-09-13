{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.TextField (
  TextFieldCfg(..),
  textField,
  textField_,
  textFieldCfg
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

data TextFieldCfg s e a = TextFieldCfg {
  _tfcValue :: WidgetValue s a,
  _tfcValid :: Maybe (WidgetValue s a),
  _tfcFromText :: Text -> Maybe a,
  _tfcToText :: a -> Text,
  _tfcAcceptInput :: Text -> Bool,
  _tfcOnChange :: [a -> e],
  _tfcOnChangeReq :: [WidgetRequest s],
  _tfcCaretWidth :: Double
}

data TextFieldState = TextFieldState {
  _tfCurrText :: !Text,
  _tfGlyphs :: Seq GlyphPos,
  _tfCursorPos :: !Int,
  _tfSelStart :: Maybe Int,
  _tfOffset :: !Double,
  _tfTextRect :: Rect
} deriving (Eq, Show, Typeable)

textFieldCfg
  :: WidgetValue s a -> (Text -> Maybe a) -> (a -> Text) -> TextFieldCfg s e a
textFieldCfg value fromText toText = TextFieldCfg {
  _tfcValue = value,
  _tfcValid = Nothing,
  _tfcFromText = fromText,
  _tfcToText = toText,
  _tfcAcceptInput = const True,
  _tfcOnChange = [],
  _tfcOnChangeReq = [],
  _tfcCaretWidth = 2
}

textFieldState :: TextFieldState
textFieldState = TextFieldState {
  _tfCurrText = "",
  _tfGlyphs = Seq.empty,
  _tfCursorPos = 0,
  _tfSelStart = Nothing,
  _tfOffset = 0,
  _tfTextRect = def
}

textField :: ALens' s Text -> WidgetInstance s e
textField field = textField_ config where
  config = textFieldCfg (WidgetLens field) Just id

textField_ :: (Eq a) => TextFieldCfg s e a -> WidgetInstance s e
textField_ config = makeInstance $ makeTextField config textFieldState

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = (defaultWidgetInstance "textField" widget) {
  _wiFocusable = True
}

makeTextField :: (Eq a) => TextFieldCfg s e a -> TextFieldState -> Widget s e
makeTextField config state = widget where
  widget = createSingle def {
    singleInit = init,
    singleGetState = makeState state,
    singleMerge = merge,
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleResize = resize,
    singleRender = render
  }

  TextFieldState currText currGlyphs currPos currSel _ _ = state
  fromText = _tfcFromText config
  toText = _tfcToText config
  getModelValue wenv = widgetValueGet (_weModel wenv) (_tfcValue config)
  setModelValue = widgetValueSet (_tfcValue config)

  init wenv inst = resultWidget newInstance where
    newText = getModelValue wenv
    newState = newTextState wenv inst state (toText newText) 0 Nothing
    newInstance = inst {
      _wiWidget = makeTextField config newState
    }

  merge wenv oldState inst = resultWidget newInstance where
    oldTextState = fromMaybe state (useState oldState)
    oldText = _tfCurrText oldTextState
    oldPos = _tfCursorPos oldTextState
    oldSel = _tfSelStart oldTextState
    value = getModelValue wenv
    newText
      | fromText oldText /= Just (getModelValue wenv) = toText value
      | otherwise = oldText
    newTextL = T.length newText
    newPos
      | newTextL < oldPos = T.length newText
      | otherwise = oldPos
    newSelStart
      | isNothing oldSel || newTextL < fromJust oldSel = Nothing
      | otherwise = oldSel
    newState = newTextState wenv inst state newText newPos newSelStart
    newInstance = inst {
      _wiWidget = makeTextField config newState
    }

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
      localX = x - _rX rect + _tfOffset state
      textLen = glyphsLength (_tfGlyphs state)
      glyphs = _tfGlyphs state |> GlyphPos textLen 0 0
      zipper i g = (i, abs (_glpXMin g - localX))
      idxs = Seq.fromList [0..length glyphs]
      pairs = Seq.zipWith zipper idxs glyphs
      cpm (_, g1) (_, g2) = compare g1 g2
      diffs = Seq.sortBy cpm pairs
      newPos = maybe 0 fst (Seq.lookup 0 diffs)
      newState = newTextState wenv inst state currText newPos Nothing
      newInst = inst {
        _wiWidget = makeTextField config newState
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
      result = generateInputResult wenv inst newText newPos newSel reqs

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
    result = generateInputResult wenv inst newText newPos Nothing []

  generateInputResult wenv inst newText newPos newSel newReqs = result where
    isValid = _tfcAcceptInput config newText
    hasChanged = currText /= newText
    newVal = fromText newText
    reqUpdateModel
      | isValid && hasChanged && isJust newVal = setModelValue (fromJust newVal)
      | otherwise = []
    reqs = newReqs ++ reqUpdateModel
    newState = newTextState wenv inst state newText newPos newSel
    newInstance = inst {
      _wiWidget = makeTextField config newState
    }
    result
      | isValid = resultReqs reqs newInstance
      | otherwise = resultReqs reqs inst

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
    newState = newTextState wenv tempInst state currText currPos currSel
    newInst = tempInst {
      _wiWidget = makeTextField config newState
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
      textRect = _tfTextRect state
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
      caretWidth = _tfcCaretWidth config
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
  :: WidgetEnv s e
  -> WidgetInstance s e
  -> TextFieldState
  -> Text
  -> Int
  -> Maybe Int
  -> TextFieldState
newTextState wenv inst oldState text cursor selection = newState where
  theme = activeTheme wenv inst
  style = instanceStyle wenv inst
  contentRect = getContentRect style inst
  glyphs = getTextGlyphs wenv theme style text
  curX = maybe 0 _glpXMax $ Seq.lookup (cursor - 1) glyphs
  oldOffset = _tfOffset oldState
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
  newState = TextFieldState {
    _tfCurrText = text,
    _tfGlyphs = glyphs,
    _tfCursorPos = cursor,
    _tfSelStart = selection,
    _tfOffset = newOffset,
    _tfTextRect = Rect cx ty textW th
  }
