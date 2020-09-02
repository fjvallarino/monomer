{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.TextField (
  TextFieldCfg(..),
  textField,
  textField_,
  textFieldCfg
) where

import Control.Monad
import Control.Lens (ALens', (&), (.~))
import Data.Default
import Data.Maybe
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Typeable

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Common.Geometry
import Monomer.Common.StyleUtil
import Monomer.Event.Keyboard
import Monomer.Event.Types
import Monomer.Graphics.Drawing
import Monomer.Graphics.Types
import Monomer.Widget.BaseSingle
import Monomer.Widget.Types
import Monomer.Widget.Util

data TextFieldCfg s e = TextFieldCfg {
  _tfcValue :: WidgetValue s Text,
  _tfcOnChange :: [Text -> e],
  _tfcOnChangeReq :: [WidgetRequest s],
  _tfcCaretWidth :: Double
}

data TextFieldState = TextFieldState {
  _tfCurrText :: Text,
  _tfGlyphs :: Seq GlyphPos,
  _tfCursorPos :: Int,
  _stSelStart :: Maybe Int
} deriving (Eq, Show, Typeable)

textFieldCfg :: WidgetValue s Text -> TextFieldCfg s e
textFieldCfg value = TextFieldCfg {
  _tfcValue = value,
  _tfcOnChange = [],
  _tfcOnChangeReq = [],
  _tfcCaretWidth = 2
}

textFieldState :: TextFieldState
textFieldState = TextFieldState {
  _tfCurrText = "",
  _tfGlyphs = Seq.empty,
  _tfCursorPos = 0,
  _stSelStart = Nothing
}

textField :: ALens' s Text -> WidgetInstance s e
textField field = textField_ config where
  config = textFieldCfg (WidgetLens field)

textField_ :: TextFieldCfg s e -> WidgetInstance s e
textField_ config = makeInstance $ makeTextField config textFieldState

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = (defaultWidgetInstance "textField" widget) {
  _wiFocusable = True
}

makeTextField :: TextFieldCfg s e -> TextFieldState -> Widget s e
makeTextField config state = widget where
  widget = createSingle def {
    singleInit = init,
    singleGetState = makeState state,
    singleMerge = merge,
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  TextFieldState currText currGlyphs currPos currSel = state
  currentValue wenv = widgetValueGet (_weModel wenv) (_tfcValue config)

  init wenv inst = resultWidget newInstance where
    currText = currentValue wenv
    newState = newTextState wenv inst currText 0 Nothing
    newInstance = inst {
      _wiWidget = makeTextField config newState
    }

  merge wenv oldState inst = resultWidget newInstance where
    TextFieldState _ _ oldPos oldSel = fromMaybe state (useState oldState)
    currText = currentValue wenv
    currTextL = T.length currText
    newPos
      | currTextL < oldPos = T.length currText
      | otherwise = oldPos
    newSelStart
      | isNothing oldSel || currTextL < fromJust oldSel = Nothing
      | otherwise = oldSel
    newState = newTextState wenv inst currText newPos newSelStart
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
        | otherwise = (txt, fixedPos, newSel)
        where
          fixedPos = fixIdx newPos
      fixIdx idx
        | idx < 0 = 0
        | idx >= txtLen = txtLen
        | otherwise = idx

  handleEvent wenv target evt inst = case evt of
    Click (Point x y) _ -> Just $ resultReqs reqs inst where
      reqs = [SetFocus $ _wiPath inst]

    KeyAction mod code KeyPressed -> Just $ resultReqs reqs newInstance where
      (newText, newPos, newSel) = handleKeyPress wenv mod code
      isPaste = isClipboardPaste wenv evt
      isCopy = isClipboardCopy wenv evt
      reqGetClipboard = [GetClipboard (_wiPath inst) | isPaste]
      reqSetClipboard = [SetClipboard (ClipboardText copyText) | isCopy]
      reqUpdateModel
        | currText /= newText = widgetValueSet (_tfcValue config) newText
        | otherwise = []
      reqs = reqGetClipboard ++ reqSetClipboard ++ reqUpdateModel
      newState = newTextState wenv inst newText newPos newSel
      newInstance = inst {
        _wiWidget = makeTextField config newState
      }

    TextInput newText -> insertText wenv inst newText

    Clipboard (ClipboardText newText) -> insertText wenv inst newText

    Focus -> Just $ resultReqs [StartTextInput (_wiViewport inst)] inst

    Blur -> Just $ resultReqs [StopTextInput] inst

    _ -> Nothing

  insertText wenv inst addedText = Just $ resultReqs reqs newInst where
    newText = replaceText currText addedText
    newPos
      | isJust currSel = 1 + min currPos (fromJust currSel)
      | otherwise = currPos + T.length addedText
    newState = newTextState wenv inst newText newPos Nothing
    reqs = widgetValueSet (_tfcValue config) newText
    newInst = inst {
      _wiWidget = makeTextField config newState
    }

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

  render renderer wenv inst = do
    textRect <- drawStyledText renderer contentRect mergedStyle currText

    when (isJust currSel) $
      drawRect renderer (selRect textRect) caretColor Nothing

    when (isFocused wenv inst && isNothing currSel) $
      drawRect renderer (caretRect textRect) caretColor Nothing

    where
      WidgetInstance{..} = inst
      theme = activeTheme wenv inst
      style = activeStyle wenv inst
      mergedStyle = mergeThemeStyle theme style
      contentRect = getContentRect style inst
      Rect cx cy cw ch = contentRect
      selRect textRect = maybe def (mkSelRect textRect) currSel
      mkSelRect (Rect rx ry rw rh) end
        | currPos <= end = Rect (rx + gx currPos) ry (gw currPos (end - 1)) rh
        | otherwise = Rect (rx + gx end) ry (gw end (currPos - 1)) rh
      gx idx = _glpXMin (glyph idx)
      gw start end = abs $ _glpXMax (glyph end) - _glpXMin (glyph start)
      glyph idx = Seq.index currGlyphs idx
      ts = _weTimestamp wenv
      caretAlpha
        | isFocused wenv inst = fromIntegral (ts `mod` 1000) / 1000.0
        | otherwise = 0
      caretColor = Just $ textColor style & alpha .~ caretAlpha
      caretWidth = _tfcCaretWidth config
      caretPos
        | currPos == 0 = 0
        | otherwise = _glpXMax (glyph $ currPos - 1)
      caretRect (Rect tx ty tw th) = Rect (tx + caretPos) ty caretWidth th

newTextState
  :: WidgetEnv s e
  -> WidgetInstance s e
  -> Text
  -> Int
  -> Maybe Int
  -> TextFieldState
newTextState wenv inst text cursor selection = newState where
  theme = activeTheme wenv inst
  style = activeStyle wenv inst
  glyphs = getTextGlyphs wenv theme style text
  newState = TextFieldState {
    _tfCurrText = text,
    _tfGlyphs = glyphs,
    _tfCursorPos = cursor,
    _stSelStart = selection
  }
