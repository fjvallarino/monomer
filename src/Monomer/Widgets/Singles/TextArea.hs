{-|
Module      : Monomer.Widgets.Singles.TextArea
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Input field for multiline 'Text'. Allows setting the maximum number of
characters, lines and whether the tab key should trigger focus change.

@
textArea longTextLens
@

With configuration options:

@
textArea_ longTextLens [maxLength 1000, selectOnFocus]
@
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Monomer.Widgets.Singles.TextArea (
  -- * Configuration
  TextAreaCfg,
  -- * Constructors
  textArea,
  textArea_,
  textAreaV,
  textAreaV_,
  textAreaD_
) where

import Control.Applicative ((<|>))
import Control.Lens hiding ((|>))
import Control.Monad (forM_, when)
import Data.Default
import Data.Foldable (toList)
import Data.Maybe
import Data.Sequence (Seq(..), (|>))
import Data.Tuple (swap)
import Data.Text (Text)
import GHC.Generics

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Helper
import Monomer.Widgets.Containers.Scroll
import Monomer.Widgets.Single

import qualified Monomer.Lens as L

defCaretW :: Double
defCaretW = 2

defCaretMs :: Millisecond
defCaretMs = 500

{-|
Configuration options for textArea:

- 'maxLength': the maximum length of input text.
- 'maxLines': the maximum number of lines of input text.
- 'acceptTab': whether to handle tab and convert it to spaces (cancelling change
  of focus), or keep default behaviour and lose focus.
- 'selectOnFocus': Whether all input should be selected when focus is received.
- 'readOnly': Whether to prevent the user changing the input text.
- 'onFocus': event to raise when focus is received.
- 'onFocusReq': 'WidgetRequest' to generate when focus is received.
- 'onBlur': event to raise when focus is lost.
- 'onBlurReq': 'WidgetRequest' to generate when focus is lost.
- 'onChange': event to raise when the value changes.
- 'onChangeReq': 'WidgetRequest' to generate when the value changes.
-}
data TextAreaCfg s e = TextAreaCfg {
  _tacCaretWidth :: Maybe Double,
  _tacCaretMs :: Maybe Millisecond,
  _tacMaxLength :: Maybe Int,
  _tacMaxLines :: Maybe Int,
  _tacAcceptTab :: Maybe Bool,
  _tacSelectOnFocus :: Maybe Bool,
  _tacReadOnly :: Maybe Bool,
  _tacOnFocusReq :: [Path -> WidgetRequest s e],
  _tacOnBlurReq :: [Path -> WidgetRequest s e],
  _tacOnChangeReq :: [Text -> WidgetRequest s e]
}

instance Default (TextAreaCfg s e) where
  def = TextAreaCfg {
    _tacCaretWidth = Nothing,
    _tacCaretMs = Nothing,
    _tacMaxLength = Nothing,
    _tacMaxLines = Nothing,
    _tacAcceptTab = Nothing,
    _tacSelectOnFocus = Nothing,
    _tacReadOnly = Nothing,
    _tacOnFocusReq = [],
    _tacOnBlurReq = [],
    _tacOnChangeReq = []
  }

instance Semigroup (TextAreaCfg s e) where
  (<>) t1 t2 = TextAreaCfg {
    _tacCaretWidth = _tacCaretWidth t2 <|> _tacCaretWidth t1,
    _tacCaretMs = _tacCaretMs t2 <|> _tacCaretMs t1,
    _tacMaxLength = _tacMaxLength t2 <|> _tacMaxLength t1,
    _tacMaxLines = _tacMaxLines t2 <|> _tacMaxLines t1,
    _tacAcceptTab = _tacAcceptTab t2 <|> _tacAcceptTab t1,
    _tacSelectOnFocus = _tacSelectOnFocus t2 <|> _tacSelectOnFocus t1,
    _tacReadOnly = _tacReadOnly t2 <|> _tacReadOnly t1,
    _tacOnFocusReq = _tacOnFocusReq t1 <> _tacOnFocusReq t2,
    _tacOnBlurReq = _tacOnBlurReq t1 <> _tacOnBlurReq t2,
    _tacOnChangeReq = _tacOnChangeReq t1 <> _tacOnChangeReq t2
  }

instance Monoid (TextAreaCfg s e) where
  mempty = def

instance CmbCaretWidth (TextAreaCfg s e) Double where
  caretWidth w = def {
    _tacCaretWidth = Just w
  }

instance CmbCaretMs (TextAreaCfg s e) Millisecond where
  caretMs ms = def {
    _tacCaretMs = Just ms
  }

instance CmbMaxLength (TextAreaCfg s e) where
  maxLength len = def {
    _tacMaxLength = Just len
  }

instance CmbMaxLines (TextAreaCfg s e) where
  maxLines lines = def {
    _tacMaxLines = Just lines
  }

instance CmbAcceptTab (TextAreaCfg s e) where
  acceptTab_ accept = def {
    _tacAcceptTab = Just accept
  }

instance CmbSelectOnFocus (TextAreaCfg s e) where
  selectOnFocus_ sel = def {
    _tacSelectOnFocus = Just sel
  }

instance CmbReadOnly (TextAreaCfg s e) where
  readOnly_ ro = def {
    _tacReadOnly = Just ro
  }

instance WidgetEvent e => CmbOnFocus (TextAreaCfg s e) e Path where
  onFocus fn = def {
    _tacOnFocusReq = [RaiseEvent . fn]
  }

instance CmbOnFocusReq (TextAreaCfg s e) s e Path where
  onFocusReq req = def {
    _tacOnFocusReq = [req]
  }

instance WidgetEvent e => CmbOnBlur (TextAreaCfg s e) e Path where
  onBlur fn = def {
    _tacOnBlurReq = [RaiseEvent . fn]
  }

instance CmbOnBlurReq (TextAreaCfg s e) s e Path where
  onBlurReq req = def {
    _tacOnBlurReq = [req]
  }

instance WidgetEvent e => CmbOnChange (TextAreaCfg s e) Text e where
  onChange fn = def {
    _tacOnChangeReq = [RaiseEvent . fn]
  }

instance CmbOnChangeReq (TextAreaCfg s e) s e Text where
  onChangeReq req = def {
    _tacOnChangeReq = [req]
  }

data HistoryStep = HistoryStep {
  _tahText :: !Text,
  _tahCursorPos :: !(Int, Int),
  _tahSelStart :: Maybe (Int, Int)
} deriving (Eq, Show, Generic)

data TextAreaState = TextAreaState {
  _tasText :: Text,
  _tasTextMetrics :: TextMetrics,
  _tasTextStyle :: Maybe TextStyle,
  _tasCursorPos :: (Int, Int),
  _tasSelStart :: Maybe (Int, Int),
  _tasTextLines :: Seq TextLine,
  _tasHistory :: Seq HistoryStep,
  _tasHistoryIdx :: Int,
  _tasFocusStart :: Millisecond
} deriving (Eq, Show, Generic)

instance Default TextAreaState where
  def = TextAreaState {
    _tasText = "",
    _tasTextMetrics = def,
    _tasTextStyle = def,
    _tasCursorPos = def,
    _tasSelStart = def,
    _tasTextLines = Seq.empty,
    _tasHistory = Seq.empty,
    _tasHistoryIdx = 0,
    _tasFocusStart = 0
  }

-- | Creates a text area using the given lens.
textArea
  :: WidgetEvent e
  => ALens' s Text   -- ^ The lens into the model.
  -> WidgetNode s e  -- ^ The created text area.
textArea field = textArea_ field def

-- | Creates a text area using the given lens. Accepts config.
textArea_
  :: WidgetEvent e
  => ALens' s Text      -- ^ The lens into the model.
  -> [TextAreaCfg s e]  -- ^ The config options.
  -> WidgetNode s e     -- ^ The created text area.
textArea_ field configs = textAreaD_ wdata configs where
  wdata = WidgetLens field

-- | Creates a text area using the given value and 'onChange' event handler.
textAreaV
  :: WidgetEvent e
  => Text            -- ^ The current value.
  -> (Text -> e)     -- ^ The event to raise on change.
  -> WidgetNode s e  -- ^ The created text area.
textAreaV value handler = textAreaV_ value handler def

-- | Creates a text area using the given value and 'onChange' event handler.
--   Accepts config.
textAreaV_
  :: WidgetEvent e
  => Text               -- ^ The current value.
  -> (Text -> e)        -- ^ The event to raise on change.
  -> [TextAreaCfg s e]  -- ^ The config options.
  -> WidgetNode s e     -- ^ The created text area.
textAreaV_ value handler configs = textAreaD_ wdata newConfig where
  wdata = WidgetValue value
  newConfig = onChange handler : configs

-- | Creates a text area providing a 'WidgetData' instance and config.
textAreaD_
  :: WidgetEvent e
  => WidgetData s Text  -- ^ The 'WidgetData' to retrieve the value from.
  -> [TextAreaCfg s e]  -- ^ The config options.
  -> WidgetNode s e     -- ^ The created text area.
textAreaD_ wdata configs = scrollNode where
  config = mconcat configs
  widget = makeTextArea wdata config def
  node = defaultWidgetNode "textArea" widget
    & L.info . L.focusable .~ True
  scrollCfg = [scrollStyle L.textAreaStyle, scrollFwdStyle scrollFwdDefault]
  scrollNode = scroll_ scrollCfg node

makeTextArea
  :: WidgetEvent e
  => WidgetData s Text
  -> TextAreaCfg s e
  -> TextAreaState
  -> Widget s e
makeTextArea !wdata !config !state = widget where
  widget = createSingle state def {
    singleInit = init,
    singleMerge = merge,
    singleDispose = dispose,
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  !caretMs = fromMaybe defCaretMs (_tacCaretMs config)
  !maxLength = _tacMaxLength config
  !maxLines = _tacMaxLines config
  !editable = _tacReadOnly config /= Just True
  getModelValue !wenv = widgetDataGet (_weModel wenv) wdata
  -- State
  !currText = _tasText state
  !textLines = _tasTextLines state
  -- Helpers
  validText !state = validLen && validLines where
    text = _tasText state
    lines = _tasTextLines state
    validLen = T.length text <= fromMaybe maxBound maxLength
    validLines = length lines <= fromMaybe maxBound maxLines
  line !idx
    | idx >= 0 && idx < length textLines = Seq.index textLines idx ^. L.text
    | otherwise = ""
  !lineLen = T.length . line
  !totalLines = length textLines
  !lastPos = (lineLen (totalLines - 1), totalLines)

  init wenv node = resultNode newNode where
    text = getModelValue wenv
    newState = stateFromText wenv node state text
    newNode = node
      & L.widget .~ makeTextArea wdata config newState

  merge wenv node oldNode oldState = resultNode newNode where
    oldText = _tasText oldState
    newText = getModelValue wenv
    newState
      | oldText /= newText = stateFromText wenv node state newText
      | otherwise = oldState
    newNode = node
      & L.widget .~ makeTextArea wdata config newState

  dispose wenv node = resultReqs node reqs where
    widgetId = node ^. L.info . L.widgetId
    reqs = [RenderStop widgetId]

  handleKeyPress wenv mod code
    | isDelBackWordNoSel && editable = Just removeWordL
    | isDelBackWord && editable = Just (replaceText state selStart "")
    | isBackspace && emptySel && editable = Just removeCharL
    | isBackspace && editable = Just (replaceText state selStart "")
    | isMoveLeft = Just $ moveCursor txt (tpX - 1, tpY) Nothing
    | isMoveRight = Just $ moveCursor txt (tpX + 1, tpY) Nothing
    | isMoveUp = Just $ moveCursor txt (tpX, tpY - 1) Nothing
    | isMoveDown = Just $ moveCursor txt (tpX, tpY + 1) Nothing
    | isMovePageUp = Just $ moveCursor txt (tpX, tpY - vpLines) Nothing
    | isMovePageDown = Just $ moveCursor txt (tpX, tpY + vpLines) Nothing
    | isMoveWordL = Just $ moveCursor txt prevWordPos Nothing
    | isMoveWordR = Just $ moveCursor txt nextWordPos Nothing
    | isMoveLineL = Just $ moveCursor txt (0, tpY) Nothing
    | isMoveLineR = Just $ moveCursor txt (lineLen tpY, tpY) Nothing
    | isMoveFullUp = Just $ moveCursor txt (0, 0) Nothing
    | isMoveFullDn = Just $ moveCursor txt lastPos Nothing
    | isSelectAll = Just $ moveCursor txt (0, 0) (Just lastPos)
    | isSelectLeft = Just $ moveCursor txt (tpX - 1, tpY) (Just tp)
    | isSelectRight = Just $ moveCursor txt (tpX + 1, tpY) (Just tp)
    | isSelectUp = Just $ moveCursor txt (tpX, tpY - 1) (Just tp)
    | isSelectDown = Just $ moveCursor txt (tpX, tpY + 1) (Just tp)
    | isSelectPageUp = Just $ moveCursor txt (tpX, tpY - vpLines) (Just tp)
    | isSelectPageDown = Just $ moveCursor txt (tpX, tpY + vpLines) (Just tp)
    | isSelectWordL = Just $ moveCursor txt prevWordPos (Just tp)
    | isSelectWordR = Just $ moveCursor txt nextWordPos (Just tp)
    | isSelectLineL = Just $ moveCursor txt (0, tpY) (Just tp)
    | isSelectLineR = Just $ moveCursor txt (lineLen tpY, tpY) (Just tp)
    | isSelectFullUp = Just $ moveCursor txt (0, 0) (Just tp)
    | isSelectFullDn = Just $ moveCursor txt lastPos (Just tp)
    | isDeselectLeft = Just $ moveCursor txt minTpSel Nothing
    | isDeselectRight = Just $ moveCursor txt maxTpSel Nothing
    | isDeselectUp = Just $ moveCursor txt minTpSel Nothing
    | isDeselectDown = Just $ moveCursor txt maxTpSel Nothing
    | otherwise = Nothing
    where
      txt = currText
      txtLen = T.length txt
      textMetrics = _tasTextMetrics state
      tp@(tpX, tpY) = _tasCursorPos state
      selStart = _tasSelStart state

      (minTpSel, maxTpSel)
        | swap tp <= swap (fromJust selStart) = (tp, fromJust selStart)
        | otherwise = (fromJust selStart, tp)
      emptySel = isNothing selStart
      vpLines = round (wenv ^. L.viewport . L.h / textMetrics ^. L.lineH)
      activeSel = isJust selStart

      prevTxt
        | tpX > 0 = T.take tpX (line tpY)
        | otherwise = line (tpY - 1)
      prevWordStart = T.dropWhileEnd (not . delim) . T.dropWhileEnd delim $ prevTxt
      prevWordPos
        | tpX == 0 && tpY == 0 = (tpX, tpY)
        | tpX > 0 = (T.length prevWordStart, tpY)
        | otherwise = (T.length prevWordStart, tpY - 1)

      nextTxt
        | tpX < lineLen tpY = T.drop tpX (line tpY)
        | otherwise = line (tpY + 1)
      nextWordEnd = T.dropWhile (not . delim) . T.dropWhile delim $ nextTxt
      nextWordPos
        | tpX == lineLen tpY && tpY == length textLines - 1 = (tpX, tpY)
        | tpX < lineLen tpY = (lineLen tpY - T.length nextWordEnd, tpY)
        | otherwise = (lineLen (tpY + 1) - T.length nextWordEnd, tpY + 1)

      isShift = _kmLeftShift mod
      isLeft = isKeyLeft code
      isRight = isKeyRight code
      isUp = isKeyUp code
      isDown = isKeyDown code
      isHome = isKeyHome code
      isEnd = isKeyEnd code
      isPageUp = isKeyPageUp code
      isPageDown = isKeyPageDown code

      isWordMod
        | isMacOS wenv = _kmLeftAlt mod
        | otherwise = _kmLeftCtrl mod
      isLineMod
        | isMacOS wenv = _kmLeftCtrl mod || _kmLeftGUI mod
        | otherwise = _kmLeftAlt mod
      isAllMod
        | isMacOS wenv = _kmLeftGUI mod
        | otherwise = _kmLeftCtrl mod

      isBackspace = isKeyBackspace code
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
      isMoveFullUp = isMoveLine && isUp
      isMoveFullDn = isMoveLine && isDown
      isMoveUp = isMove && not activeSel && isUp
      isMoveDown = isMove && not activeSel && isDown
      isMovePageUp = isMove && not activeSel && isPageUp
      isMovePageDown = isMove && not activeSel && isPageDown

      isSelectAll = isAllMod && isKeyA code
      isSelectLeft = isSelect && isLeft
      isSelectRight = isSelect && isRight
      isSelectUp = isSelect && isUp
      isSelectDown = isSelect && isDown
      isSelectPageUp = isSelect && isPageUp
      isSelectPageDown = isSelect && isPageDown
      isSelectWordL = isSelectWord && isLeft
      isSelectWordR = isSelectWord && isRight
      isSelectLineL = (isSelectLine && isLeft) || (isShift && isHome)
      isSelectLineR = (isSelectLine && isRight) || (isShift && isEnd)
      isSelectFullUp = isSelectLine && isUp
      isSelectFullDn = isSelectLine && isDown

      isDeselectLeft = isMove && activeSel && isLeft
      isDeselectRight = isMove && activeSel && isRight
      isDeselectUp = isMove && activeSel && isUp
      isDeselectDown = isMove && activeSel && isDown

      replaceFix sel text = replaceText state (Just $ fixPos sel) text
      removeCharL
        | tpX > 0 = replaceFix (tpX - 1, tpY) ""
        | otherwise = replaceFix (lineLen (tpY - 1), tpY - 1) ""
      removeWordL = replaceFix prevWordPos ""
      moveCursor txt newPos newSel
        | isJust selStart && isNothing newSel = (txt, fixedPos, Nothing)
        | isJust selStart && Just fixedPos == selStart = (txt, fixedPos, Nothing)
        | isJust selStart = (txt, fixedPos, selStart)
        | Just fixedPos == fixedSel = (txt, fixedPos, Nothing)
        | otherwise = (txt, fixedPos, fixedSel)
        where
          fixedPos = fixPos newPos
          fixedSel = fmap fixPos newSel
      fixPos (cX, cY) = result where
        nlines = length textLines
        vcY = clamp 0 (nlines - 1) cY
        vcX = clamp 0 (lineLen tpY) cX
        ncX = clamp 0 (lineLen vcY) cX
        sameX = vcX == tpX
        sameY = vcY == tpY
        result
          | sameY && cX < 0 && vcY == 0 = (0, 0)
          | sameY && cX < 0 && vcY > 0 = (lineLen (vcY - 1) + cX + 1, vcY - 1)
          | sameY && cX > lineLen vcY && vcY < nlines - 1 = (cX - lineLen vcY - 1, vcY + 1)
          | sameX && cX > lineLen vcY = (min cX (lineLen vcY), vcY)
          | otherwise = (ncX, vcY)

  handleEvent wenv node target evt = case evt of
    ButtonAction point btn BtnPressed clicks
      | clicks == 1 -> Just result where
        newPos = findClosestGlyphPos state (localPoint point)
        newState = state {
          _tasCursorPos = newPos,
          _tasSelStart = Nothing
        }
        newNode = node
          & L.widget .~ makeTextArea wdata config newState
        result = resultReqs newNode [RenderOnce]

    -- Select word if clicked twice in a row
    ButtonAction point btn BtnReleased clicks
      | clicks == 2 -> result where
        (tx, ty) = findClosestGlyphPos state (localPoint point)
        currText = Seq.index textLines ty ^. L.text
        (part1, part2) = T.splitAt tx currText
        txtLen = T.length currText
        wordStart = T.dropWhileEnd (not . delim) part1
        wordStartIdx = T.length wordStart
        wordEnd = T.dropWhile (not . delim) part2
        wordEndIdx = txtLen - T.length wordEnd
        newPos = (wordStartIdx, ty)
        newSel = Just (wordEndIdx, ty)
        newState = state {
          _tasCursorPos = newPos,
          _tasSelStart = newSel
        }
        newNode = node
          & L.widget .~ makeTextArea wdata config newState
        result
          | ty < totalLines = Just (resultReqs newNode [RenderOnce])
          | otherwise = Nothing

    -- Select line if clicked three times in a row
    ButtonAction point btn BtnReleased clicks
      | clicks == 3 -> result where
        (tx, ty) = findClosestGlyphPos state (localPoint point)
        glyphs = Seq.index textLines ty ^. L.glyphs
        newPos = (0, ty)
        newSel = Just (length glyphs, ty)
        newState = state {
          _tasCursorPos = newPos,
          _tasSelStart = newSel
        }
        newNode = node
          & L.widget .~ makeTextArea wdata config newState
        result
          | ty < totalLines = Just (resultReqs newNode [RenderOnce])
          | otherwise = Nothing

    -- Select all if clicked four times in a row
    ButtonAction point btn BtnReleased clicks
      | clicks == 4 -> result where
        glyphs = Seq.index textLines (totalLines - 1) ^. L.glyphs
        newPos = (0, 0)
        newSel = Just (length glyphs, totalLines - 1)
        newState = state {
          _tasCursorPos = newPos,
          _tasSelStart = newSel
        }
        newNode = node
          & L.widget .~ makeTextArea wdata config newState
        result
          | totalLines > 0 = Just (resultReqs newNode [RenderOnce])
          | otherwise = Nothing

    Move point
      | isNodePressed wenv node -> Just result where
        curPos = _tasCursorPos state
        selStart = _tasSelStart state
        newPos = findClosestGlyphPos state (localPoint point)
        newSel = selStart <|> Just curPos
        newState = state {
          _tasCursorPos = newPos,
          _tasSelStart = newSel
        }
        scrollReq = generateScrollReq wenv node newState
        newNode = node
          & L.widget .~ makeTextArea wdata config newState
        result = resultReqs newNode (RenderOnce : scrollReq)

    KeyAction mod code KeyPressed
      | isKeyboardCopy wenv evt -> Just resultCopy
      | isKeyboardPaste wenv evt && editable -> Just resultPaste
      | isKeyboardCut wenv evt && editable -> Just resultCut
      | isKeyboardUndo wenv evt && editable -> Just $ moveHistory bwdState (-1)
      | isKeyboardRedo wenv evt && editable -> Just $ moveHistory state 1
      | isKeyReturn code && editable -> Just resultReturn
      | isKeyTab code && acceptTab && editable -> Just resultTab
      | otherwise -> fmap handleKeyRes (handleKeyPress wenv mod code)
      where
        acceptTab = fromMaybe False (_tacAcceptTab config)
        selectedText = fromMaybe "" (getSelection state)
        clipboardReq = SetClipboard (ClipboardText selectedText)

        resultCopy = resultReqs node [clipboardReq]
        resultPaste = resultReqs node [GetClipboard widgetId]
        resultCut = insertText wenv node ""
          & L.requests <>~ Seq.singleton clipboardReq
        resultReturn = insertText wenv node "\n"
        resultTab = insertText wenv node "    "
          & L.requests <>~ Seq.singleton IgnoreParentEvents

        history = _tasHistory state
        historyIdx = _tasHistoryIdx state

        bwdState = addHistory state (historyIdx == length history)

        moveHistory state steps = result where
          newIdx = clamp 0 (length history) (historyIdx + steps)
          newState = restoreHistory wenv node state newIdx
          newNode = node
            & L.widget .~ makeTextArea wdata config newState
          result = resultReqs newNode (generateReqs wenv node newState)

        handleKeyRes (newText, newPos, newSel) = result where
          tmpState = addHistory state (_tasText state /= newText)
          newState = (stateFromText wenv node tmpState newText) {
            _tasCursorPos = newPos,
            _tasSelStart = newSel
          }
          newNode = node
            & L.widget .~ makeTextArea wdata config newState
          result = resultReqs newNode (generateReqs wenv node newState)

    TextInput newText
      | editable -> Just result where
        result = insertText wenv node newText

    Clipboard (ClipboardText newText) -> Just result where
      result = insertText wenv node newText

    Focus prev -> Just result where
      selectOnFocus = fromMaybe False (_tacSelectOnFocus config)
      tmpState
        | selectOnFocus && T.length currText > 0 = state {
            _tasCursorPos = lastPos,
            _tasSelStart = Just (0, 0)
          }
        | otherwise = state
      newState = tmpState {
        _tasFocusStart = wenv ^. L.timestamp
      }
      reqs = [RenderEvery widgetId caretMs Nothing, StartTextInput viewport]
      newNode = node
        & L.widget .~ makeTextArea wdata config newState
      newResult = resultReqs newNode reqs
      focusRs = handleFocusChange newNode prev (_tacOnFocusReq config)
      result = maybe newResult (newResult <>) focusRs

    Blur next -> Just result where
      reqs = [RenderStop widgetId, StopTextInput]
      newResult = resultReqs node reqs
      blurRes = handleFocusChange node next (_tacOnBlurReq config)
      result = maybe newResult (newResult <>) blurRes
    _ -> Nothing

    where
      widgetId = node ^. L.info . L.widgetId
      viewport = node ^. L.info . L.viewport
      style = currentStyle wenv node
      Rect cx cy cw ch = getContentArea node style
      localPoint point = subPoint point (Point cx cy)

  insertText wenv node addedText = result where
    currSel = _tasSelStart state
    (newText, newPos, newSel) = replaceText state currSel addedText
    tmpState = addHistory state (_tasText state /= newText)
    newState = (stateFromText wenv node tmpState newText) {
      _tasCursorPos = newPos,
      _tasSelStart = newSel
    }
    newNode = node
      & L.widget .~ makeTextArea wdata config newState
    newReqs = generateReqs wenv node newState
    result
      | validText newState = resultReqs newNode newReqs
      | otherwise = resultNode node

  generateReqs wenv node newState = reqs ++ reqScroll where
    widgetId = node ^. L.info . L.widgetId
    oldText = _tasText state
    newText = _tasText newState
    reqUpdate = widgetDataSet wdata newText
    reqOnChange = fmap ($ newText) (_tacOnChangeReq config)
    reqResize = [ResizeWidgetsImmediate widgetId]
    reqScroll = generateScrollReq wenv node newState
    reqs
      | oldText /= newText = reqUpdate ++ reqOnChange ++ reqResize
      | otherwise = []

  generateScrollReq wenv node newState = scrollReq where
    style = currentStyle wenv node
    scPath = parentPath node
    scWid = widgetIdFromPath wenv scPath
    contentArea = getContentArea node style
    offset = Point (contentArea ^. L.x) (contentArea ^. L.y)
    caretRect = getCaretRect config newState
    -- Padding/border added to show left/top borders when moving near them
    scrollRect = fromMaybe caretRect (addOuterBounds style caretRect)
    scrollMsg = ScrollTo $ moveRect offset scrollRect
    scrollReq
      | rectInRect caretRect (wenv ^. L.viewport) || isNothing scWid = []
      | otherwise = [SendMessage (fromJust scWid) scrollMsg]

  getSizeReq wenv node = sizeReq where
    Size w h = getTextLinesSize textLines
    {- getTextLines does not return the vertical spacing for the last line, but
    we need it since the selection rect displays it. -}
    spaceV = getSpaceV textLines
    sizeReq = (minWidth (max 100 w), minHeight (max 20 (h + spaceV)))

  render wenv node renderer =
    drawInTranslation renderer offset $ do
      when selRequired $
        forM_ selRects $ \rect ->
          when (rect ^. L.w > 0) $
            drawRect renderer rect (Just selColor) Nothing

      forM_ textLines (drawTextLine renderer style)

      when caretRequired $
        drawRect renderer caretRect (Just caretColor) Nothing
    where
      style = currentStyle wenv node
      contentArea = getContentArea node style
      ts = _weTimestamp wenv
      offset = Point (contentArea ^. L.x) (contentArea ^. L.y)
      focused = isNodeFocused wenv node

      caretTs = ts - _tasFocusStart state
      caretRequired = focused && even (caretTs `div` caretMs)
      caretColor = styleFontColor style
      caretRect = getCaretRect config state

      selRequired = isJust (_tasSelStart state)
      selColor = styleHlColor style
      selRects = getSelectionRects state contentArea

getCaretRect :: TextAreaCfg s e -> TextAreaState -> Rect
getCaretRect config state = caretRect where
  (cursorX, cursorY) = _tasCursorPos state
  Rect tx ty _ _ = lineRect
  TextMetrics asc desc lineh _ = _tasTextMetrics state
  textLines = _tasTextLines state

  (lineRect, glyphs, fspaceV) = case Seq.lookup cursorY textLines of
    Just tl -> (tl ^. L.rect, tl ^. L.glyphs, tl ^. L.fontSpaceV)
    Nothing -> (def, Seq.empty, def)
  spaceV = unFontSpace fspaceV

  caretPos
    | cursorX == 0 || cursorX > length glyphs = 0
    | cursorX == length glyphs = _glpXMax (Seq.index glyphs (cursorX - 1))
    | otherwise = _glpXMin (Seq.index glyphs cursorX)

  caretX = max 0 (tx + caretPos)
  caretY = ty - spaceV
  caretW = fromMaybe defCaretW (_tacCaretWidth config)
  caretH = lineh + spaceV

  caretRect = Rect caretX caretY caretW caretH

getSelectionRects :: TextAreaState -> Rect -> [Rect]
getSelectionRects state contentArea = rects where
  currPos = _tasCursorPos state
  currSel = fromMaybe def (_tasSelStart state)
  TextMetrics asc desc lineh _ = _tasTextMetrics state
  textLines = _tasTextLines state

  spaceV = getSpaceV textLines
  line idx
    | length textLines > idx = Seq.index textLines idx ^. L.text
    | otherwise = ""
  lineLen = T.length . line

  glyphs idx
    | length textLines > idx = Seq.index textLines idx ^. L.glyphs
    | otherwise = Seq.empty
  glyphPos posx posy
    | posx == 0 = 0
    | posx == lineLen posy = _glpXMax (Seq.index (glyphs posy) (posx - 1))
    | otherwise = _glpXMin (Seq.index (glyphs posy) posx)

  ((selX1, selY1), (selX2, selY2))
    | swap currPos <= swap currSel = (currPos, currSel)
    | otherwise = (currSel, currPos)

  totalH = lineh + spaceV
  updateRect rect = rect
    & L.y -~ spaceV
    & L.h .~ totalH
    & L.w %~ max 5 -- Empty lines show a small rect to indicate they are there.
  makeRect cx1 cx2 cy = Rect rx ry rw rh where
    rx = glyphPos cx1 cy
    rw = glyphPos cx2 cy - rx
    ry = fromIntegral cy * totalH - spaceV
    rh = totalH
  rects
    | selY1 == selY2 = [makeRect selX1 selX2 selY1]
    | otherwise = begin : middle ++ end where
      begin = makeRect selX1 (lineLen selY1) selY1
      middleLines = Seq.drop (selY1 + 1) . Seq.take selY2 $ textLines
      middle = toList (updateRect . view L.rect <$> middleLines)
      end = [makeRect 0 selX2 selY2]

stateFromText
  :: WidgetEnv s e -> WidgetNode s e -> TextAreaState -> Text -> TextAreaState
stateFromText wenv node state text = newState where
  style = currentStyle wenv node
  fontMgr = wenv ^. L.fontManager
  newTextMetrics = getTextMetrics wenv style
  tmpTextLines = fitTextToWidth fontMgr style maxNumericValue KeepSpaces text
  totalH = newTextMetrics ^. L.lineH + getSpaceV tmpTextLines
  lastRect = def
    & L.y .~ fromIntegral (length tmpTextLines) * totalH
    & L.h .~ totalH

  lastTextLine = def
    & L.rect .~ lastRect
    & L.size .~ Size 0 (lastRect ^. L.h)
  newTextLines
    | T.isSuffixOf "\n" text = tmpTextLines |> lastTextLine
    | otherwise = tmpTextLines

  newState = state {
    _tasText = text,
    _tasTextMetrics = newTextMetrics,
    _tasTextStyle = style ^. L.text,
    _tasTextLines = newTextLines
  }

textFromState :: Seq TextLine -> Text
textFromState textLines = T.unlines lines where
  lines = toList (view L.text <$> textLines)

addHistory :: TextAreaState -> Bool -> TextAreaState
addHistory state False = state
addHistory state _ = newState where
  text = _tasText state
  curPos = _tasCursorPos state
  selStart = _tasSelStart state
  prevStepIdx = _tasHistoryIdx state
  prevSteps = _tasHistory state
  steps = Seq.take prevStepIdx prevSteps
  newState = state {
    _tasHistory = steps |> HistoryStep text curPos selStart,
    _tasHistoryIdx = prevStepIdx + 1
  }

restoreHistory
  :: WidgetEnv s e -> WidgetNode s e -> TextAreaState -> Int -> TextAreaState
restoreHistory wenv node state idx
  | idx >= 0 && idx < length hist && idx /= histIdx = newState
  | otherwise = state
  where
    hist = _tasHistory state
    histIdx = _tasHistoryIdx state
    HistoryStep text curPos selStart = Seq.index hist idx
    tmpState = stateFromText wenv node state text
    newState = tmpState {
      _tasCursorPos = curPos,
      _tasSelStart = selStart,
      _tasHistoryIdx = idx
    }

getSelection
  :: TextAreaState
  -> Maybe Text
getSelection state = result where
  currPos = _tasCursorPos state
  currSel = fromJust (_tasSelStart state)
  textLines = _tasTextLines state
  oldLines = view L.text <$> textLines

  ((selX1, selY1), (selX2, selY2))
    | swap currPos <= swap currSel = (currPos, currSel)
    | otherwise = (currSel, currPos)
  newText
    | selY1 == selY2 = singleLine
    | selX2 == 0 = T.unlines . toList $ begin :<| middle
    | otherwise = T.unlines . toList $ begin :<| (middle :|> end)
    where
      singleLine = T.drop selX1 $ T.take selX2 (Seq.index oldLines selY1)
      begin = T.drop selX1 $ Seq.index oldLines selY1
      middle = Seq.drop (selY1 + 1) $ Seq.take selY2 oldLines
      end = T.take selX2 $ Seq.index oldLines selY2
  result
    | isJust (_tasSelStart state) = Just newText
    | otherwise = Nothing

replaceText
  :: TextAreaState
  -> Maybe (Int, Int)
  -> Text
  -> (Text, (Int, Int), Maybe (Int, Int))
replaceText state currSel newTxt
  | isJust currSel = replaceSelection lines currPos (fromJust currSel) newTxt
  | otherwise = replaceSelection lines currPos currPos newTxt
  where
    currPos = _tasCursorPos state
    lines = _tasTextLines state

replaceSelection
  :: Seq TextLine
  -> (Int, Int)
  -> (Int, Int)
  -> Text
  -> (Text, (Int, Int), Maybe (Int, Int))
replaceSelection textLines currPos currSel addText = result where
  oldLines = view L.text <$> textLines
  ((selX1, selY1), (selX2, selY2))
    | swap currPos <= swap currSel = (currPos, currSel)
    | otherwise = (currSel, currPos)
  prevLines = Seq.take selY1 oldLines
  postLines = Seq.drop (selY2 + 1) oldLines
  returnAdded = T.isSuffixOf "\n" addText

  linePre
    | length oldLines > selY1 = T.take selX1 (Seq.index oldLines selY1)
    | otherwise = ""
  lineSuf
    | length oldLines > selY2 = T.drop selX2 (Seq.index oldLines selY2)
    | otherwise = ""
  addLines
    | not returnAdded = Seq.fromList (T.lines addText)
    | otherwise = Seq.fromList (T.lines addText) :|> ""

  (newX, newY, midLines)
    | length addLines <= 1 = (T.length (linePre <> addText), selY1, singleLine)
    | otherwise = (T.length end, selY1 + length addLines - 1, multiline)
    where
      singleLine = Seq.singleton $ linePre <> addText <> lineSuf
      begin = Seq.index addLines 0
      middle = Seq.drop 1 $ Seq.take (length addLines - 1) addLines
      end = Seq.index addLines (length addLines - 1)
      multiline = (linePre <> begin) :<| (middle :|> (end <> lineSuf))

  newLines = prevLines <> midLines <> postLines
  newText = T.dropEnd 1 $ T.unlines (toList newLines)
  result = (newText, (newX, newY), Nothing)

findClosestGlyphPos :: TextAreaState -> Point -> (Int, Int)
findClosestGlyphPos state point = (newPos, lineIdx) where
  Point x y = point
  TextMetrics _ _ lineh _ = _tasTextMetrics state
  textLines = _tasTextLines state

  totalH = lineh + getSpaceV textLines
  lineIdx = clamp 0 (length textLines - 1) (floor (y / totalH))
  lineGlyphs
    | null textLines = Seq.empty
    | otherwise = Seq.index (view L.glyphs <$> textLines) lineIdx
  textLen = getGlyphsMax lineGlyphs

  glyphs
    | Seq.null lineGlyphs = Seq.empty
    | otherwise = lineGlyphs |> GlyphPos ' ' 0 textLen 0 0 0 0 0
  glyphStart i g = (i, abs (_glpXMin g - x))

  pairs = Seq.mapWithIndex glyphStart glyphs
  cpm (_, g1) (_, g2) = compare g1 g2
  diffs = Seq.sortBy cpm pairs
  newPos = maybe 0 fst (Seq.lookup 0 diffs)

getSpaceV :: Seq TextLine -> Double
getSpaceV textLines = spaceV where
  spaceV = unFontSpace $ maybe def (view L.fontSpaceV) (textLines ^? ix 0)

delim :: Char -> Bool
delim c = c `elem` [' ', '.', ',', '/', '-', ':']
