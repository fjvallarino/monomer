{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Monomer.Widgets.Singles.TextArea (
  textArea,
  textArea_
) where

import Debug.Trace

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~), ALens', view)
import Control.Monad (forM_, when)
import Data.Default
import Data.Foldable (toList)
import Data.Maybe
import Data.Sequence (Seq(..))
import Data.Tuple (swap)
import Data.Text (Text)
import GHC.Generics

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Widgets.Single

import qualified Monomer.Lens as L

caretW :: Double
caretW = 2

caretMs :: Int
caretMs = 500

data TextAreaCfg = TextAreaCfg {
  _tacMaxLength :: Maybe Int,
  _tacMaxLines :: Maybe Int
}

instance Default TextAreaCfg where
  def = TextAreaCfg {
    _tacMaxLength = Nothing,
    _tacMaxLines = Nothing
  }

instance Semigroup TextAreaCfg where
  (<>) s1 s2 = TextAreaCfg {
    _tacMaxLength = _tacMaxLength s2 <|> _tacMaxLength s1,
    _tacMaxLines = _tacMaxLines s2 <|> _tacMaxLines s1
  }

instance Monoid TextAreaCfg where
  mempty = def

instance CmbMaxLength TextAreaCfg where
  maxLength len = def {
    _tacMaxLength = Just len
  }

instance CmbMaxLines TextAreaCfg where
  maxLines lines = def {
    _tacMaxLines = Just lines
  }

data TextAreaState = TextAreaState {
  _tasText :: Text,
  _tasTextMetrics :: TextMetrics,
  _tasTextStyle :: Maybe TextStyle,
  _tasCursorPos :: (Int, Int),
  _tasSelStart :: Maybe (Int, Int),
  _tasTextLines :: Seq TextLine
} deriving (Eq, Show, Generic)

instance Default TextAreaState where
  def = TextAreaState {
    _tasText = "",
    _tasTextMetrics = def,
    _tasTextStyle = def,
    _tasCursorPos = def,
    _tasSelStart = def,
    _tasTextLines = def
  }

textArea :: ALens' s Text -> WidgetNode s e
textArea field = textArea_ field def

textArea_ :: ALens' s Text -> [TextAreaCfg] -> WidgetNode s e
textArea_ field configs = node where
  config = mconcat configs
  wdata = WidgetLens field
  widget = makeTextArea wdata config def
  node = defaultWidgetNode "textArea" widget
    & L.info . L.focusable .~ True

makeTextArea :: WidgetData s Text -> TextAreaCfg -> TextAreaState -> Widget s e
makeTextArea wdata config state = widget where
  widget = createSingle state def {
    singleGetBaseStyle = getBaseStyle,
    singleInit = init,
    singleDispose = dispose,
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  maxLength = _tacMaxLength config
  maxLines = _tacMaxLines config
  getModelValue wenv = widgetDataGet (_weModel wenv) wdata
  -- State
  currText = _tasText state
  textLines = _tasTextLines state

  getBaseStyle wenv node = Just style where
    style = collectTheme wenv L.textAreaStyle

  init wenv node = resultWidget newNode where
    text = getModelValue wenv
    newState = stateFromText wenv node state text
    newNode = node
      & L.widget .~ makeTextArea wdata config newState

  dispose wenv node = resultReqs node reqs where
    widgetId = node ^. L.info . L.widgetId
    reqs = [RenderStop widgetId]

  handleKeyPress wenv mod code
    | isDelBackWordNoSel = Just removeWordL
    | isDelBackWord = Just (replaceText state selStart "")
    | isBackspace && emptySel = Just removeCharL
    | isBackspace = Just (replaceText state selStart "")
    | isMoveLeft = Just $ moveCursor txt (tpX - 1, tpY) Nothing
    | isMoveRight = Just $ moveCursor txt (tpX + 1, tpY) Nothing
    | isMoveUp = Just $ moveCursor txt (tpX, tpY - 1) Nothing
    | isMoveDown = Just $ moveCursor txt (tpX, tpY + 1) Nothing
    | isMoveWordL = Just $ moveCursor txt prevWordPos Nothing
    | isMoveWordR = Just $ moveCursor txt nextWordPos Nothing
    | isMoveLineL = Just $ moveCursor txt (0, tpY) Nothing
    | isMoveLineR = Just $ moveCursor txt (lineLen tpY, tpY) Nothing
    | isSelectAll = Just $ moveCursor txt (0, 0) (Just (totalLines, lineLen (totalLines - 1)))
    | isSelectLeft = Just $ moveCursor txt (tpX - 1, tpY) (Just tp)
    | isSelectRight = Just $ moveCursor txt (tpX + 1, tpY) (Just tp)
    | isSelectUp = Just $ moveCursor txt (tpX, tpY - 1) (Just tp)
    | isSelectDown = Just $ moveCursor txt (tpX, tpY + 1) (Just tp)
    | isSelectWordL = Just $ moveCursor txt prevWordPos (Just tp)
    | isSelectWordR = Just $ moveCursor txt nextWordPos (Just tp)
    | isSelectLineL = Just $ moveCursor txt (0, tpY) (Just tp)
    | isSelectLineR = Just $ moveCursor txt (lineLen tpY, tpY) (Just tp)
    | isDeselectLeft = Just $ moveCursor txt minTpSel Nothing
    | isDeselectRight = Just $ moveCursor txt maxTpSel Nothing
    | isDeselectUp = Just $ moveCursor txt (tpX, tpY - 1) Nothing
    | isDeselectDown = Just $ moveCursor txt (tpX, tpY + 1) Nothing
    | otherwise = Nothing
    where
      txt = currText
      txtLen = T.length txt
      tp@(tpX, tpY) = _tasCursorPos state
      selStart = _tasSelStart state
      (minTpSel, maxTpSel)
        | swap tp <= swap (fromJust selStart) = (tp, fromJust selStart)
        | otherwise = (fromJust selStart, tp)
      emptySel = isNothing selStart
      line idx
        | length textLines > idx = Seq.index textLines idx ^. L.text
        | otherwise = ""
      lineLen = T.length . line
      totalLines = length textLines
      --(part1, part2) = T.splitAt currPos currText
      --currSelVal = fromMaybe 0 selStart
      activeSel = isJust selStart
      --minTpXSel = min tpX currSelVal
      --maxTpXSel = max tpX currSelVal
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
      isMoveUp = isMove && not activeSel && isUp
      isMoveDown = isMove && not activeSel && isDown
      isSelectAll = isAllMod && isKeyA code
      isSelectLeft = isSelect && isLeft
      isSelectRight = isSelect && isRight
      isSelectUp = isSelect && isUp
      isSelectDown = isSelect && isDown
      isSelectWordL = isSelectWord && isLeft
      isSelectWordR = isSelectWord && isRight
      isSelectLineL = (isSelectLine && isLeft) || (isShift && isHome)
      isSelectLineR = (isSelectLine && isRight) || (isShift && isEnd)
      isDeselectLeft = isMove && activeSel && isLeft
      isDeselectRight = isMove && activeSel && isRight
      isDeselectUp = isMove && activeSel && isUp
      isDeselectDown = isMove && activeSel && isDown
      removeCharL
        | tpX > 0 = replaceText state (Just (tpX - 1, tpY)) ""
        | otherwise = replaceText state (Just (lineLen (tpY - 1), tpY - 1)) ""
      removeWordL = replaceText state (Just prevWordPos) ""
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
        vcY = restrictValue 0 (nlines - 1) cY
        vcX = restrictValue 0 (lineLen tpY) cX
        ncX = restrictValue 0 (lineLen vcY) cX
        sameX = vcX == tpX
        sameY = vcY == tpY
        result
          | sameY && cX < 0 && vcY == 0 = (0, 0)
          | sameY && cX < 0 && vcY > 0 = (lineLen (vcY - 1) + cX + 1, vcY - 1)
          | sameY && cX > lineLen vcY && vcY < nlines - 1 = (cX - lineLen vcY - 1, vcY + 1)
          | sameX && cX > lineLen vcY = (min cX (lineLen vcY), vcY)
          | otherwise = (ncX, vcY)

  handleEvent wenv node target evt = case evt of
    KeyAction mod code KeyPressed
--      | isKeyboardCopy wenv evt
--          -> Just $ resultReqs node [SetClipboard (ClipboardText selectedText)]
--      | isKeyboardPaste wenv evt
--          -> Just $ resultReqs node [GetClipboard widgetId]
--      | isKeyboardCut wenv evt -> cutTextRes wenv node
--      | isKeyboardUndo wenv evt -> moveHistory wenv node state config (-1)
--      | isKeyboardRedo wenv evt -> moveHistory wenv node state config 1
      | otherwise -> fmap handleKeyRes keyRes where
          keyRes = handleKeyPress wenv mod code
          handleKeyRes (newText, newPos, newSel) = result where
            newState = (stateFromText wenv node state newText) {
              _tasCursorPos = newPos,
              _tasSelStart = newSel
            }
            newNode = node
              & L.widget .~ makeTextArea wdata config newState
            result = resultWidget newNode
    TextInput newText -> Just result where
      result = insertText wenv node newText
    Focus prev -> Just result where
      viewport = node ^. L.info . L.viewport
      reqs = [RenderEvery widgetId caretMs Nothing, StartTextInput viewport]
      result = resultReqs node reqs
    Blur next -> Just result where
      reqs = [RenderStop widgetId, StopTextInput]
      result = resultReqs node reqs
    _ -> Nothing
    where
      widgetId = node ^. L.info . L.widgetId

  insertText wenv node addedText = result where
    currSel = _tasSelStart state
    (newText, newPos, newSel) = replaceText state currSel addedText
    newState = (stateFromText wenv node state newText) {
      _tasCursorPos = newPos,
      _tasSelStart = newSel
    }
    newNode = node
      & L.widget .~ makeTextArea wdata config newState
    result = resultWidget newNode

  getSizeReq wenv node = sizeReq where
    sizeReq = (minWidth 100, minHeight 100)

  render wenv node renderer =
    drawInTranslation renderer offset $ do
      when selRequired $
        forM_ selRects $ \rect ->
          drawRect renderer rect (Just selColor) Nothing

      forM_ textLines (drawTextLine renderer style)

      when caretRequired $
        drawRect renderer caretRect (Just caretColor) Nothing
    where
      style = activeStyle wenv node
      contentArea = getContentArea style node
      ts = _weTimestamp wenv
      offset = Point (contentArea ^. L.x) (contentArea ^. L.y)
      caretRequired = isNodeFocused wenv node && ts `mod` 1000 < 500
      caretColor = styleFontColor style
      caretRect = getCaretRect state contentArea
      selRequired = isJust (_tasSelStart state)
      selColor = styleHlColor style
      selRects = getSelectionRects state contentArea

getCaretRect :: TextAreaState -> Rect -> Rect
getCaretRect state contentArea = caretRect where
  Rect cx cy cw ch = contentArea 
  (cursorX, cursorY) = _tasCursorPos state
  TextMetrics _ desc lineh = _tasTextMetrics state
  textLines = _tasTextLines state
  (lineRect, glyphs) = case Seq.lookup cursorY textLines of
    Just tl -> (tl ^. L.rect, tl ^. L.glyphs)
    Nothing -> (def, Seq.empty)
  Rect tx ty _ _ = lineRect
  caretPos
    | cursorX == 0 = 0
    | cursorX == length glyphs = _glpXMax (Seq.index glyphs (cursorX - 1))
    | otherwise = _glpXMin (Seq.index glyphs cursorX)
  caretX = max 0 $ min (cx + cw - caretW) (tx + caretPos)
  caretY = cy + ty + desc
  caretRect = Rect caretX caretY caretW lineh

getSelectionRects :: TextAreaState -> Rect -> [Rect]
getSelectionRects state contentArea = rects where
  currPos = _tasCursorPos state
  currSel = fromMaybe def (_tasSelStart state)
  TextMetrics _ desc lineh = _tasTextMetrics state
  textLines = _tasTextLines state
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
  makeRect cx1 cx2 cy = Rect rx ry rw rh where
    rx = glyphPos cx1 cy
    rw = glyphPos cx2 cy - rx
    ry = fromIntegral cy * lineh
    rh = lineh
  rects
    | selY1 == selY2 = [makeRect selX1 selX2 selY1]
    | otherwise = begin : middle ++ end where
      begin = makeRect selX1 (lineLen selY1) selY1
      middleLines = Seq.drop (selY1 + 1) . Seq.take selY2 $ textLines
      middle = toList (view L.rect <$> middleLines)
      end = [makeRect 0 selX2 selY2]

stateFromText
  :: WidgetEnv s e -> WidgetNode s e -> TextAreaState -> Text -> TextAreaState
stateFromText wenv node state text = newState where
  style = activeStyle wenv node
  renderer = wenv ^. L.renderer
  newTextMetrics = getTextMetrics wenv style
  newTextLines = fitTextToWidth renderer style maxNumericValue KeepSpaces text
  newState = state {
    _tasText = text,
    _tasTextMetrics = newTextMetrics,
    _tasTextStyle = style ^. L.text,
    _tasTextLines = newTextLines
  }

textFromState :: Seq TextLine -> Text
textFromState textLines = T.unlines lines where
  lines = toList (view L.text <$> textLines)

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
--  (selX1, selY1) = min currPos currSel
--  (selX2, selY2) = max currPos currSel
  prevLines = Seq.take selY1 oldLines
  postLines = Seq.drop (selY2 + 1) oldLines
  linePre = T.take selX1 (Seq.index oldLines selY1)
  lineSuf = T.drop selX2 (Seq.index oldLines selY2)
  newLines = Seq.fromList (T.lines addText)
  (newX, newY, midLines)
    | length newLines <= 1 = (T.length (linePre <> addText), selY1, singleLine)
    | otherwise = (T.length (end <> lineSuf), selY1 + length newLines - 1, multiLine)
    where
      singleLine = Seq.singleton $ linePre <> addText <> lineSuf
      begin = Seq.index newLines 0
      middle = Seq.drop 1 $ Seq.take (length newLines - 1) newLines
      end = Seq.index newLines (length newLines - 1)
      multiLine = (linePre <> begin) :<| (middle :|> (end <> lineSuf))
  newText = T.unlines . toList $ prevLines <> midLines <> postLines
  result = (newText, (newX, newY), Nothing)

delim :: Char -> Bool
delim c = c `elem` [' ', '.', ',', '/', '-', ':']
