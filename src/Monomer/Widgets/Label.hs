module Monomer.Widgets.Label (
  label,
  label_
) where

import Debug.Trace

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Data.Default
import Data.Maybe
import Data.List (foldl')
import Data.Sequence (Seq(..), (<|))
import Data.Text (Text)

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Widgets.Single

import qualified Monomer.Lens as L

data LabelCfg = LabelCfg {
  _lscTextOverflow :: Maybe TextOverflow,
  _lscMultiLine :: Maybe Bool
}

instance Default LabelCfg where
  def = LabelCfg {
    _lscTextOverflow = Nothing,
    _lscMultiLine = Nothing
  }

instance Semigroup LabelCfg where
  (<>) l1 l2 = LabelCfg {
    _lscTextOverflow = _lscTextOverflow l2 <|> _lscTextOverflow l1,
    _lscMultiLine = _lscMultiLine l2 <|> _lscMultiLine l1
  }

instance Monoid LabelCfg where
  mempty = def

instance OnTextOverflow LabelCfg where
  textEllipsis = def {
    _lscTextOverflow = Just Ellipsis
  }
  textClip = def {
    _lscTextOverflow = Just ClipText
  }

data LabelState = LabelState {
  _lstCaption :: Text,
  _lstTextLines :: Seq TextLine
} deriving (Eq, Show)

label :: Text -> WidgetInstance s e
label caption = label_ caption def

label_ :: Text -> [LabelCfg] -> WidgetInstance s e
label_ caption configs = defaultWidgetInstance "label" widget where
  config = mconcat configs
  state = LabelState caption Seq.Empty
  widget = makeLabel config state

makeLabel :: LabelCfg -> LabelState -> Widget s e
makeLabel config state = widget where
  widget = createSingle def {
    singleGetBaseStyle = getBaseStyle,
    singleMerge = merge,
    singleGetState = makeState state,
    singleGetSizeReq = getSizeReq,
    singleResize = resize,
    singleRender = render
  }

  textOverflow = fromMaybe Ellipsis (_lscTextOverflow config)
  LabelState caption textLines = state

  getBaseStyle wenv inst = Just style where
    style = collectTheme wenv L.labelStyle

  merge wenv oldState inst = resultWidget newInstance where
    newState = fromMaybe state (useState oldState)
    newInstance = inst {
      _wiWidget = makeLabel config newState
    }

  getSizeReq wenv inst = sizeReq where
    style = activeStyle wenv inst
    Size w h = getTextSize wenv style caption
    factor = 1
    sizeReq = (FlexSize w factor, FixedSize h)

--  resize wenv viewport renderArea inst = newInst where
--    style = activeStyle wenv inst
--    contentArea = fromMaybe def (removeOuterBounds style renderArea)
--    (newCaptionFit, _) = case textOverflow of
--      Ellipsis -> fitText wenv style contentArea caption
--      _ -> (caption, def)
--    newWidget
--      | captionFit == newCaptionFit = _wiWidget inst
--      | otherwise = makeLabel config (LabelState caption newCaptionFit)
--    newInst = inst {
--      _wiWidget = newWidget
--    }

  resize wenv viewport renderArea inst = newInst where
    style = activeStyle wenv inst
    contentArea = fromMaybe def (removeOuterBounds style renderArea)
    Rect cx cy cw ch = contentArea
    alignH = styleTextAlignH style
    alignV = styleTextAlignV style
    fontColor = styleFontColor style
    textMode = MultiLine -- SingleLine -- MultiLine
    newTextLines = fitTextToRect wenv style textOverflow textMode True contentArea caption
    newWidget = makeLabel config (LabelState caption newTextLines)
    newInst = inst {
      _wiWidget = newWidget
    }

  render renderer wenv inst =
    forM_ textLines (drawTextLine renderer style textMetrics)
    where
      style = activeStyle wenv inst
      textMetrics = getTextMetrics wenv style

drawTextLine :: Renderer -> StyleState -> TextMetrics -> TextLine -> IO ()
drawTextLine renderer style textMetrics textLine = do
  setFillColor renderer fontColor
  renderText renderer point font fontSize text
  where
    TextMetrics asc desc lineH = textMetrics
    TextLine text size rect glyphs = textLine
    Rect tx ty tw th = rect
    font = styleFont style
    fontSize = styleFontSize style
    fontColor = styleFontColor style
    point = Point tx (ty + th + desc)

type GlyphGroup = Seq GlyphPos

data TextMode
  = SingleLine
  | MultiLine
  deriving (Eq, Show)

data TextLine = TextLine {
  _tlText :: Text,
  _tlSize :: Size,
  _tlRect :: Rect,
  _tlGlyphs :: Seq GlyphPos
} deriving (Eq, Show)

fitTextToRect
  :: WidgetEnv s e
  -> StyleState
  -> TextOverflow
  -> TextMode
  -> Bool
  -> Rect
  -> Text
  -> Seq TextLine
fitTextToRect wenv style overflow mode trim rect text = newTextLines where
  Rect cx cy cw ch = rect
  alignH = styleTextAlignH style
  alignV = styleTextAlignV style
  textLinesW = fitTextToW wenv style cw trim text
  textLinesLen = Seq.length textLinesW
  firstLine = Seq.take 1 textLinesW
  firstLineW = _sW (getTextLinesSize firstLine)
  needsEllipsis = overflow == Ellipsis && (textLinesLen > 1 || firstLineW > cw)
  textLines
    | mode == MultiLine = fitTextLinesToH wenv style overflow cw ch textLinesW
    | needsEllipsis = addEllipsisToTextLine wenv style cw <$> firstLine
    | otherwise = firstLine
  newTextLines = alignTextLines rect alignH alignV textLines

alignTextLines :: Rect -> AlignH -> AlignV -> Seq TextLine -> Seq TextLine
alignTextLines parentRect alignH alignV textLines = newTextLines where
  Rect _ py _ ph = parentRect
  Size _ th = getTextLinesSize textLines
  alignOffsetY = case alignV of
    ATop -> 0
    AMiddle -> (ph - th) / 2
    ABottom -> ph - th
  offsetY = py + alignOffsetY
  newTextLines = fmap (alignTextLine parentRect offsetY alignH) textLines

alignTextLine :: Rect -> Double -> AlignH -> TextLine -> TextLine
alignTextLine parentRect offsetY alignH textLine = newTextLine where
  Rect px _ pw _ = parentRect
  Rect tx ty tw th = _tlRect textLine
  alignOffsetX = case alignH of
    ALeft -> 0
    ACenter -> (pw - tw) / 2
    ARight -> pw - tw
  offsetX = px + alignOffsetX
  newTextLine = textLine {
    _tlRect = Rect (tx + offsetX) (ty + offsetY) tw th
  }

getTextLinesSize :: Seq TextLine -> Size
getTextLinesSize textLines = size where
  width = maximum (fmap (_sW . _tlSize) textLines)
  height = sum (fmap (_sH . _tlSize) textLines)
  size
    | Seq.null textLines = def
    | otherwise = Size width height

fitTextLinesToH
  :: WidgetEnv s e
  -> StyleState
  -> TextOverflow
  -> Double
  -> Double
  -> Seq TextLine
  -> Seq TextLine
fitTextLinesToH wenv style overflow w h Empty = Empty
fitTextLinesToH wenv style overflow w h (g1 :<| g2 :<| gs)
  | h >= g1H + g2H = g1 :<| g2 :<| rest
  | h >= g1H = Seq.singleton newG1
  | otherwise = Empty
  where
    g1H = _sH (_tlSize g1)
    g2H = _sH (_tlSize g2)
    newH = h - g1H - g2H
    rest = fitTextLinesToH wenv style overflow w newH gs
    newG1 = case overflow of
      Ellipsis -> addEllipsisToTextLine wenv style w g1
      _ -> g1
fitTextLinesToH wenv style overflow w h (g :<| gs)
  | h < _sH (_tlSize g) = Empty
  | otherwise = Seq.singleton g

fitTextToW
  :: WidgetEnv s e
  -> StyleState
  -> Double
  -> Bool
  -> Text
  -> Seq TextLine
fitTextToW wenv style width trim text = resultLines where
  font = styleFont style
  fontSize = styleFontSize style
  metrics = computeTextMetrics (_weRenderer wenv) font fontSize
  lineH = _txhLineH metrics
  helper acc line = (currLines <> newLines, newTop) where
    (currLines, currTop) = acc
    newLines = fitSingleTextToW wenv font fontSize currTop width lineH trim line
    newTop = currTop + fromIntegral (Seq.length newLines) * lineH
  (resultLines, _) = foldl' helper (Empty, 0) (T.lines text)

fitSingleTextToW
  :: WidgetEnv s e
  -> Font
  -> FontSize
  -> Double
  -> Double
  -> Double
  -> Bool
  -> Text
  -> Seq TextLine
fitSingleTextToW wenv font fontSize top width lineH trim text = result where
  spaces = "    "
  newText = T.replace "\t" spaces text
  glyphs = computeGlyphsPos (_weRenderer wenv) font fontSize newText
  groups = fitGroups (splitGroups glyphs) width
  resetGroups
    | trim = fmap (resetGlyphs . trimGlyphs) groups
    | otherwise = fmap resetGlyphs groups
  result = Seq.mapWithIndex (buildTextLine top lineH) resetGroups

trimGlyphs :: Seq GlyphPos -> Seq GlyphPos
trimGlyphs glyphs = newGlyphs where
  isSpaceGlyph g = _glpGlyph g == ' '
  newGlyphs = Seq.dropWhileL isSpaceGlyph $ Seq.dropWhileR isSpaceGlyph glyphs

buildTextLine :: Double -> Double -> Int -> Seq GlyphPos -> TextLine
buildTextLine top lineH idx glyphs = textLine where
  x = 0
  y = top + fromIntegral idx * lineH
  width = glyphSeqLen glyphs
  height = lineH
  text = T.pack . reverse $ foldl' (\ac g -> _glpGlyph g : ac) [] glyphs
  textLine = TextLine {
    _tlText = text,
    _tlSize = Size width height,
    _tlRect = Rect x y width height,
    _tlGlyphs = glyphs
  }

addEllipsisToTextLine
  :: WidgetEnv s e
  -> StyleState
  -> Double
  -> TextLine
  -> TextLine
addEllipsisToTextLine wenv style width textLine = newTextLine where
  TextLine text textSize textRect textGlyphs = textLine
  Size dw dh = getTextSize wenv style "."
  font = styleFont style
  fontSize = styleFontSize style
  textWidth = _sW textSize
  textLen = max 1 $ fromIntegral (T.length text)
  dropHelper (idx, w) g
    | _glpW g + w <= dw = (idx + 1, _glpW g + w)
    | otherwise = (idx, w)
  (dropChars, _) = foldl' dropHelper (0, 0) textGlyphs
  newText = T.dropEnd dropChars text <> "..."
  newGlyphs = computeGlyphsPos (_weRenderer wenv) font fontSize newText
  newW = glyphSeqLen newGlyphs
  newTextLine = TextLine {
    _tlText = newText,
    _tlSize = textSize { _sW = newW },
    _tlRect = textRect { _rW = newW },
    _tlGlyphs = newGlyphs
  }

fitGroups :: Seq GlyphGroup -> Double -> Seq GlyphGroup
fitGroups Empty _ = Empty
fitGroups (g :<| gs) width = currentLine <| extraLines where
  (lineGroups, remainingGroups) = fitExtraGroups gs (width - glyphSeqLen g)
  currentLine = g <> lineGroups
  extraLines = fitGroups remainingGroups width

fitExtraGroups :: Seq GlyphGroup -> Double -> (Seq GlyphPos, Seq GlyphGroup)
fitExtraGroups Empty _ = (Empty, Empty)
fitExtraGroups (g :<| gs) width
  | gw <= width = (g <> newFit, newRest)
  | otherwise = (Empty, g :<| gs)
  where
    gw = glyphSeqLen g
    (newFit, newRest) = fitExtraGroups gs (width - gw)

splitGroups :: Seq GlyphPos -> Seq GlyphGroup
splitGroups Empty = Empty
splitGroups glyphs = group <| splitGroups rest where
  g :<| gs = glyphs
  groupWordFn = not . isWordDelimiter . _glpGlyph
  (group, rest)
    | isWordDelimiter (_glpGlyph g) = (Seq.singleton g, gs)
    | otherwise = Seq.spanl groupWordFn glyphs

glyphSeqLen :: Seq GlyphPos -> Double
glyphSeqLen glyphs = getGlyphsMax glyphs - getGlyphsMin glyphs

resetGlyphs :: Seq GlyphPos -> Seq GlyphPos
resetGlyphs Empty = Empty
resetGlyphs gs@(g :<| _) = resetGlyphsPos gs (_glpXMin g)

resetGlyphsPos :: Seq GlyphPos -> Double -> Seq GlyphPos
resetGlyphsPos Empty _ = Empty
resetGlyphsPos (g :<| gs) offset = newG <| resetGlyphsPos gs offset where
  newG = g {
    _glpXMin = _glpXMin g - offset,
    _glpXMax = _glpXMax g - offset
  }

isWordDelimiter :: Char -> Bool
isWordDelimiter = (== ' ')
