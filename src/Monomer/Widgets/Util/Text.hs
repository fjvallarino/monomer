{-# LANGUAGE BangPatterns #-}

module Monomer.Widgets.Util.Text (
  TextMode(..),
  TextLine(..),
  getTextMetrics,
  getTextSize,
  getTextSize_,
  getTextRect,
  getTextGlyphs,
  getGlyphsMin,
  getGlyphsMax,
  getGlyphsWidth,
  drawTextLine,
  fitTextToRect
) where

import Data.Default
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq(..), (><), (<|), (|>))
import Data.Text (Text)

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Core
import Monomer.Graphics
import Monomer.Widgets.Util.Misc
import Monomer.Widgets.Util.Style

type GlyphGroup = Seq GlyphPos

data TextMode
  = SingleLine
  | MultiLine
  deriving (Eq, Show)

data TextLine = TextLine {
  _tlText :: Text,
  _tlSize :: Size,
  _tlRect :: Rect,
  _tlGlyphs :: Seq GlyphPos,
  _tlMetrics :: TextMetrics
} deriving (Eq, Show)

getTextMetrics :: WidgetEnv s e -> StyleState -> TextMetrics
getTextMetrics wenv style = textMetrics where
  renderer = _weRenderer wenv
  !textMetrics = computeTextMetrics renderer font fontSize
  font = styleFont style
  fontSize = styleFontSize style

getTextSize :: WidgetEnv s e -> StyleState -> Text -> Size
getTextSize wenv style !text = newSize where
  newSize = getTextSize_ wenv style SingleLine False Nothing text

getTextSize_
  :: WidgetEnv s e
  -> StyleState
  -> TextMode
  -> Bool
  -> Maybe Double
  -> Text
  -> Size
getTextSize_ wenv style mode trim mwidth text = newSize where
  font = styleFont style
  fontSize = styleFontSize style
  !metrics = computeTextMetrics (_weRenderer wenv) font fontSize
  width = fromMaybe maxNumericValue mwidth
  textLinesW = fitTextToW wenv style width trim text
  textLines
    | mode == SingleLine = Seq.take 1 textLinesW
    | otherwise = textLinesW
  newSize
    | not (Seq.null textLines) = getTextLinesSize textLines
    | otherwise = Size 0 (_txmLineH metrics)

getTextRect :: WidgetEnv s e -> StyleState -> Rect -> Align -> Text -> Rect
getTextRect wenv style !rect !align !text = textRect where
  renderer = _weRenderer wenv
  font = styleFont style
  fontSize = styleFontSize style
  !textRect = computeTextRect renderer rect font fontSize align text

getTextGlyphs :: WidgetEnv s e -> StyleState -> Text -> Seq GlyphPos
getTextGlyphs wenv style !text = glyphs where
  font = styleFont style
  fontSize = styleFontSize style
  !glyphs = computeGlyphsPos (_weRenderer wenv) font fontSize text

getGlyphsMin :: Seq GlyphPos -> Double
getGlyphsMin Empty = 0
getGlyphsMin (g :<| gs) = _glpXMin g

getGlyphsMax :: Seq GlyphPos -> Double
getGlyphsMax Empty = 0
getGlyphsMax (gs :|> g) = _glpXMax g

getGlyphsWidth :: Seq GlyphPos -> Double
getGlyphsWidth glyphs = getGlyphsMax glyphs - getGlyphsMin glyphs

getTextLinesSize :: Seq TextLine -> Size
getTextLinesSize textLines = size where
  width = maximum (fmap (_sW . _tlSize) textLines)
  height = sum (fmap (_sH . _tlSize) textLines)
  size
    | Seq.null textLines = def
    | otherwise = Size width height

drawTextLine :: Renderer -> StyleState -> TextLine -> IO ()
drawTextLine renderer style textLine = do
  setFillColor renderer fontColor
  renderText renderer point font fontSize text
  where
    TextLine text size rect glyphs metrics = textLine
    TextMetrics asc desc lineH = metrics
    Rect tx ty tw th = rect
    font = styleFont style
    fontSize = styleFontSize style
    fontColor = styleFontColor style
    point = Point tx (ty + th + desc)

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
  !metrics = computeTextMetrics (_weRenderer wenv) font fontSize
  lineH = _txmLineH metrics
  helper acc line = (cLines <> newLines, newTop) where
    (cLines, cTop) = acc
    newLines = fitSingleTextToW wenv font fontSize metrics cTop width trim line
    newTop = cTop + fromIntegral (Seq.length newLines) * lineH
  (resultLines, _) = foldl' helper (Empty, 0) (T.lines text)

fitSingleTextToW
  :: WidgetEnv s e
  -> Font
  -> FontSize
  -> TextMetrics
  -> Double
  -> Double
  -> Bool
  -> Text
  -> Seq TextLine
fitSingleTextToW wenv font fontSize metrics top width trim text = result where
  spaces = "    "
  newText = T.replace "\t" spaces text
  !glyphs = computeGlyphsPos (_weRenderer wenv) font fontSize newText
  keepTailSpaces = trim -- Do not break on trailing spaces (removed in next step)
  groups = fitGroups (splitGroups glyphs) width keepTailSpaces
  resetGroups
    | trim = fmap (resetGlyphs . trimGlyphs) groups
    | otherwise = fmap resetGlyphs groups
  result = Seq.mapWithIndex (buildTextLine metrics top) resetGroups

buildTextLine :: TextMetrics -> Double -> Int -> Seq GlyphPos -> TextLine
buildTextLine metrics top idx glyphs = textLine where
  lineH = _txmLineH metrics
  x = 0
  y = top + fromIntegral idx * lineH
  width = getGlyphsWidth glyphs
  height = lineH
  text = T.pack . reverse $ foldl' (\ac g -> _glpGlyph g : ac) [] glyphs
  textLine = TextLine {
    _tlText = text,
    _tlSize = Size width height,
    _tlRect = Rect x y width height,
    _tlGlyphs = glyphs,
    _tlMetrics = metrics
  }

addEllipsisToTextLine
  :: WidgetEnv s e
  -> StyleState
  -> Double
  -> TextLine
  -> TextLine
addEllipsisToTextLine wenv style width textLine = newTextLine where
  TextLine text textSize textRect textGlyphs textMetrics = textLine
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
  !newGlyphs = computeGlyphsPos (_weRenderer wenv) font fontSize newText
  newW = getGlyphsWidth newGlyphs
  newTextLine = TextLine {
    _tlText = newText,
    _tlSize = textSize { _sW = newW },
    _tlRect = textRect { _rW = newW },
    _tlGlyphs = newGlyphs,
    _tlMetrics = textMetrics
  }

fitGroups :: Seq GlyphGroup -> Double -> Bool -> Seq GlyphGroup
fitGroups Empty _ _ = Empty
fitGroups (g :<| gs) width keepTailSpaces = currentLine <| extraLines where
  extraGroups = fitExtraGroups gs (width - getGlyphsWidth g) keepTailSpaces
  (lineGroups, remainingGroups) = extraGroups
  currentLine = g <> lineGroups
  extraLines = fitGroups remainingGroups width keepTailSpaces

fitExtraGroups
  :: Seq GlyphGroup
  -> Double
  -> Bool
  -> (Seq GlyphPos, Seq GlyphGroup)
fitExtraGroups Empty _ _ = (Empty, Empty)
fitExtraGroups (g :<| gs) width keepTailSpaces
  | gw <= width || keepSpace = (g <> newFit, newRest)
  | otherwise = (Empty, g :<| gs)
  where
    gw = getGlyphsWidth g
    keepSpace = keepTailSpaces && isSpaceGroup g
    (newFit, newRest) = fitExtraGroups gs (width - gw) keepTailSpaces

isSpaceGroup :: Seq GlyphPos -> Bool
isSpaceGroup Empty = False
isSpaceGroup (g :<| gs) = isSpace (_glpGlyph g)

splitGroups :: Seq GlyphPos -> Seq GlyphGroup
splitGroups Empty = Empty
splitGroups glyphs = group <| splitGroups rest where
  g :<| gs = glyphs
  groupWordFn = not . isWordDelimiter . _glpGlyph
  (group, rest)
    | isWordDelimiter (_glpGlyph g) = (Seq.singleton g, gs)
    | otherwise = Seq.spanl groupWordFn glyphs

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

trimGlyphs :: Seq GlyphPos -> Seq GlyphPos
trimGlyphs glyphs = newGlyphs where
  isSpaceGlyph g = _glpGlyph g == ' '
  newGlyphs = Seq.dropWhileL isSpaceGlyph $ Seq.dropWhileR isSpaceGlyph glyphs

isWordDelimiter :: Char -> Bool
isWordDelimiter = (== ' ')

isSpace :: Char -> Bool
isSpace = (== ' ')
