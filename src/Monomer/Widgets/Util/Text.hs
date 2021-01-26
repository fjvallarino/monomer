{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Monomer.Widgets.Util.Text (
  getTextMetrics,
  getTextSize,
  getTextSize_,
  getTextRect,
  getTextGlyphs,
  getTextLinesSize,
  getGlyphsMin,
  getGlyphsMax,
  getGlyphsWidth,
  moveTextLines,
  drawTextLine,
  fitTextToRect,
  fitTextToWidth,
  alignTextLines
) where

import Control.Lens ((&), (+~))
import Data.Default
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text (Text)

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Core
import Monomer.Graphics
import Monomer.Widgets.Util.Style

import Monomer.Lens as L

type GlyphGroup = Seq GlyphPos

getTextMetrics :: WidgetEnv s e -> StyleState -> TextMetrics
getTextMetrics wenv style = textMetrics where
  renderer = _weRenderer wenv
  !textMetrics = computeTextMetrics renderer font fontSize
  font = styleFont style
  fontSize = styleFontSize style

getTextSize :: WidgetEnv s e -> StyleState -> Text -> Size
getTextSize wenv style !text = newSize where
  newSize = getTextSize_ wenv style SingleLine KeepSpaces Nothing text

getTextSize_
  :: WidgetEnv s e
  -> StyleState
  -> TextMode
  -> TextTrim
  -> Maybe Double
  -> Text
  -> Size
getTextSize_ wenv style mode trim mwidth text = newSize where
  font = styleFont style
  fontSize = styleFontSize style
  !metrics = computeTextMetrics (_weRenderer wenv) font fontSize
  width = fromMaybe maxNumericValue mwidth
  textLinesW = fitTextToWidth wenv style width trim text
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

moveTextLines :: Double -> Double -> Seq TextLine -> Seq TextLine
moveTextLines offsetX offsetY textLines = newTextLines where
  moveTextLine tl = tl
    & L.rect . L.x +~ offsetX
    & L.rect . L.y +~ offsetY
  newTextLines = fmap moveTextLine textLines

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
  -> TextTrim
  -> Rect
  -> Text
  -> Seq TextLine
fitTextToRect wenv style overflow mode trim !rect !text = textLines where
  Rect cx cy cw ch = rect
  textLinesW = fitTextToWidth wenv style cw trim text
  fittedLines = fitTextLinesToH wenv style overflow cw ch textLinesW
  textLines
    | mode == MultiLine = fittedLines
    | otherwise = Seq.take 1 fittedLines

alignTextLines :: StyleState -> Rect -> Seq TextLine -> Seq TextLine
alignTextLines style parentRect textLines = newTextLines where
  Rect _ py _ ph = parentRect
  Size _ th = getTextLinesSize textLines
  alignH = styleTextAlignH style
  alignV = styleTextAlignV style
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
  | overflow == Ellipsis && h >= g1H + g2H = g1 :<| rest
  | overflow == Ellipsis && h >= g1H = Seq.singleton ellipsisG1
  | overflow == ClipText && h >= g1H = g1 :<| rest
  where
    g1H = _sH (_tlSize g1)
    g2H = _sH (_tlSize g2)
    newH = h - g1H
    rest = fitTextLinesToH wenv style overflow w newH (g2 :<| gs)
    ellipsisG1 = addEllipsisToTextLine wenv style w g1
fitTextLinesToH wenv style overflow w h (g :<| gs)
  | h > 0 = Seq.singleton newG
  | otherwise = Empty
  where
    gW = _sW (_tlSize g)
    newG
      | overflow == Ellipsis && w < gW = addEllipsisToTextLine wenv style w g
      | otherwise = g

fitTextToWidth
  :: WidgetEnv s e
  -> StyleState
  -> Double
  -> TextTrim
  -> Text
  -> Seq TextLine
fitTextToWidth wenv style width trim text = resultLines where
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
  -> TextTrim
  -> Text
  -> Seq TextLine
fitSingleTextToW wenv font fontSize metrics top width trim text = result where
  spaces = T.replicate 4 " "
  -- Temporary solution. It should return empty line, not one with space
  newText
    | text /= "" = T.replace "\t" spaces text
    | otherwise = " "
  !glyphs = computeGlyphsPos (_weRenderer wenv) font fontSize newText
  -- Do not break line on trailing spaces, since they are removed in next step
  -- In the case of KeepSpaces, lines with only spaces can be generated
  keepTailSpaces = trim == TrimSpaces
  groups = fitGroups (splitGroups glyphs) width keepTailSpaces
  resetGroups
    | trim == TrimSpaces = fmap (resetGlyphs . trimGlyphs) groups
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
  Size tw th = textSize
  Size dw dh = getTextSize wenv style "..."
  font = styleFont style
  fontSize = styleFontSize style
  targetW = width - tw
  dropHelper (idx, w) g
    | _glpW g + w <= dw = (idx + 1, _glpW g + w)
    | otherwise = (idx, w)
  (dropChars, _) = foldl' dropHelper (0, targetW) (Seq.reverse textGlyphs)
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
fitGroups (g :<| gs) !width !keepTailSpaces = currentLine <| extraLines where
  gW = getGlyphsWidth g
  gMax = getGlyphsMax g
  extraGroups = fitExtraGroups gs (width - gW) gMax keepTailSpaces
  (lineGroups, remainingGroups) = extraGroups
  currentLine = g <> lineGroups
  extraLines = fitGroups remainingGroups width keepTailSpaces

fitExtraGroups
  :: Seq GlyphGroup
  -> Double
  -> Double
  -> Bool
  -> (Seq GlyphPos, Seq GlyphGroup)
fitExtraGroups Empty _ _ _ = (Empty, Empty)
fitExtraGroups (g :<| gs) !width !prevGMax !keepTailSpaces
  | gW + wDiff <= width || keepSpace = (g <> newFit, newRest)
  | otherwise = (Empty, g :<| gs)
  where
    gW = getGlyphsWidth g
    gMin = getGlyphsMin g
    gMax = getGlyphsMax g
    wDiff = gMin - prevGMax
    remWidth = width - (gW + wDiff)
    keepSpace = keepTailSpaces && isSpaceGroup g
    (newFit, newRest) = fitExtraGroups gs remWidth gMax keepTailSpaces

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
