{-|
Module      : Monomer.Graphics.Text
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions for calculating text size.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Graphics.Text (
  calcTextSize,
  calcTextSize_,
  getTextLinesSize,
  fitTextToSize,
  fitTextToWidth,
  alignTextLines,
  moveTextLines,
  getGlyphsMin,
  getGlyphsMax
) where

import Control.Lens ((&), (^.), (^?), (+~), ix, non)
import Data.Default
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text (Text)

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Common
import Monomer.Core.StyleTypes
import Monomer.Core.StyleUtil
import Monomer.Graphics.Types
import Monomer.Helper

import qualified Monomer.Common.Lens as L
import qualified Monomer.Graphics.Lens as L

type GlyphGroup = Seq GlyphPos

-- | Returns the size a given text an style will take.
calcTextSize
  :: FontManager   -- ^ The font manager.
  -> StyleState    -- ^ The style.
  -> Text          -- ^ The text to calculate.
  -> Size          -- ^ The calculated size.
calcTextSize fontMgr style !text = size where
  size = calcTextSize_ fontMgr style SingleLine KeepSpaces Nothing Nothing text

-- | Returns the size a given text an style will take.
calcTextSize_
  :: FontManager   -- ^ The font manager.
  -> StyleState    -- ^ The style.
  -> TextMode      -- ^ Single or multiline.
  -> TextTrim      -- ^ Whether to trim or keep spaces.
  -> Maybe Double  -- ^ Optional max width (needed for multiline).
  -> Maybe Int     -- ^ Optional max lines.
  -> Text          -- ^ The text to calculate.
  -> Size          -- ^ The calculated size.
calcTextSize_ fontMgr style mode trim mwidth mlines text = newSize where
  font = styleFont style
  fontSize = styleFontSize style
  !metrics = computeTextMetrics fontMgr font fontSize
  width = fromMaybe maxNumericValue mwidth
  textLinesW = fitTextToWidth fontMgr style width trim text
  textLines
    | mode == SingleLine = Seq.take 1 textLinesW
    | isJust mlines = Seq.take (fromJust mlines) textLinesW
    | otherwise = textLinesW
  newSize
    | not (Seq.null textLines) = getTextLinesSize textLines
    | otherwise = Size 0 (_txmLineH metrics)

{-|
Fits the given text to a determined size, splitting on multiple lines as needed.
Since the function returns glyphs that may be partially visible, the text can
overflow vertically or horizontally and a scissor is needed. The rectangles are
returned with zero offset (i.e., x = 0 and first line y = 0), and a translation
transform is needed when rendering.
-}
fitTextToSize
  :: FontManager   -- ^ The font manager.
  -> StyleState    -- ^ The style.
  -> TextOverflow  -- ^ Whether to clip or use ellipsis.
  -> TextMode      -- ^ Single or multiline.
  -> TextTrim      -- ^ Whether to trim or keep spaces.
  -> Maybe Int     -- ^ Optional max lines.
  -> Size          -- ^ The bounding size.
  -> Text          -- ^ The text to fit.
  -> Seq TextLine  -- ^ The fitted text lines.
fitTextToSize fontMgr style ovf mode trim mlines !size !text = newLines where
  Size cw ch = size
  font = styleFont style
  fontSize = styleFontSize style
  textMetrics = computeTextMetrics fontMgr font fontSize
  fitW
    | mode == MultiLine = cw
    | otherwise = maxNumericValue
  maxH = case mlines of
    Just maxLines -> min ch (fromIntegral maxLines * textMetrics ^. L.lineH)
    _ -> ch
  textLinesW = fitTextToWidth fontMgr style fitW trim text
  firstLine = Seq.take 1 textLinesW
  isMultiline = mode == MultiLine
  ellipsisReq = ovf == Ellipsis && getTextLinesSize firstLine ^. L.w > cw
  newLines
    | isMultiline = fitLinesToH fontMgr style ovf cw maxH textLinesW
    | ellipsisReq = addEllipsisToTextLine fontMgr style cw <$> firstLine
    | otherwise = clipTextLine fontMgr style trim cw <$> firstLine

-- | Fits a single line of text to the given width, potencially spliting into
-- | several lines.
fitTextToWidth
  :: FontManager   -- ^ The fontManager.
  -> StyleState    -- ^ The style.
  -> Double        -- ^ The maximum width.
  -> TextTrim      -- ^ Whether to trim or keep spaces.
  -> Text          -- ^ The text to calculate.
  -> Seq TextLine  -- ^ The fitted text lines.
fitTextToWidth fontMgr style width trim text = resultLines where
  font = styleFont style
  fSize = styleFontSize style
  fSpcH = styleFontSpaceH style
  fSpcV = styleFontSpaceV style
  lineH = _txmLineH metrics
  !metrics = computeTextMetrics fontMgr font fSize
  fitToWidth = fitLineToW fontMgr font fSize fSpcH fSpcV metrics
  helper acc line = (cLines <> newLines, newTop) where
    (cLines, cTop) = acc
    newLines = fitToWidth cTop width trim line
    vspc = unFontSpace fSpcV
    newTop = cTop + fromIntegral (Seq.length newLines) * (lineH + vspc)
  (resultLines, _) = foldl' helper (Empty, 0) (T.lines text)

-- | Aligns a Seq of TextLines to the given rect.
alignTextLines
  :: StyleState    -- ^ The style.
  -> Rect          -- ^ The bounding rect. Text may overflow.
  -> Seq TextLine  -- ^ The TextLines to align.
  -> Seq TextLine  -- ^ The aligned TextLines.
alignTextLines style parentRect textLines = newTextLines where
  Rect _ py _ ph = parentRect
  Size _ th = getTextLinesSize textLines
  TextMetrics asc _ lineH lowerX = (textLines ^? ix 0) ^. non def . L.metrics
  isSingle = length textLines == 1
  alignH = styleTextAlignH style
  alignV = styleTextAlignV style
  alignOffsetY = case alignV of
    ATTop -> 0
    ATAscender
      | isSingle -> (ph - asc) / 2
    ATLowerX
      | isSingle -> (ph - lowerX) / 2 - (asc - lowerX)
    ATBottom -> ph - th
    ATBaseline -> ph - th
    _ -> (ph - th) / 2 -- ATMiddle
  offsetY = py + alignOffsetY
  newTextLines = fmap (alignTextLine parentRect offsetY alignH) textLines

-- | Moves a Seq of TextLines by the given offset.
moveTextLines
  :: Point         -- ^ The offset.
  -> Seq TextLine  -- ^ The TextLines.
  -> Seq TextLine  -- ^ The displaced TextLines.
moveTextLines (Point offsetX offsetY) textLines = newTextLines where
  moveTextLine tl = tl
    & L.rect . L.x +~ offsetX
    & L.rect . L.y +~ offsetY
  newTextLines = fmap moveTextLine textLines

-- | Gets the minimum x a Seq of Glyphs will use.
getGlyphsMin :: Seq GlyphPos -> Double
getGlyphsMin Empty = 0
getGlyphsMin (g :<| gs) = _glpXMin g

-- | Gets the maximum x a Seq of Glyphs will use.
getGlyphsMax :: Seq GlyphPos -> Double
getGlyphsMax Empty = 0
getGlyphsMax (gs :|> g) = _glpXMax g

-- Helpers
alignTextLine :: Rect -> Double -> AlignTH -> TextLine -> TextLine
alignTextLine parentRect offsetY alignH textLine = newTextLine where
  Rect px _ pw _ = parentRect
  Rect tx ty tw th = _tlRect textLine
  alignOffsetX = case alignH of
    ATLeft -> 0
    ATCenter -> (pw - tw) / 2
    ATRight -> pw - tw
  offsetX = px + alignOffsetX
  newTextLine = textLine {
    _tlRect = Rect (tx + offsetX) (ty + offsetY) tw th
  }

fitLinesToH
  :: FontManager
  -> StyleState
  -> TextOverflow
  -> Double
  -> Double
  -> Seq TextLine
  -> Seq TextLine
fitLinesToH fontMgr style overflow w h Empty = Empty
fitLinesToH fontMgr style overflow w h (g1 :<| g2 :<| gs)
  | overflow == Ellipsis && h >= g1H + g2H = g1 :<| rest
  | overflow == Ellipsis && h >= g1H = Seq.singleton ellipsisG1
  | overflow == ClipText && h >= g1H = g1 :<| rest
  where
    g1H = _sH (_tlSize g1)
    g2H = _sH (_tlSize g2)
    newH = h - g1H
    rest = fitLinesToH fontMgr style overflow w newH (g2 :<| gs)
    ellipsisG1 = addEllipsisToTextLine fontMgr style w g1
fitLinesToH fontMgr style overflow w h (g :<| gs)
  | h > 0 = Seq.singleton newG
  | otherwise = Empty
  where
    gW = _sW (_tlSize g)
    newG
      | overflow == Ellipsis && w < gW = addEllipsisToTextLine fontMgr style w g
      | otherwise = g

fitLineToW
  :: FontManager
  -> Font
  -> FontSize
  -> FontSpace
  -> FontSpace
  -> TextMetrics
  -> Double
  -> Double
  -> TextTrim
  -> Text
  -> Seq TextLine
fitLineToW fontMgr font fSize fSpcH fSpcV metrics top width trim text = res where
  spaces = T.replicate 4 " "
  newText = T.replace "\t" spaces text
  !glyphs = computeGlyphsPos fontMgr font fSize fSpcH newText
  -- Do not break line on trailing spaces, they are removed in the next step
  -- In the case of KeepSpaces, lines with only spaces (empty looking) are valid
  keepTailSpaces = trim == TrimSpaces
  groups = fitGroups (splitGroups glyphs) width keepTailSpaces
  resetGroups
    | trim == TrimSpaces = fmap (resetGlyphs . trimGlyphs) groups
    | otherwise = fmap resetGlyphs groups
  buildLine = buildTextLine font fSize fSpcH fSpcV metrics top
  res
    | text /= "" = Seq.mapWithIndex buildLine resetGroups
    | otherwise = Seq.singleton (buildLine 0 Empty)

buildTextLine
  :: Font
  -> FontSize
  -> FontSpace
  -> FontSpace
  -> TextMetrics
  -> Double
  -> Int
  -> Seq GlyphPos
  -> TextLine
buildTextLine font fSize fSpcH fSpcV metrics top idx glyphs = textLine where
  lineH = _txmLineH metrics
  x = 0
  y = top + fromIntegral idx * (lineH + unFontSpace fSpcV)
  width = getGlyphsWidth glyphs
  height = lineH
  text = T.pack . reverse $ foldl' (\ac g -> _glpGlyph g : ac) [] glyphs
  textLine = TextLine {
    _tlFont = font,
    _tlFontSize = fSize,
    _tlFontSpaceH = fSpcH,
    _tlFontSpaceV = fSpcV,
    _tlMetrics = metrics,
    _tlText = text,
    _tlSize = Size width height,
    _tlRect = Rect x y width height,
    _tlGlyphs = glyphs
  }

addEllipsisToTextLine
  :: FontManager
  -> StyleState
  -> Double
  -> TextLine
  -> TextLine
addEllipsisToTextLine fontMgr style width textLine = newTextLine where
  TextLine{..} = textLine
  Size tw th = _tlSize
  Size dw dh = calcTextSize fontMgr style "..."
  font = styleFont style
  fontSize = styleFontSize style
  fontSpcH = styleFontSpaceH style
  targetW = width - tw
  dropHelper (idx, w) g
    | _glpW g + w <= dw = (idx + 1, _glpW g + w)
    | otherwise = (idx, w)
  (dropChars, _) = foldl' dropHelper (0, targetW) (Seq.reverse _tlGlyphs)
  newText = T.dropEnd dropChars _tlText <> "..."
  !newGlyphs = computeGlyphsPos fontMgr font fontSize fontSpcH newText
  newW = getGlyphsWidth newGlyphs
  newTextLine = textLine {
    _tlText = newText,
    _tlSize = _tlSize { _sW = newW },
    _tlRect = _tlRect { _rW = newW },
    _tlGlyphs = newGlyphs
  }

clipTextLine
  :: FontManager
  -> StyleState
  -> TextTrim
  -> Double
  -> TextLine
  -> TextLine
clipTextLine fontMgr style trim width textLine = newTextLine where
  TextLine{..} = textLine
  Size tw th = _tlSize
  font = styleFont style
  fontSize = styleFontSize style
  fontSpcH = styleFontSpaceH style
  takeHelper (idx, w) g
    | _glpW g + w <= width = (idx + 1, _glpW g + w)
    | otherwise = (idx, w)
  (takeChars, _) = foldl' takeHelper (0, 0) _tlGlyphs
  validGlyphs = Seq.takeWhileL (\g -> _glpXMax g <= width) _tlGlyphs
  newText
    | trim == KeepSpaces = T.take (length validGlyphs) _tlText
    | otherwise = T.dropWhileEnd (== ' ') $ T.take (length validGlyphs) _tlText
  !newGlyphs = computeGlyphsPos fontMgr font fontSize fontSpcH newText
  newW = getGlyphsWidth newGlyphs
  newTextLine = textLine {
    _tlText = newText,
    _tlSize = _tlSize { _sW = newW },
    _tlRect = _tlRect { _rW = newW },
    _tlGlyphs = newGlyphs
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

getGlyphsWidth :: Seq GlyphPos -> Double
getGlyphsWidth glyphs = getGlyphsMax glyphs - getGlyphsMin glyphs

getTextLinesSize :: Seq TextLine -> Size
getTextLinesSize textLines = size where
  -- Excludes last line vertical spacing
  spaceV = unFontSpace $ maybe def _tlFontSpaceV (textLines ^? ix 0)
  lineW line = line ^. L.size . L.w
  lineH line = line ^. L.size . L.h + unFontSpace (_tlFontSpaceV line)
  width = maximum (fmap lineW textLines)
  height = sum (fmap lineH textLines) - spaceV
  size
    | Seq.null textLines = def
    | otherwise = Size width height

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
