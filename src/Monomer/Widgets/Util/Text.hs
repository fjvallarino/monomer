{-# LANGUAGE BangPatterns #-}

module Monomer.Widgets.Util.Text (
  fitText,
  getTextMetrics,
  getTextSize,
  getTextRect,
  getTextGlyphs,
  getGlyphsMin,
  getGlyphsMax,
) where

import Data.Maybe
import Data.Sequence (Seq(..), (><), (|>))
import Data.Text (Text)

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Core
import Monomer.Graphics
import Monomer.Widgets.Util.Style

getTextMetrics
  :: WidgetEnv s e -> StyleState -> Rect -> Align -> Text -> TextMetrics
getTextMetrics wenv style !rect !align !text = textMetrics where
  renderer = _weRenderer wenv
  !textMetrics = computeTextMetrics renderer font fontSize
  font = styleFont style
  fontSize = styleFontSize style

getTextSize :: WidgetEnv s e -> StyleState -> Text -> Size
getTextSize wenv style !text = textBounds where
  font = styleFont style
  fontSize = styleFontSize style
  !textBounds = computeTextSize (_weRenderer wenv) font fontSize text

getTextRect
  :: WidgetEnv s e -> StyleState -> Rect -> Align -> Text -> Rect
getTextRect wenv style !rect !align !text = textRect where
  renderer = _weRenderer wenv
  !textRect = computeTextRect renderer rect font fontSize align text
  font = styleFont style
  fontSize = styleFontSize style

fitText :: WidgetEnv s e -> StyleState -> Rect -> Text -> (Text, Size)
fitText wenv style !rect !text = (newText, newSize) where
  font = styleFont style
  fontSize = styleFontSize style
  sizeHandler = computeTextSize (_weRenderer wenv)
  !size = sizeHandler font fontSize text
  (newText, newSize)
    | _sW size <= _rW rect = (text, size)
    | otherwise = fitEllipsis wenv style rect size text

fitEllipsis
  :: WidgetEnv s e -> StyleState -> Rect -> Size -> Text -> (Text, Size)
fitEllipsis wenv style !rect !textSize !text = (newText, newSize) where
  Size tw th = textSize
  vpW = _rW rect
  !glyphs = getTextGlyphs wenv style (text <> ".")
  dotW = maybe 0 _glpW (Seq.lookup (Seq.length glyphs - 1) glyphs)
  dotsW = 3 * dotW
  dotsFit = vpW >= tw + dotsW
  targetW
    | dotsFit = vpW
    | otherwise = vpW - dotsW
  (gCount, gWidth) = fitGlyphsCount targetW 0 glyphs
  remW = vpW - gWidth
  dotCount = min 3 . max 0 $ round (remW / dotW)
  newText
    | dotsFit = text <> "..."
    | otherwise = T.take gCount text <> T.replicate dotCount "."
  newWidth
    | dotsFit = tw + dotsW
    | otherwise = gWidth + fromIntegral dotCount * dotW
  newSize = Size newWidth th

getTextGlyphs :: WidgetEnv s e -> StyleState -> Text -> Seq GlyphPos
getTextGlyphs wenv style !text = glyphs where
  font = styleFont style
  fontSize = styleFontSize style
  glyphs = computeGlyphsPos (_weRenderer wenv) font fontSize text

getGlyphsMin :: Seq GlyphPos -> Double
getGlyphsMin Empty = 0
getGlyphsMin (g :<| gs) = _glpXMin g

getGlyphsMax :: Seq GlyphPos -> Double
getGlyphsMax Empty = 0
getGlyphsMax (gs :|> g) = _glpXMax g

fitGlyphsCount :: Double -> Double -> Seq GlyphPos -> (Int, Double)
fitGlyphsCount _ _ Empty = (0, 0)
fitGlyphsCount targetW currW (g :<| gs)
  | newCurrW <= targetW = (gCount + 1, gWidth + gsW)
  | otherwise = (0, 0)
  where
    gsW = _glpW g
    newCurrW = currW + gsW
    (gCount, gWidth) = fitGlyphsCount targetW newCurrW gs
