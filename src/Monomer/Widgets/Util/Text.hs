module Monomer.Widgets.Util.Text where

import Control.Lens ((^.), (^?), _Just)
import Data.Maybe
import Data.Sequence (Seq(..), (><), (|>))
import Data.Text (Text)

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Core
import Monomer.Graphics

import qualified Monomer.Core.Lens.Style as S

getTextSize :: WidgetEnv s e -> ThemeState -> StyleState -> Text -> Size
getTextSize wenv theme style text = handler font fontSize text where
  handler = computeTextSize (_weRenderer wenv)
  (font, fontSize) = getFontAndSize theme style

getFullTextSize :: WidgetEnv s e -> ThemeState -> StyleState -> Text -> Size
getFullTextSize wenv theme style text = totalBounds where
  textBounds = getTextSize wenv theme style text
  totalBounds = addOuterSize style textBounds

getTextRect
  :: WidgetEnv s e -> ThemeState -> StyleState -> Rect -> Align -> Text -> Rect
getTextRect wenv theme style rect align text = textRect where
  textRect = computeTextRect (_weRenderer wenv) rect font fontSize align text
  (font, fontSize) = getFontAndSize theme style

fitText
  :: WidgetEnv s e -> ThemeState -> StyleState -> Rect -> Text -> (Text, Size)
fitText wenv theme style viewport text = (newText, newSize) where
  (font, fontSize) = getFontAndSize theme style
  sizeHandler = computeTextSize (_weRenderer wenv)
  size = sizeHandler font fontSize text
  (newText, newSize)
    | _sW size <= _rW viewport = (text, size)
    | otherwise = fitEllipsis wenv theme style viewport size text

fitEllipsis
  :: WidgetEnv s e
  -> ThemeState
  -> StyleState
  -> Rect
  -> Size
  -> Text
  -> (Text, Size)
fitEllipsis wenv theme style viewport textSize text = (newText, newSize) where
  Size tw th = textSize
  vpW = _rW viewport
  glyphs = getTextGlyphs wenv theme style (text <> ".")
  dotW = _glpW $ Seq.index glyphs (Seq.length glyphs - 1)
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

getTextGlyphs
  :: WidgetEnv s e -> ThemeState -> StyleState -> Text -> Seq GlyphPos
getTextGlyphs wenv theme style text = glyphs where
  (font, fontSize) = getFontAndSize theme style
  glyphs = computeGlyphsPos (_weRenderer wenv) font fontSize text

glyphsLength :: Seq GlyphPos -> Double
glyphsLength Empty = 0
glyphsLength (gs :|> g) = _glpXMax g

fitGlyphsCount :: Double -> Double -> Seq GlyphPos -> (Int, Double)
fitGlyphsCount _ _ Empty = (0, 0)
fitGlyphsCount totalW currW (g :<| gs)
  | newCurrW <= totalW = (gCount + 1, gWidth + gsW)
  | otherwise = (0, 0)
  where
    gsW = _glpW g
    newCurrW = currW + gsW
    (gCount, gWidth) = fitGlyphsCount totalW newCurrW gs

getFontAndSize :: ThemeState -> StyleState -> (Font, FontSize)
getFontAndSize theme style = (font, fontSize) where
  styleFont = style ^? S.text . _Just  . S.font . _Just
  styleFontSize = style ^? S.text . _Just . S.fontSize . _Just
  themeFont = theme ^. S.font
  themeFontSize = theme ^. S.fontSize
  font = fromMaybe themeFont styleFont
  fontSize = fromMaybe themeFontSize styleFontSize
