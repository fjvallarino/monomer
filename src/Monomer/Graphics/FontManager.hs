{-|
Module      : Monomer.Graphics.FontManager
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Provides functions for getting text dimensions and metrics.
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Monomer.Graphics.FontManager (
  makeFontManager
) where

import Control.Monad (foldM, when)

import Data.Default
import Data.Sequence (Seq)
import Data.Text (Text)
import System.IO.Unsafe

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Common.BasicTypes
import Monomer.Graphics.FFI
import Monomer.Graphics.Types
import Monomer.Helper (putStrLnErr)

-- | Creates a font manager instance.
makeFontManager
  :: [FontDef]       -- ^ The font definitions.
  -> Double          -- ^ The device pixel rate.
  -> IO FontManager  -- ^ The created renderer.
makeFontManager fonts dpr = do
  ctx <- fmInit dpr

  validFonts <- foldM (loadFont ctx) [] fonts

  when (null validFonts) $
    putStrLnErr "Could not find any valid fonts. Text size calculations will fail."

  return $ newManager ctx

newManager :: FMContext -> FontManager
newManager ctx = FontManager {..} where
  computeTextMetrics font fontSize =
    computeTextMetrics_ 1 font fontSize

  computeTextMetrics_ scale font fontSize = unsafePerformIO $ do
    setFont ctx scale font fontSize def
    (asc, desc, lineh) <- fmTextMetrics ctx
    lowerX <- Seq.lookup 0 <$> fmTextGlyphPositions ctx 0 0 "x"
    let heightLowerX = case lowerX of
          Just lx -> glyphPosMaxY lx - glyphPosMinY lx
          Nothing -> realToFrac asc

    return $ TextMetrics {
      _txmAsc = asc,
      _txmDesc = desc,
      _txmLineH = lineh,
      _txmLowerX = realToFrac heightLowerX
    }

  computeTextSize font fontSize fontSpaceH text =
    computeTextSize_ 1 font fontSize fontSpaceH text

  computeTextSize_ scale font fontSize fontSpaceH text = unsafePerformIO $ do
    setFont ctx scale font fontSize fontSpaceH
    (x1, y1, x2, y2) <- if text /= ""
      then fmTextBounds ctx 0 0 text
      else do
        (asc, desc, lineh) <- fmTextMetrics ctx
        return (0, 0, 0, lineh)

    return $ Size (realToFrac (x2 - x1)) (realToFrac (y2 - y1))

  computeGlyphsPos font fontSize fontSpaceH text =
    computeGlyphsPos_ 1 font fontSize fontSpaceH text

  computeGlyphsPos_ scale font fontSize fontSpaceH text = unsafePerformIO $ do
    setFont ctx scale font fontSize fontSpaceH
    glyphs <- if text /= ""
      then fmTextGlyphPositions ctx 0 0 text
      else return Seq.empty

    return $ Seq.zipWith toGlyphPos (Seq.fromList (T.unpack text)) glyphs
    where
      toGlyphPos chr glyph = GlyphPos {
        _glpGlyph = chr,
        _glpX = realToFrac (glyphX glyph),
        _glpXMin = realToFrac (glyphPosMinX glyph),
        _glpXMax = realToFrac (glyphPosMaxX glyph),
        _glpYMin = realToFrac (glyphPosMinY glyph),
        _glpYMax = realToFrac (glyphPosMaxY glyph),
        _glpW = realToFrac (glyphPosMaxX glyph - glyphPosMinX glyph),
        _glpH = realToFrac (glyphPosMaxY glyph - glyphPosMinY glyph)
      }

loadFont :: FMContext -> [Text] -> FontDef -> IO [Text]
loadFont ctx fonts (FontDef name path) = do
  res <- fmCreateFont ctx name path
  if res >= 0
    then return $ path : fonts
    else putStrLnErr ("Failed to load font: " ++ T.unpack name) >> return fonts

setFont :: FMContext -> Double -> Font -> FontSize -> FontSpace -> IO ()
setFont ctx scale (Font name) (FontSize size) (FontSpace spaceH) = do
  fmSetScale ctx scale
  fmFontFace ctx name
  fmFontSize ctx $ realToFrac size
  fmTextLetterSpacing ctx $ realToFrac spaceH
