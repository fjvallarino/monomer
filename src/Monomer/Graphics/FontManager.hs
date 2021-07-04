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

module Monomer.Graphics.FontManager (
  makeFontManager
) where

import Control.Monad (foldM, when)

import Data.Sequence (Seq)
import Data.Text (Text)
import System.IO.Unsafe

import qualified Control.Concurrent.Lock as LK
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Common.BasicTypes
import Monomer.Graphics.FFI
import Monomer.Graphics.Types

makeFontManager
  :: [FontDef]    -- ^ The font definitions.
  -> Double       -- ^ The device pixel rate.
  -> IO FontManager  -- ^ The created renderer.
makeFontManager fonts dpr = do
  ctx <- fmInit dpr

  lock <- LK.new
  validFonts <- foldM (loadFont ctx) [] fonts

  when (null validFonts) $
    putStrLn "Could not find any valid fonts. Text will fail to be displayed."
  
  return $ newManager ctx dpr lock

newManager :: FMContext -> Double -> LK.Lock -> FontManager
newManager ctx dpr lock = FontManager {..} where
  computeTextMetrics font fontSize = unsafePerformIO $ LK.with lock $ do
    setFont ctx dpr font fontSize
    (asc, desc, lineh) <- fmTextMetrics ctx
    lowerX <- Seq.lookup 0 <$> fmTextGlyphPositions ctx 0 0 "x"
    let heightLowerX = case lowerX of
          Just lx -> glyphPosMaxY lx - glyphPosMinY lx
          Nothing -> realToFrac asc

    return $ TextMetrics {
      _txmAsc = asc / dpr,
      _txmDesc = desc / dpr,
      _txmLineH = lineh / dpr,
      _txmLowerX = realToFrac heightLowerX / dpr
    }

  computeTextSize font fontSize text = unsafePerformIO $ LK.with lock $ do
    setFont ctx dpr font fontSize
    (x1, y1, x2, y2) <- if text /= ""
      then fmTextBounds ctx 0 0 text
      else do
        (asc, desc, lineh) <- fmTextMetrics ctx
        return (0, 0, 0, lineh)

    return $ Size (realToFrac (x2 - x1) / dpr) (realToFrac (y2 - y1) / dpr)

  computeGlyphsPos font fontSize text = unsafePerformIO $ LK.with lock $ do
    setFont ctx dpr font fontSize
    glyphs <- if text /= ""
      then fmTextGlyphPositions ctx 0 0 text
      else return Seq.empty

    return $ Seq.zipWith toGlyphPos (Seq.fromList (T.unpack text)) glyphs
    where
      toGlyphPos chr glyph = GlyphPos {
        _glpGlyph = chr,
        _glpXMin = realToFrac (glyphPosMinX glyph) / dpr,
        _glpXMax = realToFrac (glyphPosMaxX glyph) / dpr,
        _glpYMin = realToFrac (glyphPosMinY glyph) / dpr,
        _glpYMax = realToFrac (glyphPosMaxY glyph) / dpr,
        _glpW = realToFrac (glyphPosMaxX glyph - glyphPosMinX glyph) / dpr,
        _glpH = realToFrac (glyphPosMaxY glyph - glyphPosMinY glyph) / dpr
      }


loadFont :: FMContext -> [Text] -> FontDef -> IO [Text]
loadFont ctx fonts (FontDef name path) = do
  res <- fmCreateFont ctx name path
  if res >= 0
    then return $ path : fonts
    else putStrLn ("Failed to load font: " ++ T.unpack name) >> return fonts

setFont :: FMContext -> Double -> Font -> FontSize -> IO ()
setFont ctx dpr (Font name) (FontSize size) = do
  fmFontFace ctx name
  fmFontSize ctx $ realToFrac $ size * dpr
