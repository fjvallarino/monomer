{-|
Module      : Monomer.Graphics.FFI
Copyright   : (c) 2018 Francisco Vallarino,
              (c) 2016 Moritz Kiefer
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Provides functions for getting text dimensions and metrics.

Based on code from cocreature's https://github.com/cocreature/nanovg-hs
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Monomer.Graphics.FFI where

import Control.Monad (forM)
import Data.ByteString (useAsCString, useAsCStringLen, ByteString)
import Data.ByteString.Char8 (useAsCStringLen, unpack)
import Data.Text (Text)
import Data.Text.Foreign (withCStringLen)
import Data.Sequence (Seq)
import Foreign
import Foreign.C (CString, castCharToCUChar, newCAStringLen)
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Monomer.Graphics.Types (GlyphPos(..))

#include "fontmanager.h"

-- | Vector of 4 strict elements.
data V4 a = V4 !a !a !a !a
  deriving (Show, Read, Eq, Ord)

-- | Bounds of a block of text.
newtype Bounds
  = Bounds (V4 CFloat)
  deriving (Show, Read, Eq, Ord)

instance Storable Bounds where
  sizeOf _ = sizeOf (0 :: CFloat) * 4
  alignment _ = alignment (0 :: CFloat)
  peek p =
    do let p' = castPtr p :: Ptr CFloat
       a <- peekElemOff p' 0
       b <- peekElemOff p' 1
       c <- peekElemOff p' 2
       d <- peekElemOff p' 3
       pure (Bounds (V4 a b c d))
  poke p (Bounds (V4 a b c d)) =
    do let p' = castPtr p :: Ptr CFloat
       pokeElemOff p' 0 a
       pokeElemOff p' 1 b
       pokeElemOff p' 2 c
       pokeElemOff p' 3 d

-- | Position of a glyph in a text string.
data GlyphPosition = GlyphPosition {
  -- | Pointer of the glyph in the input string.
  str :: !(Ptr CChar),
  -- | The x-coordinate of the logical glyph position.
  glyphX :: !CFloat,
  -- | The left bound of the glyph shape.
  glyphPosMinX :: !CFloat,
  -- | The right bound of the glyph shape.
  glyphPosMaxX :: !CFloat,
  -- | The lower bound of the glyph shape.
  glyphPosMinY :: !CFloat,
  -- | The upper bound of the glyph shape.
  glyphPosMaxY :: !CFloat
} deriving (Show, Eq, Ord)

instance Storable GlyphPosition where
  sizeOf _ = {# sizeof FMGglyphPosition #}
  alignment _ = {#alignof FMGglyphPosition#}
  peek p =
    do str <- {#get FMGglyphPosition->str#} p
       x <- {#get FMGglyphPosition->x#} p
       minx <- {#get FMGglyphPosition->minx#} p
       maxx <- {#get FMGglyphPosition->maxx#} p
       miny <- {#get FMGglyphPosition->miny#} p
       maxy <- {#get FMGglyphPosition->maxy#} p
       pure (GlyphPosition str x minx maxx miny maxy)
  poke p (GlyphPosition str x minx maxx miny maxy) =
    do {#set FMGglyphPosition->str#} p str
       {#set FMGglyphPosition->x#} p x
       {#set FMGglyphPosition->minx#} p minx
       {#set FMGglyphPosition->maxx#} p maxx
       {#set FMGglyphPosition->miny#} p miny
       {#set FMGglyphPosition->maxy#} p maxy

{#pointer *FMGglyphPosition as GlyphPositionPtr -> GlyphPosition#}

-- | Reads Bounds from a pointer.
peekBounds :: Ptr CFloat -> IO Bounds
peekBounds = peek . castPtr

-- | Allocates space for Bounds.
allocaBounds :: (Ptr CFloat -> IO b) -> IO b
allocaBounds f = alloca (\(p :: Ptr Bounds) -> f (castPtr p))

withCString :: Text -> (CString -> IO b) -> IO b
withCString t = useAsCString (T.encodeUtf8 t)

withText :: Text -> (CString -> IO b) -> IO b
withText t = useAsCString (T.encodeUtf8 t)

-- | Marshalling helper for a constant 'nullPtr'
withNull :: (Ptr a -> b) -> b
withNull f = f nullPtr

toUString :: ByteString -> ((Ptr CChar, CInt) -> IO a) -> IO a
toUString bs continuation = do
  -- not freeing the new string, as FMContext frees the fonts upon destruction
  (ptr, len) <- newCAStringLen (unpack bs)
  let args = (ptr, fromIntegral len)
  continuation args

-- Common
{# pointer *FMcontext as FMContext newtype #}
deriving instance Storable FMContext

{# fun unsafe fmInit {`Double'} -> `FMContext' #}

{# fun unsafe fmCreateFont {`FMContext', withCString*`Text', withCString*`Text'} -> `Int' #}

{# fun unsafe fmCreateFontMem {`FMContext', withCString*`Text', toUString*`ByteString'&} -> `Int' #}

{# fun unsafe fmSetScale {`FMContext', `Double'} -> `()' #}

{# fun unsafe fmFontFace {`FMContext', withCString*`Text'} -> `()' #}

{# fun unsafe fmFontSize {`FMContext', `Double'} -> `()' #}

{# fun unsafe fmFontBlur {`FMContext', `Double'} -> `()' #}

{# fun unsafe fmTextLetterSpacing {`FMContext', `Double'} -> `()' #}

{# fun unsafe fmTextLineHeight {`FMContext', `Double'} -> `()' #}

{# fun unsafe fmTextMetrics as fmTextMetrics_ {`FMContext', alloca- `CFloat' peek*, alloca- `CFloat' peek*, alloca- `CFloat' peek*} -> `()' #}

fmTextMetrics :: FMContext -> IO (Double, Double, Double)
fmTextMetrics fm = do
  (asc, desc, lineh) <- fmTextMetrics_ fm
  return (realToFrac asc, realToFrac desc, realToFrac lineh)

{# fun unsafe fmTextBounds as fmTextBounds_
        {`FMContext', `Double', `Double', withText*`Text', withNull-`Ptr CUChar', allocaBounds-`Bounds'peekBounds*} -> `Double' #}

fmTextBounds :: FMContext -> Double -> Double -> Text -> IO (Double, Double, Double, Double)
fmTextBounds fm x y text = do
  (_, Bounds (V4 x1 y1 x2 y2)) <- fmTextBounds_ fm x y text
  return (realToFrac x1, realToFrac y1, realToFrac x2, realToFrac y2)

{# fun unsafe fmTextGlyphPositions as fmTextGlyphPositions_
        {`FMContext', `Double', `Double', id`Ptr CChar', id`Ptr CChar', `GlyphPositionPtr', `CInt'} -> `CInt' #}

fmTextGlyphPositions :: FMContext -> Double -> Double -> Text -> IO (Seq GlyphPosition)
fmTextGlyphPositions c x y text =
  withCStringLen text $ \(ptr, len) -> do
    let startPtr = ptr
    let endPtr = ptr `plusPtr` len
    allocaBytesAligned bufferSize align $ \arrayPtr -> do
      count <- fmTextGlyphPositions_ c x y startPtr endPtr arrayPtr maxGlyphs
      Seq.fromList <$> readChunk arrayPtr count
  where
    maxGlyphs = fromIntegral (T.length text)
    bufferSize = sizeOf (undefined :: GlyphPosition) * fromIntegral maxGlyphs
    align = alignment (undefined :: GlyphPosition)
    readChunk :: GlyphPositionPtr -> CInt -> IO [GlyphPosition]
    readChunk arrayPtr count = forM [0..count-1] $ \i ->
      peekElemOff arrayPtr (fromIntegral i)
