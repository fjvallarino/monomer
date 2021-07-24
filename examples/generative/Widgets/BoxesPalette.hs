{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Widgets.BoxesPalette (
  BoxesPaletteCfg(..),
  HasItemWidth(..),
  HasPaletteType(..),
  HasPaletteSize(..),
  HasSeed(..),
  boxesPalette
) where

import Control.Lens
import Control.Monad (forM, when)
import Data.Default
import Data.Maybe
import System.Random

import Monomer.Graphics.ColorTable
import Monomer.Widgets.Single

import qualified Monomer.Lens as L

data ColorHSV
  = ColorHSV Double Double Double
  deriving (Eq, Show)

data BoxesPaletteCfg = BoxesPaletteCfg {
  _bpcItemWidth :: Double,
  _bpcPaletteType :: Int,
  _bpcPaletteSize :: Int,
  _bpcSeed :: Maybe Int
} deriving (Eq, Show)

instance Default BoxesPaletteCfg where
  def = BoxesPaletteCfg {
    _bpcItemWidth = 25,
    _bpcPaletteType = 0,
    _bpcPaletteSize = 20,
    _bpcSeed = Just 42
  }

data BoxesPaletteState = BoxesPaletteState {
  _cgcMouseX :: Double,
  _cgcMouseY :: Double
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''BoxesPaletteCfg
makeLensesWith abbreviatedFields ''BoxesPaletteState

boxesPalette :: BoxesPaletteCfg -> WidgetNode s e
boxesPalette cfg = defaultWidgetNode "boxesPalette" widget where
  widget = makeBoxesPalette cfg (BoxesPaletteState 100 100)

makeBoxesPalette :: BoxesPaletteCfg -> BoxesPaletteState -> Widget s e
makeBoxesPalette cfg state = widget where
  widget = createSingle state def {
    singleUseScissor = True,
    singleMerge = merge,
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  merge wenv node oldNode oldState = resultNode newNode where
    newNode = node
      & L.widget .~ makeBoxesPalette cfg oldState

  handleEvent wenv node target evt = case evt of
    Move (Point x y) -> Just (resultReqs newNode [RenderOnce]) where
      newState = BoxesPaletteState x y
      newNode = node
        & L.widget .~ makeBoxesPalette cfg newState
    _ -> Nothing

  getSizeReq wenv node = (expandSize 100 1, expandSize 100 1)

  render wenv node renderer = do
    when (isJust (cfg ^. seed)) $
      setStdGen $ mkStdGen (fromJust $ cfg ^. seed)

    colors <- makePalette (cfg ^. paletteType) (cfg ^. paletteSize)
    mapM_ (drawRectangle renderer state colors vp cols rows) [0..cols * rows - 1]
    where
      style = activeStyle wenv node
      vp = getContentArea style node
      iw = cfg ^. itemWidth
      fw = 0.5 + 5 * (state ^. mouseX - vp ^. L.x) / vp ^. L.w
      fh = 0.5 + 5 * (state ^. mouseY - vp ^. L.y) / vp ^. L.h
      cols = round $ vp ^. L.w / (fw * iw)
      rows = round $ vp ^. L.h / (fh * iw)

drawRectangle
  :: Renderer
  -> BoxesPaletteState
  -> [Color]
  -> Rect
  -> Int
  -> Int
  -> Int
  -> IO ()
drawRectangle renderer state colors vp cols rows idx = do
  colorIdx :: Double <- randomIO
  let color = colors !! floor (fromIntegral (length colors) * colorIdx)
  beginPath renderer
  setFillColor renderer color
  renderRect renderer rect
  fill renderer
  where
    rw = fromIntegral . round $ vp ^. L.w / fromIntegral cols
    rh = fromIntegral . round $ vp ^. L.h / fromIntegral rows
    rx = vp ^. L.x + rw * fromIntegral (idx `rem` cols)
    ry = vp ^. L.y + rh * fromIntegral (idx `div` cols)
    rect = Rect rx ry rw rh

makePalette :: Int -> Int -> IO [Color]
makePalette palette count = forM [0..count - 1] $ \idx -> do
  h <- randomIO
  s <- randomIO
  v <- randomIO
  return $ hsvToRGB (makeHSV idx h s v)
  where
    makeHSV idx h s v
      | palette == 1 = ColorHSV h s 1
      | palette == 2 = ColorHSV h 1 v
      | palette == 3 && even idx = ColorHSV h 1 v
      | palette == 3 = ColorHSV (195 / 360) s 1
      | otherwise = ColorHSV h s v

hsvToRGB :: ColorHSV -> Color
hsvToRGB (ColorHSV h s v) = Color r g b 1 where
  i = floor (h * 6)
  f = h * 6 - fromIntegral i
  p = v * (1 - s)
  q = v * (1 - f * s)
  t = v * (1 - (1 - f) * s)

  (r1, g1, b1) = case i of
    0 -> (v, t, p)
    1 -> (q, v, p)
    2 -> (p, v, t)
    3 -> (p, q, v)
    4 -> (t, p, v)
    _ -> (v, p, q)
  to255 v = round (v * 255)
  (r, g, b) = (to255 r1, to255 g1, to255 b1)
