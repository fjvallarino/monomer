{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Graphics.NanoVGRenderer (makeRenderer) where

import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Default
import Data.IORef
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import System.IO.Unsafe

import qualified NanoVG as VG
import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Graphics.Renderer
import Monomer.Graphics.Types

type Overlays m = Monad m => IORef (Seq (m ()))

makeRenderer :: (MonadIO m) => VG.Context -> Double -> m (Renderer m)
makeRenderer c dpr = do
  overlaysRef <- liftIO $ newIORef Seq.empty

  return $ newRenderer c dpr overlaysRef

newRenderer :: (MonadIO m) => VG.Context -> Double -> Overlays m -> Renderer m
newRenderer c dpr overlaysRef = Renderer {..} where
  beginPath =
    liftIO $ VG.beginPath c

  closePath =
    liftIO $ VG.closePath c

  -- Context management
  saveContext =
    liftIO $ VG.save c

  restoreContext =
    liftIO $ VG.restore c

  -- Overlays
  createOverlay overlay =
    liftIO $ modifyIORef overlaysRef (|> overlay)

  renderOverlays = do
    overlays <- liftIO $ readIORef overlaysRef
    sequence_ overlays
    liftIO $ writeIORef overlaysRef Seq.empty

  -- Scissor operations
  setScissor (Rect x y w h) =
    liftIO $ VG.scissor c
      (realToFrac $ x * dpr)
      (realToFrac $ y * dpr)
      (realToFrac $ w * dpr)
      (realToFrac $ h * dpr)

  resetScissor =
    liftIO $ VG.resetScissor c

  -- Strokes
  stroke =
    liftIO $ VG.stroke c

  setStrokeColor color =
    liftIO $ VG.strokeColor c (colorToPaint color)

  setStrokeWidth width =
    liftIO $ VG.strokeWidth c (realToFrac $ width * dpr)

  -- Fill
  fill =
    liftIO $ VG.fill c

  setFillColor color =
    liftIO $ VG.fillColor c (colorToPaint color)

  setFillLinearGradient (Point x1 y1) (Point x2 y2) color1 color2 =
    let
      col1 = colorToPaint color1
      col2 = colorToPaint color2
    in do
      gradient <- liftIO $ VG.linearGradient c
        (realToFrac $ x1 * dpr)
        (realToFrac $ y1 * dpr)
        (realToFrac $ x2 * dpr)
        (realToFrac $ y2 * dpr)
        col1 col2
      liftIO $ VG.fillPaint c gradient

  -- Drawing
  moveTo (Point x y) =
    liftIO $ nvMoveTo c (x * dpr) (y * dpr)

  renderLine (Point x1 y1) (Point x2 y2) = do
    liftIO $ nvMoveTo c (x1 * dpr) (y1 * dpr)
    liftIO $ nvLineTo c (x2 * dpr) (y2 * dpr)

  renderLineTo (Point x y) = do
    liftIO $ VG.lineJoin c VG.Bevel
    liftIO $ nvLineTo c (x * dpr) (y * dpr)

  renderRect (Rect x y w h) =
    liftIO $ VG.rect c
      (realToFrac $ x * dpr)
      (realToFrac $ y * dpr)
      (realToFrac $ w * dpr)
      (realToFrac $ h * dpr)

  renderArc (Point x1 y1) rad angleStart angleEnd winding =
    liftIO $ nvArc c
      (x1 * dpr) (y1 * dpr)
      (rad * dpr)
      angleStart angleEnd
      (convertWinding winding)

  renderQuadTo (Point x1 y1) (Point x2 y2) =
    liftIO $ VG.quadTo c
      (realToFrac $ x1 * dpr) (realToFrac $ y1 * dpr)
      (realToFrac $ x2 * dpr) (realToFrac $ y2 * dpr)

  renderEllipse (Rect x y w h) =
    liftIO $ VG.ellipse c
      (realToFrac $ cx * dpr) (realToFrac $ cy * dpr)
      (realToFrac $ rx * dpr) (realToFrac $ ry * dpr)
    where cx = x + rx
          cy = y + ry
          rx = w / 2
          ry = h / 2

  -- Text
  renderText (Rect x y w h) font fontSize (Align ha va) message = do
    liftIO $ VG.fontFace c font
    liftIO $ VG.fontSize c $ realToFrac $ fontSize * dpr
    VG.Bounds (VG.V4 x1 _ x2 _) <- liftIO $ VG.textBounds c
      (realToFrac $ x * dpr)
      (realToFrac $ y * dpr)
      message
    (asc, desc, _) <- liftIO $ VG.textMetrics c

    let xr = x * dpr
        yr = y * dpr
        wr = w * dpr
        hr = h * dpr
        tw = x2 - x1
        th = asc + desc
        tx | ha == ALeft = xr
           | ha == ACenter = xr + (wr - realToFrac tw) / 2
           | otherwise = xr + (wr - realToFrac tw)
        ty | va == ATop = yr + realToFrac th
           | va == AMiddle = yr + (hr + realToFrac th) / 2
           | otherwise = yr + hr

    when (message /= "") $
      liftIO $ VG.text c (realToFrac tx) (realToFrac ty) message

    return $ Rect
      (tx / dpr)
      ((ty - realToFrac asc) / dpr)
      (realToFrac tw / dpr)
      (realToFrac th / dpr)

  getTextSize font fontSize message = unsafePerformIO $ do
    let text = if message == "" then " " else message

    liftIO $ VG.fontFace c font
    liftIO $ VG.fontSize c $ realToFrac fontSize
    VG.Bounds (VG.V4 x1 y1 x2 y2) <- liftIO $ VG.textBounds c 0 0 text

    return $ Size (realToFrac $ x2 - x1) (realToFrac $ y2 - y1)

nvMoveTo :: VG.Context -> Double -> Double -> IO ()
nvMoveTo c x y =
  VG.moveTo c (realToFrac x) (realToFrac y)

nvLineTo :: VG.Context -> Double -> Double -> IO ()
nvLineTo c x y =
  VG.lineTo c (realToFrac x) (realToFrac y)

nvArc
  :: VG.Context
  -> Double -> Double -> Double -> Double -> Double -> VG.Winding -> IO ()
nvArc c cx cy radius angleStart angleEnd winding =
  VG.arc c
    (realToFrac cx) (realToFrac cy)
    (realToFrac radius)
    (VG.degToRad $ realToFrac angleStart) (VG.degToRad $ realToFrac angleEnd)
    winding

colorToPaint :: Color -> VG.Color
colorToPaint (Color r g b a)
  | a >= 1.0  = VG.rgb red green blue
  | otherwise = VG.rgba red green blue alpha
  where
    red = fromIntegral r
    green = fromIntegral g
    blue = fromIntegral b
    alpha = round $ a * 255

convertWinding :: Winding -> VG.Winding
convertWinding CW = VG.CW
convertWinding CCW = VG.CCW
