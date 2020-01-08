{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GUI.Platform.NanoVGRenderer (makeRenderer) where

import Data.Default

import Control.Monad (when)

import qualified GUI.Common.Types as C
import qualified Data.Text as T
import qualified NanoVG as VG

import Control.Monad.IO.Class
import GHC.Float

makeRenderer :: (MonadIO m) => VG.Context -> Double -> C.Renderer m
makeRenderer c dpr = C.Renderer {..} where
  beginPath =
    liftIO $ VG.beginPath c
  -- Context management
  saveContext =
    liftIO $ VG.save c

  restoreContext =
    liftIO $ VG.restore c

  -- Scissor operations
  scissor (C.Rect x y w h) =
    liftIO $ VG.scissor c (realToFrac $ x * dpr) (realToFrac $ y * dpr) (realToFrac $ w * dpr) (realToFrac $ h * dpr)

  resetScissor =
    liftIO $ VG.resetScissor c

  -- Strokes
  stroke =
    liftIO $ VG.stroke c

  strokeColor color = do
    liftIO $ VG.strokeColor c (colorToPaint color)

  strokeWidth width = do
    liftIO $ VG.strokeWidth c (realToFrac $ width * dpr)

  -- Fill
  fill =
    liftIO $ VG.fill c

  fillColor color = do
    liftIO $ VG.fillColor c (colorToPaint color)

  fillLinearGradient (C.Point x1 y1) (C.Point x2 y2) color1 color2 =
    let
      col1 = colorToPaint color1
      col2 = colorToPaint color2
    in do
      gradient <- liftIO $ VG.linearGradient c (realToFrac $ x1 * dpr) (realToFrac $ y1 * dpr) (realToFrac $ x2 * dpr) (realToFrac $ y2 * dpr) col1 col2
      liftIO $ VG.fillPaint c gradient

  -- Drawing
  moveTo (C.Point x y) = do
    liftIO $ nvMoveTo c (x * dpr) (y * dpr)

  line (C.Point x1 y1) (C.Point x2 y2) = do
    liftIO $ nvMoveTo c (x1 * dpr) (y1 * dpr)
    liftIO $ nvLineTo c (x2 * dpr) (y2 * dpr)

  lineTo (C.Point x y) = do
    liftIO $ nvLineTo c (x * dpr) (y * dpr)

  rect (C.Rect x y w h) = do
    liftIO $ VG.rect c (realToFrac $ x * dpr) (realToFrac $ y * dpr) (realToFrac $ w * dpr) (realToFrac $ h * dpr)

  arc (C.Point x1 y1) rad angleStart angleEnd = do
    liftIO $ nvArc c (x1 * dpr) (y1 * dpr) (rad * dpr) angleStart angleEnd VG.CW

  quadTo (C.Point x1 y1) (C.Point x2 y2) = do
    liftIO $ VG.quadTo c (realToFrac $ x1 * dpr) (realToFrac $ y1 * dpr) (realToFrac $ x2 * dpr) (realToFrac $ y2 * dpr)

  ellipse (C.Rect x y w h) = do
    liftIO $ VG.ellipse c (realToFrac $ cx * dpr) (realToFrac $ cy * dpr) (realToFrac $ rx * dpr) (realToFrac $ ry * dpr)
    where cx = x + rx
          cy = y + ry
          rx = w / 2
          ry = h / 2

  -- Text
  text (C.Rect x y w h) font fontSize (C.Align ha va) message = do
    liftIO $ VG.fontFace c font
    liftIO $ VG.fontSize c $ realToFrac $ fontSize * dpr
    VG.Bounds (VG.V4 x1 _ x2 _) <- liftIO $ VG.textBounds c (realToFrac $ x * dpr) (realToFrac $ y * dpr) message
    (asc, desc, _) <- liftIO $ VG.textMetrics c

    let xr = x * dpr
        yr = y * dpr
        wr = w * dpr
        hr = h * dpr
        tw = x2 - x1
        th = asc + desc
        tx | ha == C.ALeft = xr
           | ha == C.ACenter = xr + (wr - realToFrac tw) / 2
           | otherwise = xr + (wr - realToFrac tw)
        ty | va == C.ATop = yr + realToFrac th
           | va == C.AMiddle = yr + (hr + realToFrac th) / 2
           | otherwise = yr + hr

    when (message /= "") $ do
      liftIO $ VG.text c (realToFrac tx) (realToFrac ty) message

    return $ C.Rect (tx / dpr) ((ty - realToFrac asc) / dpr) (realToFrac tw / dpr) (realToFrac th / dpr)

  textBounds _ _ "" = return def
  textBounds font fontSize message = do
    liftIO $ VG.fontFace c font
    liftIO $ VG.fontSize c $ realToFrac $ fontSize
    VG.Bounds (VG.V4 x1 y1 x2 y2) <- liftIO $ VG.textBounds c 0 0 message

    return $ C.Size (realToFrac $ x2 - x1) (realToFrac $ y2 - y1)

nvMoveTo :: VG.Context -> Double -> Double -> IO ()
nvMoveTo c x y = do
  VG.moveTo c (realToFrac x) (realToFrac y)

nvLineTo :: VG.Context -> Double -> Double -> IO ()
nvLineTo c x y = do
  VG.lineTo c (realToFrac x) (realToFrac y)

nvArc :: VG.Context -> Double -> Double -> Double -> Double -> Double -> VG.Winding -> IO ()
nvArc c cx cy radius angleStart angleEnd winding = do
  VG.arc c (realToFrac cx) (realToFrac cy) (realToFrac radius) (VG.degToRad $ realToFrac angleStart) (VG.degToRad $ realToFrac angleEnd) winding

colorToPaint :: C.Color -> VG.Color
colorToPaint (C.Color r g b a)
  | a >= 1.0  = VG.rgb (fromIntegral r) (fromIntegral g) (fromIntegral b)
  | otherwise = VG.rgba (fromIntegral r) (fromIntegral g) (fromIntegral b) (round $ a * 255)
