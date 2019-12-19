{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GUI.Platform.NanoVGRenderer (makeRenderer) where

import Data.Default

import qualified GUI.Common.Types as C
import qualified Data.Text as T
import qualified NanoVG as VG

import Control.Monad.IO.Class
import GHC.Float

makeRenderer :: (MonadIO m) => VG.Context -> C.Renderer m
makeRenderer c = C.Renderer {..} where
  beginPath =
    liftIO $ VG.beginPath c
  -- Context management
  saveContext =
    liftIO $ VG.save c

  restoreContext =
    liftIO $ VG.restore c

  -- Scissor operations
  scissor (C.Rect x y w h) =
    liftIO $ VG.scissor c (realToFrac x) (realToFrac y) (realToFrac w) (realToFrac h)

  resetScissor =
    liftIO $ VG.resetScissor c

  -- Strokes
  stroke =
    liftIO $ VG.stroke c

  strokeColor (C.RGB r g b) = do
    liftIO $ VG.strokeColor c (VG.rgb (round r) (round g) (round b))

  strokeWidth width = do
    liftIO $ VG.strokeWidth c (realToFrac width)

  -- Fill
  fill =
    liftIO $ VG.fill c

  fillColor (C.RGB r g b) = do
    liftIO $ VG.fillColor c (VG.rgb (round r) (round g) (round b))

  fillLinearGradient (C.Point x1 y1) (C.Point x2 y2) (C.RGB r1 g1 b1) (C.RGB r2 g2 b2) =
    let
      col1 = VG.rgb (round r1) (round g1) (round b1)
      col2 = VG.rgb (round r2) (round g2) (round b2)
    in do
      gradient <- liftIO $ VG.linearGradient c (realToFrac x1) (realToFrac y1) (realToFrac x2) (realToFrac y2) col1 col2
      liftIO $ VG.fillPaint c gradient

  -- Drawing
  moveTo (C.Point x y) = do
    liftIO $ nvMoveTo c x y

  line (C.Point x1 y1) (C.Point x2 y2) = do
    liftIO $ nvMoveTo c x1 y1
    liftIO $ nvLineTo c x2 y2

  lineTo (C.Point x y) = do
    liftIO $ nvLineTo c x y

  rect (C.Rect x y w h) = do
    liftIO $ VG.rect c (realToFrac x) (realToFrac y) (realToFrac w) (realToFrac h)

  arc (C.Point x1 y1) rad angleStart angleEnd = do
    liftIO $ nvArc c x1 y1 rad angleStart angleEnd VG.CW

  quadTo (C.Point x1 y1) (C.Point x2 y2) = do
    liftIO $ VG.quadTo c (realToFrac x1) (realToFrac y1) (realToFrac x2) (realToFrac y2)

  ellipse (C.Rect x y w h) = do
    liftIO $ VG.ellipse c (realToFrac cx) (realToFrac cy) (realToFrac rx) (realToFrac ry)
    where cx = x + rx
          cy = y + ry
          rx = w / 2
          ry = h / 2

  -- Text
  text _ _ _ _ "" = return ()
  text (C.Rect x y w h) font fontSize (C.Align ha va) message = do
    liftIO $ VG.fontFace c font
    liftIO $ VG.fontSize c $ realToFrac fontSize
    VG.Bounds (VG.V4 x1 _ x2 _) <- liftIO $ VG.textBounds c (realToFrac x) (realToFrac y) message
    (asc, desc, _) <- liftIO $ VG.textMetrics c

    let tw = x2 - x1
        th = asc + desc
        tx | ha == C.ALeft = x
           | ha == C.ACenter = x + (w - realToFrac tw) / 2
           | otherwise = x + (w - realToFrac tw)
        ty | va == C.ATop = y + realToFrac th
           | va == C.AMiddle = y + (h + realToFrac th) / 2
           | otherwise = y + h

    liftIO $ VG.text c (realToFrac tx) (realToFrac ty) message

  textBounds _ _ "" = return def
  textBounds font fontSize message = do
    liftIO $ VG.fontFace c font
    liftIO $ VG.fontSize c $ realToFrac fontSize
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
