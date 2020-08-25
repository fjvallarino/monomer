{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Graphics.NanoVGRenderer (makeRenderer) where

import Debug.Trace

import Control.Monad (forM, when)
import Control.Monad.IO.Class
import Data.Default
import Data.IORef
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import Foreign.C.Types (CFloat)
import System.IO.Unsafe

import qualified NanoVG as VG
import qualified NanoVG.Internal.Image as VGI
import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Graphics.Renderer
import Monomer.Graphics.Types

type Overlays m = Monad m => IORef (Seq (m ()))

data CPoint
    = CPoint CFloat CFloat
    deriving (Eq, Show)

data CRect
    = CRect CFloat CFloat CFloat CFloat
    deriving (Eq, Show)

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
  setScissor rect =
    liftIO $ VG.scissor c x y w h
    where
      CRect x y w h = rectToCRect rect dpr

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

  setFillLinearGradient p1 p2 color1 color2 = do
    gradient <- liftIO $ VG.linearGradient c x1 y1 x2 y2 col1 col2
    liftIO $ VG.fillPaint c gradient
    where
      col1 = colorToPaint color1
      col2 = colorToPaint color2
      CPoint x1 y1 = pointToCPoint p1 dpr
      CPoint x2 y2 = pointToCPoint p2 dpr

  -- Drawing
  moveTo point =
    liftIO $ VG.moveTo c x y
    where
      CPoint x y = pointToCPoint point dpr

  renderLine p1 p2 = do
    liftIO $ VG.moveTo c x1 y1
    liftIO $ VG.lineTo c x2 y2
    where
      CPoint x1 y1 = pointToCPoint p1 dpr
      CPoint x2 y2 = pointToCPoint p2 dpr

  renderLineTo point = do
    liftIO $ VG.lineJoin c VG.Bevel
    liftIO $ VG.lineTo c x y
    where
      CPoint x y = pointToCPoint point dpr

  renderRect rect =
    liftIO $ VG.rect c x y w h
    where
      CRect x y w h = rectToCRect rect dpr

  renderArc point rad angleStart angleEnd winding =
    liftIO $ VG.arc c x y radius start end wind
    where
      CPoint x y = pointToCPoint point dpr
      radius = realToFrac rad
      start = VG.degToRad $ realToFrac angleStart
      end = VG.degToRad $ realToFrac angleEnd
      wind = convertWinding winding

  renderQuadTo p1 p2 =
    liftIO $ VG.quadTo c x1 y1 x2 y2
    where
      CPoint x1 y1 = pointToCPoint p1 dpr
      CPoint x2 y2 = pointToCPoint p2 dpr

  renderEllipse rect =
    liftIO $ VG.ellipse c cx cy rx ry
    where
      CRect x y w h = rectToCRect rect dpr
      cx = x + rx
      cy = y + ry
      rx = w / 2
      ry = h / 2

  -- Text
  renderText rect font fontSize (Align ha va) message = do
    liftIO $ VG.fontFace c (unFont font)
    liftIO $ VG.fontSize c $ realToFrac $ unFontSize fontSize * dpr
    VG.Bounds (VG.V4 x1 _ x2 _) <- liftIO $ VG.textBounds c x y message
    (asc, desc, _) <- liftIO $ VG.textMetrics c

    let
      tw = x2 - x1
      th = asc + desc
      tx | ha == ALeft = x
         | ha == ACenter = x + (w - tw) / 2
         | otherwise = x + (w - tw)
      ty | va == ATop = y + th
         | va == AMiddle = y + (h + th) / 2
         | otherwise = y + h

    when (message /= "") $
      liftIO $ VG.text c tx ty message

    return $ Rect {
      _rX = fromCFloat tx,
      _rY = fromCFloat (ty - asc),
      _rW = fromCFloat tw,
      _rH = fromCFloat th
    }
    where
      CRect x y w h = rectToCRect rect dpr
      fromCFloat val = realToFrac $ val / realToFrac dpr

  computeTextSize font fontSize message = unsafePerformIO $ do
    let text = if message == "" then " " else message

    liftIO $ VG.fontFace c (unFont font)
    liftIO $ VG.fontSize c $ realToFrac (unFontSize fontSize)
    VG.Bounds (VG.V4 x1 y1 x2 y2) <- liftIO $ VG.textBounds c 0 0 text

    return $ Size (realToFrac $ x2 - x1) (realToFrac $ y2 - y1)

  createImage w h imgData = unsafePerformIO $ do
    nvImg <- liftIO $ VG.createImageRGBA c cw ch VGI.ImageNearest imgData
    forM nvImg $ createImageHandle c
    where
      cw = fromIntegral w
      ch = fromIntegral h

  renderImage rect image = do
    imgPaint <- liftIO $ VG.imagePattern c x y w h 0 nvImg 1
    liftIO $ VG.beginPath c
    liftIO $ VG.rect c x y w h
    liftIO $ VG.fillPaint c imgPaint
    liftIO $ VG.fill c
    where
      nvImg = VG.Image $ fromIntegral (_imageId image)
      CRect x y w h = rectToCRect rect dpr

createImageHandle :: (MonadIO m) => VG.Context -> VG.Image -> m ImageHandle
createImageHandle c nvImg = do
  (w, h) <- liftIO $ VG.imageSize c nvImg

  return $ ImageHandle {
    _imageId = fromIntegral $ VG.imageHandle nvImg,
    _imageSize = Size (fromIntegral w) (fromIntegral h)
  }

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

pointToCPoint :: Point -> Double -> CPoint
pointToCPoint (Point x y) dpr = CPoint cx cy where
  cx = realToFrac $ x * dpr
  cy = realToFrac $ y * dpr

rectToCRect :: Rect -> Double -> CRect
rectToCRect (Rect x y w h) dpr = CRect cx cy cw ch where
  cx = realToFrac $ x * dpr
  cy = realToFrac $ y * dpr
  ch = realToFrac $ h * dpr
  cw = realToFrac $ w * dpr
