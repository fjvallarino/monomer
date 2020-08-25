{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Graphics.NanoVGRenderer (makeRenderer) where

import Debug.Trace

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
--import Control.Exception.Safe (finally)
import Control.Exception.Base (finally)
import Control.Monad (foldM, forM, when)
import Control.Monad.IO.Class
import Data.Default
import Data.IORef
import Data.Maybe
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import Foreign.C.Types (CFloat)
import System.IO.Unsafe

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified NanoVG as VG
import qualified NanoVG.Internal.Image as VGI

import Monomer.Common.Geometry
import Monomer.Graphics.Renderer
import Monomer.Graphics.Types

newtype Lock = Lock (MVar ())

type ImagesMap = M.Map String VG.Image

data ImageReq = ImageReq {
  _irName :: String,
  _irAction :: ResourceAction,
  _irWidth :: Int,
  _irHeight :: Int,
  _irImgData :: BS.ByteString
}

data Env = Env {
  overlays :: Seq (IO ()),
  imagesMap :: ImagesMap,
  addedImages :: Seq ImageReq
}

data CPoint
  = CPoint CFloat CFloat
  deriving (Eq, Show)

data CRect
  = CRect CFloat CFloat CFloat CFloat
  deriving (Eq, Show)

makeRenderer :: Double -> IO Renderer
makeRenderer dpr = do
  c <- VG.createGL3 (Set.fromList [VG.Antialias, VG.StencilStrokes, VG.Debug])
  _ <- VG.createFont c "sans" (VG.FileName "./assets/fonts/Roboto-Regular.ttf")

  lock <- newLock
  envRef <- newIORef $ Env {
    overlays = Seq.empty,
    imagesMap = M.empty,
    addedImages = Seq.empty
  }

  return $ newRenderer c dpr lock envRef

newRenderer :: VG.Context -> Double -> Lock -> IORef Env -> Renderer
newRenderer c dpr lock envRef = Renderer {..} where
  beginFrame w h = withLock lock $ do
    env <- readIORef envRef
    newMap <- handlePendingImages c (imagesMap env) (addedImages env)
    writeIORef envRef env {
      imagesMap = newMap,
      addedImages = Seq.empty
    }

    VG.beginFrame c cw ch pxRatio
    where
      cw = fromIntegral w
      ch = fromIntegral h
      pxRatio = fromIntegral w / fromIntegral h

  endFrame =
    VG.endFrame c

  beginPath =
    VG.beginPath c

  closePath =
    VG.closePath c

  -- Context management
  saveContext =
    VG.save c

  restoreContext =
    VG.restore c

  -- Overlays
  createOverlay overlay = withLock lock $
    modifyIORef envRef $ \env -> env {
      overlays = overlays env |> overlay
    }

  renderOverlays = withLock lock $ do
    env <- readIORef envRef
    sequence_ $ overlays env
    writeIORef envRef env {
      overlays = Seq.empty
    }

  -- Scissor operations
  setScissor rect =
    VG.scissor c x y w h
    where
      CRect x y w h = rectToCRect rect dpr

  resetScissor =
    VG.resetScissor c

  -- Strokes
  stroke =
    VG.stroke c

  setStrokeColor color =
    VG.strokeColor c (colorToPaint color)

  setStrokeWidth width =
    VG.strokeWidth c (realToFrac $ width * dpr)

  -- Fill
  fill =
    VG.fill c

  setFillColor color =
    VG.fillColor c (colorToPaint color)

  setFillLinearGradient p1 p2 color1 color2 = do
    gradient <- VG.linearGradient c x1 y1 x2 y2 col1 col2
    VG.fillPaint c gradient
    where
      col1 = colorToPaint color1
      col2 = colorToPaint color2
      CPoint x1 y1 = pointToCPoint p1 dpr
      CPoint x2 y2 = pointToCPoint p2 dpr

  -- Drawing
  moveTo point =
    VG.moveTo c x y
    where
      CPoint x y = pointToCPoint point dpr

  renderLine p1 p2 = do
    VG.moveTo c x1 y1
    VG.lineTo c x2 y2
    where
      CPoint x1 y1 = pointToCPoint p1 dpr
      CPoint x2 y2 = pointToCPoint p2 dpr

  renderLineTo point = do
    VG.lineJoin c VG.Bevel
    VG.lineTo c x y
    where
      CPoint x y = pointToCPoint point dpr

  renderRect rect =
    VG.rect c x y w h
    where
      CRect x y w h = rectToCRect rect dpr

  renderArc point rad angleStart angleEnd winding =
    VG.arc c x y radius start end wind
    where
      CPoint x y = pointToCPoint point dpr
      radius = realToFrac rad
      start = VG.degToRad $ realToFrac angleStart
      end = VG.degToRad $ realToFrac angleEnd
      wind = convertWinding winding

  renderQuadTo p1 p2 =
    VG.quadTo c x1 y1 x2 y2
    where
      CPoint x1 y1 = pointToCPoint p1 dpr
      CPoint x2 y2 = pointToCPoint p2 dpr

  renderEllipse rect =
    VG.ellipse c cx cy rx ry
    where
      CRect x y w h = rectToCRect rect dpr
      cx = x + rx
      cy = y + ry
      rx = w / 2
      ry = h / 2

  -- Text
  renderText rect font fontSize (Align ha va) message = do
    VG.fontFace c (unFont font)
    VG.fontSize c $ realToFrac $ unFontSize fontSize * dpr
    VG.Bounds (VG.V4 x1 _ x2 _) <- VG.textBounds c x y message
    (asc, desc, _) <- VG.textMetrics c

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
      VG.text c tx ty message

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

    VG.fontFace c (unFont font)
    VG.fontSize c $ realToFrac (unFontSize fontSize)
    VG.Bounds (VG.V4 x1 y1 x2 y2) <- VG.textBounds c 0 0 text

    return $ Size (realToFrac $ x2 - x1) (realToFrac $ y2 - y1)

  addImage name w h action imgData = withLock lock $ do
    env <- readIORef envRef

    writeIORef envRef env {
      addedImages = addedImages env |> imageReq
    }
    where
      imageReq = ImageReq name action w h imgData

  renderImage rect name = do
    env <- readIORef envRef
    mapM_ (handleRender c dpr rect) $ M.lookup name (imagesMap env)

handleRender :: VG.Context -> Double -> Rect -> VG.Image -> IO ()
handleRender c dpr rect nvImg = do
  imgPaint <- VG.imagePattern c x y w h 0 nvImg 1
  VG.beginPath c
  VG.rect c x y w h
  VG.fillPaint c imgPaint
  VG.fill c
  where
    CRect x y w h = rectToCRect rect dpr

handlePendingImages :: VG.Context -> ImagesMap -> Seq ImageReq -> IO ImagesMap
handlePendingImages c imagesMap addedImages =
  foldM (handlePendingImage c) imagesMap addedImages

handlePendingImage :: VG.Context -> ImagesMap -> ImageReq -> IO ImagesMap
handlePendingImage c imagesMap imageReq
  | action == Keep && imageExists = trace "A" return imagesMap
  | imageExists = trace "B" $ do
    nvImg <- VG.updateImage c (fromJust nvImg) imgData
    return imagesMap
  | otherwise = trace "C" $ do
    nvImg <- VG.createImageRGBA c cw ch VGI.ImageNearest imgData
    return $ maybe imagesMap insertImage nvImg
  where
    name = _irName imageReq
    action = _irAction imageReq
    cw = fromIntegral $ _irWidth imageReq
    ch = fromIntegral $ _irHeight imageReq
    imgData = _irImgData imageReq
    nvImg = M.lookup name imagesMap
    imageExists = isJust nvImg
    insertImage img = M.insert name img imagesMap

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

-- | Create a new lock.
newLock :: IO Lock
newLock = Lock <$> newMVar ()

-- | Block until the lock is available, then grab it. Something that acquires
-- the lock should at some point subsequently relinquish it with 'releaseLock'.
-- Consider using 'withLock' instead unless you need more fine-grained control.
acquireLock :: Lock -> IO ()
acquireLock (Lock v) = takeMVar v

-- | Release a lock that you have previously acquired with 'acquireLock'.
releaseLock :: Lock -> IO ()
releaseLock (Lock v) = putMVar v ()

-- | Acquire the lock, perform some action while the lock is held, then
-- release the lock. You can use this instead of manually calling 'acquireLock'
-- and 'releaseLock'.
withLock :: Lock -> IO a -> IO a
withLock lock action =
  acquireLock lock *> action `finally` releaseLock lock
