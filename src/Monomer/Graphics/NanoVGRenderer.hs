{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Graphics.NanoVGRenderer (makeRenderer) where

import Control.Monad (foldM, when)
import Data.IORef
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import Data.Text.Foreign (withCStringLen)
import Foreign.C.Types (CFloat)
import Foreign.Ptr
import System.IO.Unsafe

import qualified Control.Concurrent.Lock as L
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified NanoVG as VG
import qualified NanoVG.Internal.Image as VGI

import Monomer.Common.Geometry
import Monomer.Graphics.Renderer
import Monomer.Graphics.Types

type ImagesMap = M.Map String Image

data Action
  = AddKeep
  | AddReplace
  | Update
  | Delete
  deriving (Eq, Show)

data Image = Image {
  _imNvImage :: VG.Image,
  _imCount :: Int
}

data ImageReq = ImageReq {
  _irName :: String,
  _irWidth :: Int,
  _irHeight :: Int,
  _irImgData :: Maybe BS.ByteString,
  _irAction :: Action
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

  lock <- L.new
  envRef <- newIORef $ Env {
    overlays = Seq.empty,
    imagesMap = M.empty,
    addedImages = Seq.empty
  }

  return $ newRenderer c dpr lock envRef

newRenderer :: VG.Context -> Double -> L.Lock -> IORef Env -> Renderer
newRenderer c dpr lock envRef = Renderer {..} where
  beginFrame w h = L.with lock $ do
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
  createOverlay overlay = L.with lock $
    modifyIORef envRef $ \env -> env {
      overlays = overlays env |> overlay
    }

  renderOverlays = L.with lock $ do
    env <- readIORef envRef
    sequence_ $ overlays env
    writeIORef envRef env {
      overlays = Seq.empty
    }

  -- Scissor operations
  setScissor rect = do
    VG.save c
    VG.scissor c x y w h
    where
      CRect x y w h = rectToCRect rect dpr

  resetScissor = do
    VG.resetScissor c
    VG.restore c

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
      radius = realToFrac $ rad * dpr
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
  computeTextSize font fontSize message = unsafePerformIO $ do
    VG.fontFace c (unFont font)
    VG.fontSize c $ realToFrac (unFontSize fontSize)
    VG.Bounds (VG.V4 x1 y1 x2 y2) <- VG.textBounds c 0 0 text

    return $ Size (realToFrac $ x2 - x1) (realToFrac $ y2 - y1)
    where
      text = if message == "" then " " else message

  computeGlyphsPos :: Font -> FontSize -> Text -> Seq GlyphPos
  computeGlyphsPos font fontSize message = unsafePerformIO $ do
    VG.fontFace c (unFont font)
    VG.fontSize c $ realToFrac (unFontSize fontSize)

    glyphs <- textGlyphPositions c 0 0 text
    return $ foldl' (\acc glyph -> acc |> convert glyph) Seq.empty glyphs
    where
      text = if message == "" then " " else message
      convert glyph = GlyphPos {
        _glpXMin = realToFrac $ VG.glyphPosMinX glyph,
        _glpXMax = realToFrac $ VG.glyphPosMaxX glyph,
        _glpW = realToFrac $ VG.glyphPosMaxX glyph - VG.glyphPosMinX glyph
      }

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
      ty | va == ATop = y + asc
         | va == AMiddle = y + (h + th) / 2
         | otherwise = y + h + desc

    when (message /= "") $
      VG.text c tx ty message

    return $ Rect {
      _rX = fromCFloat tx,
      _rY = fromCFloat (ty - asc),
      _rW = fromCFloat tw,
      _rH = fromCFloat (asc - desc)
    }
    where
      CRect x y w h = rectToCRect rect dpr
      fromCFloat val = realToFrac $ val / realToFrac dpr

  addImage name w h replace imgData = addPending lock envRef imageReq where
    action = if replace then AddReplace else AddKeep
    imageReq = ImageReq name w h (Just imgData) action

  updateImage name imgData = addPending lock envRef imageReq where
    imageReq = ImageReq name 0 0 (Just imgData) Update

  deleteImage name = addPending lock envRef imageReq where
    imageReq = ImageReq name 0 0 Nothing Delete

  renderImage rect name = do
    env <- readIORef envRef
    mapM_ (handleRender c dpr rect) $ M.lookup name (imagesMap env)

addPending lock envRef imageReq = L.with lock $ do
  env <- readIORef envRef

  writeIORef envRef env {
    addedImages = addedImages env |> imageReq
  }

handleRender :: VG.Context -> Double -> Rect -> Image -> IO ()
handleRender c dpr rect image = do
  imgPaint <- VG.imagePattern c x y w h 0 nvImg 1
  VG.beginPath c
  VG.rect c x y w h
  VG.fillPaint c imgPaint
  VG.fill c
  where
    CRect x y w h = rectToCRect rect dpr
    nvImg = _imNvImage image

textGlyphPositions
  :: VG.Context -> Double -> Double -> Text -> IO (V.Vector VG.GlyphPosition)
textGlyphPositions c x y text = withCStringLen text $ \(ptr, len) ->
    VG.textGlyphPositions c cx cy ptr (ptr `plusPtr` len) count
  where
    cx = fromIntegral $ round x
    cy = fromIntegral $ round y
    count = fromIntegral $ T.length text

handlePendingImages :: VG.Context -> ImagesMap -> Seq ImageReq -> IO ImagesMap
handlePendingImages c imagesMap addedImages =
  foldM (handlePendingImage c) imagesMap addedImages

handlePendingImage :: VG.Context -> ImagesMap -> ImageReq -> IO ImagesMap
handlePendingImage c imagesMap imageReq
  | action == AddKeep && imageExists =
      return $ imgIncreaseCount name imagesMap
  | action `elem` [AddKeep, AddReplace] && not imageExists = do
      nvImg <- VG.createImageRGBA c cw ch VGI.ImageNearest imgData
      return $ maybe imagesMap (imgInsertNew name imagesMap) nvImg
  | action `elem` [AddReplace, Update] && imageExists = do
      VG.updateImage c (_imNvImage image) imgData
      return imagesMap
  | action == Delete && imageExists = do
      when (_imCount image == 1) $
        VG.deleteImage c (_imNvImage image)
      return $ imgDelete name imagesMap
  | otherwise =
      return imagesMap
  where
    name = _irName imageReq
    action = _irAction imageReq
    cw = fromIntegral $ _irWidth imageReq
    ch = fromIntegral $ _irHeight imageReq
    imgData = fromJust $ _irImgData imageReq
    mimage = M.lookup name imagesMap
    imageExists = isJust mimage
    image = fromJust mimage

imgIncreaseCount :: String -> ImagesMap -> ImagesMap
imgIncreaseCount name imagesMap = newImageMap where
  incCount img = img { _imCount = _imCount img + 1 }
  newImageMap = M.adjust incCount name imagesMap

imgInsertNew :: String -> ImagesMap -> VG.Image -> ImagesMap
imgInsertNew name imagesMap nvImg = newImagesMap where
  image = Image nvImg 0
  newImagesMap = M.insert name image imagesMap

imgDelete :: String -> ImagesMap -> ImagesMap
imgDelete name imagesMap = newImageMap where
  deleteInstance img
    | _imCount img > 1 = Just $ img { _imCount = _imCount img - 1 }
    | otherwise = Nothing
  newImageMap = M.update deleteInstance name imagesMap

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
