{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Graphics.NanoVGRenderer (makeRenderer) where

import Control.Lens ((&), (^.), (.~))
import Control.Monad (foldM, forM_, unless, when)
import Data.Default
import Data.IORef
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Set (Set(..))
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

import Monomer.Core.BasicTypes
import Monomer.Graphics.Types

import qualified Monomer.Lens as L

type ImagesMap = M.Map String Image

data ImageAction
  = ImageAdd
  | ImageUpdate
  | ImageDelete
  deriving (Eq, Show)

data Image = Image {
  _imImageDef :: ImageDef,
  _imNvImage :: VG.Image,
  _imCount :: Int
}

data ImageReq = ImageReq {
  _irName :: String,
  _irSize :: Size,
  _irImgData :: Maybe BS.ByteString,
  _irAction :: ImageAction
}

data Env = Env {
  inFrame :: Bool,
  scissors :: Seq CRect,
  overlays :: Seq (IO ()),
  validFonts :: Set Text,
  imagesMap :: ImagesMap,
  addedImages :: Seq ImageReq
}

data CPoint
  = CPoint CFloat CFloat
  deriving (Eq, Show)

data CRect
  = CRect CFloat CFloat CFloat CFloat
  deriving (Eq, Show)

makeRenderer :: [FontDef] -> Double -> IO Renderer
makeRenderer fonts dpr = do
  c <- VG.createGL3 (Set.fromList [VG.Antialias, VG.StencilStrokes])

  lock <- L.new
  validFonts <- foldM (loadFont c) Set.empty fonts

  when (null validFonts) $
    putStrLn "Could not find any valid fonts. Text will fail to be displayed."

  envRef <- newIORef $ Env {
    inFrame = False,
    scissors = Seq.empty,
    overlays = Seq.empty,
    validFonts = validFonts,
    imagesMap = M.empty,
    addedImages = Seq.empty
  }

  return $ newRenderer c dpr lock envRef

newRenderer :: VG.Context -> Double -> L.Lock -> IORef Env -> Renderer
newRenderer c dpr lock envRef = Renderer {..} where
  beginFrame w h = L.with lock $ do
    newEnv <- handlePendingImages c envRef

    VG.beginFrame c cw ch cdpr

    writeIORef envRef newEnv {
      inFrame = True
    }
    where
      cw = fromIntegral w
      ch = fromIntegral h
      cdpr = realToFrac dpr

  endFrame = L.with lock $ do
    modifyIORef' envRef (\env -> env {
      inFrame = False
    })

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
  setScissor !rect = do
    env <- readIORef envRef
    modifyIORef envRef $ \env -> env {
      scissors = crect <| scissors env
    }
    handleScissor $ scissors env
    where
      crect = rectToCRect rect dpr
      handleScissor scissors
        | Seq.null scissors = setScissorVG c crect
        | otherwise = intersectScissorVG c crect

  resetScissor = do
    modifyIORef envRef $ \env -> env {
      scissors = Seq.drop 1 (scissors env)
    }
    env <- readIORef envRef
    handleScissor $ scissors env
    where
      handleScissor scissors
        | Seq.null scissors = VG.resetScissor c
        | otherwise = applyScissorsVG c scissors

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
  moveTo !point =
    VG.moveTo c x y
    where
      CPoint x y = pointToCPoint point dpr

  renderLine p1 p2 = do
    VG.moveTo c x1 y1
    VG.lineTo c x2 y2
    where
      CPoint x1 y1 = pointToCPoint p1 dpr
      CPoint x2 y2 = pointToCPoint p2 dpr

  renderLineTo !point = do
    VG.lineJoin c VG.Bevel
    VG.lineTo c x y
    where
      CPoint x y = pointToCPoint point dpr

  renderRect !rect =
    VG.rect c x y w h
    where
      CRect x y w h = rectToCRect rect dpr

  renderArc !point rad angleStart angleEnd winding =
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

  renderEllipse !rect =
    VG.ellipse c cx cy rx ry
    where
      CRect x y w h = rectToCRect rect dpr
      cx = x + rx
      cy = y + ry
      rx = w / 2
      ry = h / 2

  -- Text
  computeTextMetrics !font !fontSize = unsafePerformIO $ do
    setFont c envRef dpr font fontSize
    (asc, desc, lineh) <- getTextMetrics c

    return $ TextMetrics {
      _txmAsc = asc / dpr,
      _txmDesc = desc / dpr,
      _txmLineH = lineh / dpr
    }

  computeTextSize !font !fontSize !text = unsafePerformIO $ do
    setFont c envRef dpr font fontSize
    (x1, y1, x2, y2) <- getTextBounds c 0 0 text

    return $ Size (realToFrac (x2 - x1) / dpr) (realToFrac (y2 - y1) / dpr)

  computeGlyphsPos :: Font -> FontSize -> Text -> Seq GlyphPos
  computeGlyphsPos !font !fontSize !message = unsafePerformIO $ do
    -- Glyph position is usually used in local coord calculations, ignoring dpr
    setFont c envRef dpr font fontSize

    glyphsPos <- fmap vecToSeq (textGlyphPositions c 0 0 message)

    return $ foldl' reducer Seq.empty (Seq.zip glyphs glyphsPos)
    where
      vecToSeq = foldl' (|>) Seq.empty
      glyphs = Seq.fromList $ T.unpack message
      reducer acc glyph = acc |> convert glyph
      convert (glyph, pos) = GlyphPos {
        _glpGlyph = glyph,
        _glpXMin = realToFrac (VG.glyphPosMinX pos) / dpr,
        _glpXMax = realToFrac (VG.glyphPosMaxX pos) / dpr,
        _glpW = realToFrac (VG.glyphPosMaxX pos - VG.glyphPosMinX pos) / dpr
      }

  renderText !point font fontSize message = do
    setFont c envRef dpr font fontSize

    when (message /= "") $
      VG.text c tx ty message
    where
      CPoint tx ty = pointToCPoint point dpr

  getImage name = unsafePerformIO $ do
    env <- readIORef envRef
    let image = M.lookup name (imagesMap env)
    return $ fmap _imImageDef image

  addImage name size imgData = L.with lock (addPending c envRef imgReq) where
    imgReq = ImageReq name size (Just imgData) ImageAdd

  updateImage name size imgData = L.with lock (addPending c envRef imgReq) where
    imgReq = ImageReq name size (Just imgData) ImageUpdate

  deleteImage name = L.with lock (addPending c envRef imgReq) where
    imgReq = ImageReq name def Nothing ImageDelete

  renderImage name rect alpha = do
    env <- readIORef envRef
    mapM_ (handleImageRender c dpr rect alpha) $ M.lookup name (imagesMap env)

  renderNewImage name size imgData rect alpha = L.with lock $ do
    addPending c envRef $ ImageReq name size (Just imgData) ImageUpdate
    newEnv <- handlePendingImages c envRef
    mapM_ (handleImageRender c dpr rect alpha) $ M.lookup name (imagesMap newEnv)

    writeIORef envRef newEnv

loadFont :: VG.Context -> Set Text -> FontDef -> IO (Set Text)
loadFont c fonts (FontDef name path) = do
  res <- VG.createFont c name (VG.FileName path)
  case res of
    Just{} -> return $ Set.insert name fonts
    _ -> putStrLn ("Failed to load font: " ++ T.unpack name) >> return fonts

setFont :: VG.Context -> IORef Env -> Double -> Font -> FontSize -> IO ()
setFont c envRef dpr (Font name) (FontSize size) = do
  env <- readIORef envRef
  handleSetFont (validFonts env)
  where
    handleSetFont validFonts
      | Set.member name validFonts = do
          VG.fontFace c name
          VG.fontSize c $ realToFrac $ size * dpr
      | otherwise = return ()

getTextMetrics :: VG.Context -> IO (Double, Double, Double)
getTextMetrics c = do
  (asc, desc, lineh) <- VG.textMetrics c
  return (realToFrac asc, realToFrac desc, realToFrac lineh)

getTextBounds
  :: VG.Context
  -> Double
  -> Double
  -> Text
  -> IO (Double, Double, Double, Double)
getTextBounds c x y "" = do
  (asc, desc, lineh) <- VG.textMetrics c
  return (x, y, 0, realToFrac lineh)
getTextBounds c x y text = do
  VG.Bounds (VG.V4 x1 y1 x2 y2) <- VG.textBounds c cx cy text
  return (realToFrac x1, realToFrac y1, realToFrac x2, realToFrac y2)
  where
    msg = if text == "" then " " else text
    cx = realToFrac x
    cy = realToFrac y

addPending :: VG.Context -> IORef Env -> ImageReq -> IO ()
addPending c envRef imageReq = do
  env <- readIORef envRef

  writeIORef envRef env {
    addedImages = addedImages env |> imageReq
  }

handleImageRender :: VG.Context -> Double -> Rect -> Double -> Image -> IO ()
handleImageRender c dpr rect alpha image = do
  imgPaint <- VG.imagePattern c x y w h 0 nvImg calpha
  VG.beginPath c
  VG.rect c x y w h
  VG.fillPaint c imgPaint
  VG.fill c
  where
    CRect x y w h = rectToCRect rect dpr
    nvImg = _imNvImage image
    calpha = realToFrac alpha

textGlyphPositions
  :: VG.Context -> Double -> Double -> Text -> IO (V.Vector VG.GlyphPosition)
textGlyphPositions c x y "" = return V.empty
textGlyphPositions c x y text = withCStringLen text $ \(ptr, len) ->
    VG.textGlyphPositions c cx cy ptr (ptr `plusPtr` len) count
  where
    cx = fromIntegral $ round x
    cy = fromIntegral $ round y
    count = fromIntegral $ T.length text

handlePendingImages :: VG.Context -> IORef Env -> IO Env
handlePendingImages c envRef = do
  env <- readIORef envRef
  newImagesMap <- foldM (handlePendingImage c) (imagesMap env) (addedImages env)
  return env {
    imagesMap = newImagesMap,
    addedImages = Seq.empty
  }

handlePendingImage :: VG.Context -> ImagesMap -> ImageReq -> IO ImagesMap
handlePendingImage c imagesMap imageReq
  | action == ImageAdd && imageExists =
      return $ imgIncreaseCount name imagesMap
  | action `elem` [ImageAdd, ImageUpdate] && not imageExists = do
      nvImg <- VG.createImageRGBA c cw ch VGI.ImageNearest imgData
      return $ maybe imagesMap (imgInsertNew name imgDef imagesMap) nvImg
  | action == ImageUpdate && imageExists && sizeMatches = do
      VG.updateImage c (_imNvImage image) imgData
      return imagesMap
  | action == ImageDelete && imageExists = do
      when (_imCount image == 1) $
        VG.deleteImage c (_imNvImage image)
      return $ imgDelete name imagesMap
  | otherwise =
      return imagesMap
  where
    name = _irName imageReq
    action = _irAction imageReq
    size = _irSize imageReq
    cw = round (size ^. L.w)
    ch = round (size ^. L.h)
    imgData = fromJust $ _irImgData imageReq
    imgDef = ImageDef name size imgData
    mimage = M.lookup name imagesMap
    imageExists = isJust mimage
    image = fromJust mimage
    sizeMatches = size == _imImageDef image ^. L.size

imgIncreaseCount :: String -> ImagesMap -> ImagesMap
imgIncreaseCount name imagesMap = newImageMap where
  incCount img = img { _imCount = _imCount img + 1 }
  newImageMap = M.adjust incCount name imagesMap

imgInsertNew :: String -> ImageDef -> ImagesMap -> VG.Image -> ImagesMap
imgInsertNew name imageDef imagesMap nvImg = newImagesMap where
  image = Image imageDef nvImg 0
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

setScissorVG :: VG.Context -> CRect -> IO ()
setScissorVG c crect = VG.scissor c x y w h where
  CRect x y w h = crect

intersectScissorVG :: VG.Context -> CRect -> IO ()
intersectScissorVG c crect = VG.intersectScissor c x y w h where
  CRect x y w h = crect

applyScissorsVG :: VG.Context -> Seq CRect -> IO ()
applyScissorsVG c Empty = return ()
applyScissorsVG c (x :<| xs) = do
  setScissorVG c x
  mapM_ (intersectScissorVG c) xs
