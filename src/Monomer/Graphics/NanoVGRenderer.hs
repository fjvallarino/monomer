{-|
Module      : Monomer.Graphics.NanoVGRenderer
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Renderer based on the nanovg library.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Monomer.Graphics.NanoVGRenderer (makeRenderer) where

import Control.Lens ((&), (^.), (.~))
import Control.Monad (foldM, forM_, unless, when)
import Data.Default
import Data.Functor ((<&>))
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

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified NanoVG as VG
import qualified NanoVG.Internal.Image as VGI

import Monomer.Common
import Monomer.Graphics.Types

import qualified Monomer.Common.Lens as L
import qualified Monomer.Graphics.Lens as L

type ImagesMap = M.Map Text Image

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
  _irName :: Text,
  _irSize :: Size,
  _irImgData :: Maybe BS.ByteString,
  _irAction :: ImageAction,
  _irFlags :: [ImageFlag]
}

data Env = Env {
  overlays :: Seq (IO ()),
  tasksRaw :: Seq (IO ()),
  overlaysRaw :: Seq (IO ()),
  validFonts :: Set Text,
  imagesMap :: ImagesMap,
  addedImages :: Seq ImageReq
}

data CSize
  = CSize CFloat CFloat
  deriving (Eq, Show)

data CPoint
  = CPoint CFloat CFloat
  deriving (Eq, Show)

data CRect
  = CRect CFloat CFloat CFloat CFloat
  deriving (Eq, Show)

-- | Creates a nanovg based renderer.
makeRenderer
  :: [FontDef]    -- ^ The font definitions.
  -> Double       -- ^ The device pixel rate.
  -> IO Renderer  -- ^ The created renderer.
makeRenderer fonts dpr = do
  c <- VG.createGL3 (Set.fromList [VG.Antialias, VG.StencilStrokes])

  validFonts <- foldM (loadFont c) Set.empty fonts

  when (null validFonts) $
    putStrLn "Could not find any valid fonts. Text will fail to be displayed."

  envRef <- newIORef $ Env {
    overlays = Seq.empty,
    tasksRaw = Seq.empty,
    overlaysRaw = Seq.empty,
    validFonts = validFonts,
    imagesMap = M.empty,
    addedImages = Seq.empty
  }

  return $ newRenderer c dpr envRef

newRenderer :: VG.Context -> Double -> IORef Env -> Renderer
newRenderer c rdpr envRef = Renderer {..} where
  dpr = 1

  beginFrame w h = do
    newEnv <- handlePendingImages c envRef

    VG.beginFrame c cw ch cdpr
    where
      cw = realToFrac (w / dpr)
      ch = realToFrac (h / dpr)
      cdpr = realToFrac rdpr

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
  createOverlay overlay =
    modifyIORef envRef $ \env -> env {
      overlays = overlays env |> overlay
    }

  renderOverlays = do
    env <- readIORef envRef
    sequence_ $ overlays env
    writeIORef envRef env {
      overlays = Seq.empty
    }

  -- Raw tasks
  createRawTask task =
    modifyIORef envRef $ \env -> env {
      tasksRaw = tasksRaw env |> task
    }

  renderRawTasks = do
    env <- readIORef envRef
    sequence_ $ tasksRaw env
    writeIORef envRef env {
      tasksRaw = Seq.empty
    }

  -- Raw overlays
  createRawOverlay overlay =
    modifyIORef envRef $ \env -> env {
      overlaysRaw = overlaysRaw env |> overlay
    }

  renderRawOverlays = do
    env <- readIORef envRef
    sequence_ $ overlaysRaw env
    writeIORef envRef env {
      overlaysRaw = Seq.empty
    }

  -- Scissor
  intersectScissor rect = do
    VG.intersectScissor c cx cy cw ch
    where
      CRect cx cy cw ch = rectToCRect rect dpr

  -- Translation
  setTranslation offset = do
    VG.translate c tx ty
    where
      CPoint tx ty = pointToCPoint offset dpr

  -- Scale
  setScale point = do
    VG.scale c sx sy
    where
      sx = realToFrac (point ^. L.x)
      sy = realToFrac (point ^. L.y)

  -- Rotation
  setRotation angle = do
    VG.rotate c cangle
    where
      cangle = VG.degToRad $ realToFrac angle

  -- Alpha
  setGlobalAlpha alpha = do
    VG.globalAlpha c calpha
    where
      calpha = max 0 . min 1 $ realToFrac alpha

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

  addImage name size imgData flags = addPending c envRef req where
    req = ImageReq name size (Just imgData) ImageAdd flags

  updateImage name size imgData = addPending c envRef req where
    req = ImageReq name size (Just imgData) ImageUpdate []

  deleteImage name = addPending c envRef req where
    req = ImageReq name def Nothing ImageDelete []

  renderImage name rect alpha = do
    env <- readIORef envRef
    mapM_ (handleImageRender c dpr rect alpha) $ M.lookup name (imagesMap env)

  renderNewImage name rect alpha size imgData flags = do
    addPending c envRef $ ImageReq name size (Just imgData) ImageUpdate flags
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

addPending :: VG.Context -> IORef Env -> ImageReq -> IO ()
addPending c envRef imageReq = do
  env <- readIORef envRef

  writeIORef envRef env {
    addedImages = addedImages env |> imageReq
  }

handleImageRender :: VG.Context -> Double -> Rect -> Double -> Image -> IO ()
handleImageRender c dpr rect alpha image = do
  imgPaint <- VG.imagePattern c x y iw ih 0 nvImg calpha
  VG.beginPath c
  VG.rect c x y w h
  VG.fillPaint c imgPaint
  VG.fill c
  where
    CRect x y w h = rectToCRect rect dpr
    imgDef = _imImageDef image
    imgFlags = _idfFlags imgDef
    CSize dw dh = sizeToCSize (_idfSize imgDef) dpr
    iw = if ImageRepeatX `elem` imgFlags then dw else w
    ih = if ImageRepeatY `elem` imgFlags then dh else h
    nvImg = _imNvImage image
    calpha = realToFrac alpha

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
      -- Attempt to create image. If it fails, remove existing images and retry.
      -- Ideally only LRU should be removed.
      tmpImg <- createImage
      (newImgMap, nvImg) <- if isNothing tmpImg
        then clearImagesMap c imagesMap >> createImage <&> (M.empty, )
        else return (imagesMap, tmpImg)
      return $ maybe newImgMap (imgInsertNew name imgDef newImgMap) nvImg
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
    imgFlags = _irFlags imageReq
    flags = Set.fromList (toVGImgFlag <$> imgFlags)
    imgDef = ImageDef name size imgData imgFlags
    mimage = M.lookup name imagesMap
    imageExists = isJust mimage
    image = fromJust mimage
    sizeMatches = size == _imImageDef image ^. L.size
    createImage = VG.createImageRGBA c cw ch flags imgData

toVGImgFlag :: ImageFlag -> VGI.ImageFlags
toVGImgFlag ImageNearest = VGI.ImageNearest
toVGImgFlag ImageRepeatX = VGI.ImageRepeatx
toVGImgFlag ImageRepeatY = VGI.ImageRepeaty

imgIncreaseCount :: Text -> ImagesMap -> ImagesMap
imgIncreaseCount name imagesMap = newImageMap where
  incCount img = img { _imCount = _imCount img + 1 }
  newImageMap = M.adjust incCount name imagesMap

imgInsertNew :: Text -> ImageDef -> ImagesMap -> VG.Image -> ImagesMap
imgInsertNew name imageDef imagesMap nvImg = newImagesMap where
  image = Image imageDef nvImg 0
  newImagesMap = M.insert name image imagesMap

imgDelete :: Text -> ImagesMap -> ImagesMap
imgDelete name imagesMap = newImageMap where
  deleteInstance img
    | _imCount img > 1 = Just $ img { _imCount = _imCount img - 1 }
    | otherwise = Nothing
  newImageMap = M.update deleteInstance name imagesMap

clearImagesMap :: VG.Context -> ImagesMap -> IO ()
clearImagesMap c imagesMap = do
  putStrLn "Clearing images map"
  forM_ (M.elems imagesMap) $ \image ->
    VG.deleteImage c (_imNvImage image)

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

sizeToCSize :: Size -> Double -> CSize
sizeToCSize (Size w h) dpr = CSize cw ch where
  cw = realToFrac $ w * dpr
  ch = realToFrac $ h * dpr

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
