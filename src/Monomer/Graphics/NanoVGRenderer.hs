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
{-# LANGUAGE StrictData #-}
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
  imagesMap :: ImagesMap
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
    imagesMap = M.empty
  }

  return $ newRenderer c dpr envRef

newRenderer :: VG.Context -> Double -> IORef Env -> Renderer
newRenderer c rdpr envRef = Renderer {..} where
  {-
  rdpr is used to let nanovg know the real device pixel rate.
  dpr is set to 1 to disable all NanoVGRenderer internal calculations.
  -}
  dpr = 1

  beginFrame w h = do
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

  -- Winding
  setPathWinding winding = do
    VG.pathWinding c cwinding
    where
      cwinding = if winding == CW then 0 else 1

  -- Strokes
  stroke =
    VG.stroke c

  setStrokeWidth width =
    VG.strokeWidth c (realToFrac $ width * dpr)

  setStrokeColor color =
    VG.strokeColor c (colorToPaint color)

  setStrokeLinearGradient p1 p2 color1 color2 = do
    gradient <- makeLinearGradient c dpr p1 p2 color1 color2
    VG.strokePaint c gradient

  setStrokeRadialGradient p1 rad1 rad2 color1 color2 = do
    gradient <- makeRadialGradient c dpr p1 rad1 rad2 color1 color2
    VG.strokePaint c gradient

  setStrokeImagePattern name topLeft size angle alpha = do
    env <- readIORef envRef

    forM_ (M.lookup name (imagesMap env)) $ \image -> do
      imgPattern <- makeImagePattern c dpr image topLeft size angle alpha
      VG.strokePaint c imgPattern

  -- Fill
  fill =
    VG.fill c

  setFillColor color =
    VG.fillColor c (colorToPaint color)

  setFillLinearGradient p1 p2 color1 color2 = do
    gradient <- makeLinearGradient c dpr p1 p2 color1 color2
    VG.fillPaint c gradient

  setFillRadialGradient p1 rad1 rad2 color1 color2 = do
    gradient <- makeRadialGradient c dpr p1 rad1 rad2 color1 color2
    VG.fillPaint c gradient

  setFillImagePattern name topLeft size angle alpha = do
    env <- readIORef envRef

    forM_ (M.lookup name (imagesMap env)) $ \image -> do
      imgPattern <- makeImagePattern c dpr image topLeft size angle alpha
      VG.fillPaint c imgPattern

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

  renderRoundedRect !rect tl tr br bl =
    VG.roundedRectVarying c x y w h ctl ctr cbr cbl
    where
      CRect x y w h = rectToCRect rect dpr
      ctl = realToFrac tl
      ctr = realToFrac tr
      cbr = realToFrac br
      cbl = realToFrac bl

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
  renderText !point font fontSize fontSpaceH message = do
    setFont c envRef dpr font fontSize fontSpaceH

    when (message /= "") $
      VG.text c tx ty message
    where
      CPoint tx ty = pointToCPoint point dpr

  getImage name = do
    env <- readIORef envRef
    let image = M.lookup name (imagesMap env)
    return $ fmap _imImageDef image

  addImage name size imgData flags = do
    processImgReq c envRef req
    where
      req = ImageReq name size (Just imgData) ImageAdd flags

  updateImage name size imgData = do
    processImgReq c envRef req
    where
      req = ImageReq name size (Just imgData) ImageUpdate []

  deleteImage name = do
    processImgReq c envRef req
    where
    req = ImageReq name def Nothing ImageDelete []

loadFont :: VG.Context -> Set Text -> FontDef -> IO (Set Text)
loadFont c fonts (FontDef name path) = do
  res <- VG.createFont c name (VG.FileName path)
  case res of
    Just{} -> return $ Set.insert name fonts
    _ -> putStrLn ("Failed to load font: " ++ T.unpack name) >> return fonts

setFont
  :: VG.Context
  -> IORef Env
  -> Double
  -> Font
  -> FontSize
  -> FontSpace
  -> IO ()
setFont c envRef dpr (Font name) (FontSize size) (FontSpace spaceH) = do
  env <- readIORef envRef
  handleSetFont (validFonts env)
  where
    handleSetFont validFonts
      | Set.member name validFonts = do
          VG.fontFace c name
          VG.fontSize c $ realToFrac $ size * dpr
          VG.textLetterSpacing c $ realToFrac $ spaceH * dpr
      | otherwise = return ()

makeLinearGradient
  :: VG.Context -> Double -> Point -> Point -> Color -> Color -> IO VG.Paint
makeLinearGradient c dpr p1 p2 color1 color2 = do
  VG.linearGradient c x1 y1 x2 y2 col1 col2
  where
    CPoint x1 y1 = pointToCPoint p1 dpr
    CPoint x2 y2 = pointToCPoint p2 dpr
    col1 = colorToPaint color1
    col2 = colorToPaint color2

makeRadialGradient
  :: VG.Context -> Double -> Point -> Double -> Double -> Color -> Color -> IO VG.Paint
makeRadialGradient c dpr center rad1 rad2 color1 color2 = do
  VG.radialGradient c cx cy crad1 crad2 col1 col2
  where
    CPoint cx cy = pointToCPoint center dpr
    crad1 = realToFrac rad1
    crad2 = realToFrac rad2
    col1 = colorToPaint color1
    col2 = colorToPaint color2

makeImagePattern
  :: VG.Context -> Double -> Image -> Point -> Size -> Double -> Double -> IO VG.Paint
makeImagePattern c dpr image topLeft size angle alpha = do
  VG.imagePattern c x y w h cangle nvImg calpha
  where
    CPoint x y = pointToCPoint topLeft dpr
    CSize w h = sizeToCSize size dpr
    cangle = realToFrac angle
    calpha = realToFrac alpha
    nvImg = _imNvImage image

processImgReq :: VG.Context -> IORef Env -> ImageReq -> IO ()
processImgReq c envRef imageReq = do
  env <- readIORef envRef
  newImgMap <- handlePendingImage c (imagesMap env) imageReq

  writeIORef envRef $ env {
    imagesMap = newImgMap
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
    createImage = VG.createImageRGBA c cw ch flags imgData

    mimage = M.lookup name imagesMap
    imageExists = isJust mimage
    image = fromJust mimage
    sizeMatches = size == _imImageDef image ^. L.size

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
  image = Image imageDef nvImg 1
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
