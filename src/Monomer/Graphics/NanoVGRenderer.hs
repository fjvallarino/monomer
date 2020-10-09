{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Graphics.NanoVGRenderer (makeRenderer) where

import Control.Monad (foldM, unless, when)
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
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified NanoVG as VG
import qualified NanoVG.Internal.Image as VGI

import Monomer.Core.BasicTypes
import Monomer.Graphics.Types

import qualified Monomer.Graphics.RobotoRegular as RBTReg

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
  _irWidth :: Double,
  _irHeight :: Double,
  _irImgData :: Maybe BS.ByteString,
  _irAction :: Action
}

data Env = Env {
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
  c <- VG.createGL3 (Set.fromList [VG.Antialias, VG.StencilStrokes, VG.Debug])

  lock <- L.new
  validFonts <- if null fonts
    then useDefaultFont c
    else foldM (loadFont c) Set.empty fonts

  envRef <- newIORef $ Env {
    scissors = Seq.empty,
    overlays = Seq.empty,
    validFonts = validFonts,
    imagesMap = M.empty,
    addedImages = Seq.empty
  }

  return $ newRenderer c dpr lock envRef

newRenderer :: VG.Context -> Double -> L.Lock -> IORef Env -> Renderer
newRenderer c dpr lock envRef = Renderer {..} where
  defaultDpr = 1

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
  computeTextSize font fontSize message = unsafePerformIO $ do
    setFont c envRef defaultDpr font fontSize
    VG.Bounds (VG.V4 x1 y1 x2 y2) <- VG.textBounds c 0 0 text

    return $ Size (realToFrac $ x2 - x1) (realToFrac $ y2 - y1)
    where
      text = if message == "" then " " else message

  computeTextMetrics !rect font fontSize (Align ha va) msg = unsafePerformIO $ do
    setFont c envRef defaultDpr font fontSize
    VG.Bounds (VG.V4 x1 y1 x2 y2) <- VG.textBounds c x y msg
    (asc, desc, lineh) <- VG.textMetrics c

    let
      tw = x2 - x1
      th = lineh
      tx | ha == ALeft = x
         | ha == ACenter = x + (w - tw) / 2
         | otherwise = x + (w - tw)
      ty | va == ATop = y + asc
         | va == AMiddle = y + h + desc - (h - th) / 2
         | otherwise = y + h + desc

    return $ TextMetrics {
      _txmX = fromCFloat tx,
      _txmY = fromCFloat (ty - th),
      _txmW = fromCFloat tw,
      _txmH = fromCFloat th,
      _txhAsc = fromCFloat asc,
      _txhDesc = fromCFloat desc
    }
    where
      CRect x y w h = rectToCRect rect defaultDpr
      fromCFloat val = realToFrac val / realToFrac defaultDpr

  computeGlyphsPos :: Font -> FontSize -> Text -> Seq GlyphPos
  computeGlyphsPos font fontSize message = unsafePerformIO $ do
    -- Glyph position is usually used in local coord calculations, ignoring dpr
    setFont c envRef defaultDpr font fontSize

    glyphs <- textGlyphPositions c 0 0 text
    return $ foldl' (\acc glyph -> acc |> convert glyph) Seq.empty glyphs
    where
      text = if message == "" then " " else message
      convert glyph = GlyphPos {
        _glpXMin = realToFrac $ VG.glyphPosMinX glyph,
        _glpXMax = realToFrac $ VG.glyphPosMaxX glyph,
        _glpW = realToFrac $ VG.glyphPosMaxX glyph - VG.glyphPosMinX glyph
      }

  renderText !point font fontSize message = do
    setFont c envRef dpr font fontSize

    when (message /= "") $
      VG.text c tx ty message
    where
      CPoint tx ty = pointToCPoint point dpr

  addImage name action size imgData = addPending lock envRef imageReq where
    newAction = case action of
      ImageAddKeep -> AddKeep
      _ -> AddReplace
    Size w h = size
    imageReq = ImageReq name w h (Just imgData) newAction

  updateImage name imgData = addPending lock envRef imageReq where
    imageReq = ImageReq name 0 0 (Just imgData) Update

  deleteImage name = addPending lock envRef imageReq where
    imageReq = ImageReq name 0 0 Nothing Delete

  existsImage name = unsafePerformIO $ do
    env <- readIORef envRef
    return $ M.member name (imagesMap env)

  renderImage name rect alpha = do
    env <- readIORef envRef
    mapM_ (handleImageRender c dpr rect alpha) $ M.lookup name (imagesMap env)

useDefaultFont :: VG.Context -> IO (Set Text)
useDefaultFont c = do
  font <- VG.createFontMem c defaultFontName fontMem

  when (isNothing font) $
    putStrLn "Failed to load default font"

  return $ Set.singleton defaultFontName
  where
    fontMem = B64.decodeLenient RBTReg.fontBase64

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

addPending :: L.Lock -> IORef Env -> ImageReq -> IO ()
addPending lock envRef imageReq = L.with lock $ do
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
    cw = round $ _irWidth imageReq
    ch = round $ _irHeight imageReq
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
