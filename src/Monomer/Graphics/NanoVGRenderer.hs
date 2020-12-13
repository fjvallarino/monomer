{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Graphics.NanoVGRenderer (makeRenderer) where

import Debug.Trace

import Control.Lens ((&), (^.), (.~), (%~))
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
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

import qualified Monomer.Lens as L

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

data Offset = Offset {
  _offAccum :: Point,
  _offCurrent :: Point
} deriving (Eq, Show)

instance Default Offset where
  def = Offset def def

data Scissor = Scissor {
  _scsArea :: Rect,
  _scsOffset :: Offset
} deriving (Eq, Show)

instance Default Scissor where
  def = Scissor def def

data Env = Env {
  _envScissors :: Seq Scissor,
  _envOffsets :: Seq Offset,
  _envOverlays :: Seq (IO ()),
  _envValidFonts :: Set Text,
  _envImagesMap :: ImagesMap,
  _envAddedImages :: Seq ImageReq
}

instance Default Env where
  def = Env {
    _envScissors = Seq.empty,
    _envOffsets = Seq.empty,
    _envOverlays = Seq.empty,
    _envValidFonts = Set.empty,
    _envImagesMap = M.empty,
    _envAddedImages = Seq.empty
  }

data CPoint
  = CPoint CFloat CFloat
  deriving (Eq, Show)

data CRect
  = CRect CFloat CFloat CFloat CFloat
  deriving (Eq, Show)

makeLensesWith abbreviatedFields ''Scissor
makeLensesWith abbreviatedFields ''Offset
makeLensesWith abbreviatedFields ''Env

makeRenderer :: [FontDef] -> Double -> IO Renderer
makeRenderer fonts dpr = do
  c <- VG.createGL3 (Set.fromList [VG.Antialias, VG.StencilStrokes, VG.Debug])

  lock <- L.new
  loadedFonts <- if null fonts
    then useDefaultFont c
    else foldM (loadFont c) Set.empty fonts

  envRef <- newIORef $ def & validFonts .~ loadedFonts

  return $ newRenderer c dpr lock envRef

newRenderer :: VG.Context -> Double -> L.Lock -> IORef Env -> Renderer
newRenderer c dpr lock envRef = Renderer {..} where
  beginFrame w h = L.with lock $ do
    env <- readIORef envRef
    newMap <- handlePendingImages c (env ^. imagesMap) (env ^. addedImages)
    writeIORef envRef $ env
      & scissors .~ Seq.empty
      & offsets .~ Seq.empty
      & overlays .~ Seq.empty
      & imagesMap .~ newMap
      & addedImages .~ Seq.empty

    putStrLn "Frame started"

    VG.resetTransform c
    VG.resetScissor c
    VG.beginFrame c cw ch cdpr
    where
      cw = fromIntegral w
      ch = fromIntegral h
      cdpr = realToFrac dpr

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
    modifyIORef' envRef $ \env -> env & overlays %~ (|> overlay)

  renderOverlays = L.with lock $ do
    env <- readIORef envRef
    sequence_ $ env ^. overlays
    writeIORef envRef $ env & overlays .~ Seq.empty

  -- Scissor operations
  setScissor rect = do
    env <- readIORef envRef
    let newScissor = scissorPair env
    modifyIORef' envRef $ \env -> env & scissors %~ (newScissor <|)
    setScissorVG c (newRectC newScissor)
    where
      scissorPair env = Scissor newRect latestOffset where
        Scissor prevScissor prevOffset = headOrVal (Scissor rect def) (env ^. scissors)
        latestOffset = headOrVal def (env ^. offsets)
        newOffset
          | latestOffset == prevOffset = def
          | otherwise = latestOffset
        oldRect = moveRect (negPoint (newOffset ^. current)) prevScissor
        tempRect = moveRect (negPoint (newOffset ^. current)) rect
        newRect = fromMaybe def (intersectRects oldRect tempRect)
      newRectC sc = rectToCRect (sc ^. area) dpr

  resetScissor = do
    prevEnv <- readIORef envRef
    modifyIORef' envRef $ \env -> env & scissors %~ Seq.drop 1
    env <- readIORef envRef
    if null (env ^. scissors)
      then VG.resetScissor c
      else setScissorVG c (currRect prevEnv)
    where
      currRect env = newRect where
        (Scissor r1 o1, Scissor r2 o2) = case env ^. scissors of
          it1 :<| it2 :<| its -> (it1, it2)
          it1 :<| its -> (it1, def)
          _ -> (def, def)
        --newRect = rectToCRect (fst $ headOrVal def (env ^. scissors)) dpr
        o3 = negPoint (o1 ^. accum) -- addPoint o2 (negPoint o1)
        newRect = rectToCRect (moveRect o3 r2) dpr

  -- Transforms
  pushTranslation p = do
    env <- readIORef envRef
    setTranslationVG c (newOffsetC env)
    modifyIORef' envRef $ \env -> env & offsets %~ (newOffset env <|)
    where
      newOffset env = offset where
        prevOffset = headOrVal def (env ^. offsets)
        offset = Offset {
          _offAccum = addPoint (prevOffset ^. accum) p,
          _offCurrent = p
        }
      newOffsetC env = pointToCPoint (newOffset env ^. current) dpr where

  popTranslation = do
    prevEnv <- readIORef envRef
    modifyIORef envRef $ \env -> env & offsets %~ Seq.drop 1
    env <- readIORef envRef
    if null (env ^. offsets)
      then VG.resetTransform c
      else setTranslationVG c (reverseOffset prevEnv)
    where
      reverseOffset env = pointToCPoint (negPoint curr) dpr where
        Offset accum curr = (headOrVal def (env ^. offsets))

  resetTranslation = do
    modifyIORef envRef $ \env -> env & offsets .~ Seq.empty
    VG.resetTransform c

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
    return $ M.member name (env ^. imagesMap)

  renderImage name rect alpha = do
    env <- readIORef envRef
    mapM_ (handleImageRender c dpr rect alpha) $ M.lookup name (env ^. imagesMap)

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
  handleSetFont (env ^. validFonts)
  where
    handleSetFont fonts
      | Set.member name fonts = do
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

addPending :: L.Lock -> IORef Env -> ImageReq -> IO ()
addPending lock envRef imageReq = L.with lock $ do
  env <- readIORef envRef

  writeIORef envRef $ env & addedImages %~ (|> imageReq)

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

{--
intersectScissorVG :: VG.Context -> CRect -> IO ()
intersectScissorVG c crect = VG.intersectScissor c x y w h where
  CRect x y w h = crect

applyScissorsVG :: VG.Context -> Seq CRect -> IO ()
applyScissorsVG c Empty = return ()
applyScissorsVG c (x :<| xs) = do
  setScissorVG c x
  mapM_ (intersectScissorVG c) xs
--}

setTranslationVG :: VG.Context -> CPoint -> IO ()
--setTranslationVG c (CPoint x y) = VG.translate c x y
setTranslationVG c (CPoint x y) = do
--  VG.resetTransform c
  VG.translate c x y

headOrVal :: a -> Seq a -> a
headOrVal val items = offset where
  offset = fromMaybe val (Seq.lookup 0 items)
