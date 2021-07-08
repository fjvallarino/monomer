{-|
Module      : Monomer.Widgets.Singles.Image
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Displays an image from local storage or a url.

Notes:

- Depending on the type of image fit chosen and the assigned viewport, some
  space may remain unused. The alignment options exist to handle this situation.

Configs:

- transparency: the alpha to apply when rendering the image.
- onLoadError: an event to report a load error.
- imageNearest: apply nearest filtering when stretching an image.
- imageRepeatX: repeat the image across the x coordinate.
- imageRepeatY: repeat the image across the y coordinate.
- fitNone: does not perform any streching if the size does not match viewport.
- fitFill: stretches the image to match the viewport.
- fitWidth: stretches the image to match the viewport width. Maintains ratio.
- fitHeight: stretches the image to match the viewport height. Maintains ratio.
- alignLeft: aligns left if extra space is available.
- alignRight: aligns right if extra space is available.
- alignCenter: aligns center if extra space is available.
- alignTop: aligns top if extra space is available.
- alignMiddle: aligns middle if extra space is available.
- alignBottom: aligns bottom if extra space is available.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widgets.Singles.Image (
  ImageLoadError(..),
  image,
  image_,
  imageMem,
  imageMem_
) where

import Codec.Picture (DynamicImage, Image(..))
import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Exception (try)
import Control.Lens ((&), (^.), (.~), (%~), (?~), at)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Default
import Data.Map (Map)
import Data.Maybe
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Typeable (cast)
import Data.Vector.Storable.ByteString (vectorToByteString)
import GHC.Generics
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..))
import Network.Wreq

import qualified Codec.Picture as Pic
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Network.Wreq as Wreq

import Monomer.Widgets.Single

import qualified Monomer.Lens as L

data ImageFit
  = FitNone
  | FitFill
  | FitWidth
  | FitHeight
  deriving (Eq, Show)

data ImageLoadError
  = ImageLoadFailed String
  | ImageInvalid String
  deriving (Eq, Show)

data ImageCfg e = ImageCfg {
  _imcLoadError :: [ImageLoadError -> e],
  _imcFlags :: [ImageFlag],
  _imcFit :: Maybe ImageFit,
  _imcTransparency :: Maybe Double,
  _imcAlignH :: Maybe AlignH,
  _imcAlignV :: Maybe AlignV,
  _imcFactorW :: Maybe Double,
  _imcFactorH :: Maybe Double
}

instance Default (ImageCfg e) where
  def = ImageCfg {
    _imcLoadError = [],
    _imcFlags = [],
    _imcFit = Nothing,
    _imcTransparency = Nothing,
    _imcAlignH = Nothing,
    _imcAlignV = Nothing,
    _imcFactorW = Nothing,
    _imcFactorH = Nothing
  }

instance Semigroup (ImageCfg e) where
  (<>) i1 i2 = ImageCfg {
    _imcLoadError = _imcLoadError i1 ++ _imcLoadError i2,
    _imcFlags = _imcFlags i1 ++ _imcFlags i2,
    _imcFit = _imcFit i2 <|> _imcFit i1,
    _imcTransparency = _imcTransparency i2 <|> _imcTransparency i1,
    _imcAlignH = _imcAlignH i2 <|> _imcAlignH i1,
    _imcAlignV = _imcAlignV i2 <|> _imcAlignV i1,
    _imcFactorW = _imcFactorW i2 <|> _imcFactorW i1,
    _imcFactorH = _imcFactorH i2 <|> _imcFactorH i1
  }

instance Monoid (ImageCfg e) where
  mempty = def

instance CmbOnLoadError (ImageCfg e) e ImageLoadError where
  onLoadError err = def {
    _imcLoadError = [err]
  }

instance CmbImageNearest (ImageCfg e) where
  imageNearest = def {
    _imcFlags = [ImageNearest]
  }

instance CmbImageRepeatX (ImageCfg e) where
  imageRepeatX = def {
    _imcFlags = [ImageRepeatX]
  }

instance CmbImageRepeatY (ImageCfg e) where
  imageRepeatY = def {
    _imcFlags = [ImageRepeatY]
  }

instance CmbFitNone (ImageCfg e) where
  fitNone = def {
    _imcFit = Just FitNone
  }

instance CmbFitFill (ImageCfg e) where
  fitFill = def {
    _imcFit = Just FitFill
  }

instance CmbFitWidth (ImageCfg e) where
  fitWidth = def {
    _imcFit = Just FitWidth
  }

instance CmbFitHeight (ImageCfg e) where
  fitHeight = def {
    _imcFit = Just FitHeight
  }

instance CmbTransparency (ImageCfg e) where
  transparency alpha = def {
    _imcTransparency = Just alpha
  }

instance CmbAlignLeft (ImageCfg e) where
  alignLeft_ False = def
  alignLeft_ True = def {
    _imcAlignH = Just ALeft
  }

instance CmbAlignCenter (ImageCfg e) where
  alignCenter_ False = def
  alignCenter_ True = def {
    _imcAlignH = Just ACenter
  }

instance CmbAlignRight (ImageCfg e) where
  alignRight_ False = def
  alignRight_ True = def {
    _imcAlignH = Just ARight
  }

instance CmbAlignTop (ImageCfg e) where
  alignTop_ False = def
  alignTop_ True = def {
    _imcAlignV = Just ATop
  }

instance CmbAlignMiddle (ImageCfg e) where
  alignMiddle_ False = def
  alignMiddle_ True = def {
    _imcAlignV = Just AMiddle
  }

instance CmbAlignBottom (ImageCfg e) where
  alignBottom_ False = def
  alignBottom_ True = def {
    _imcAlignV = Just ABottom
  }

instance CmbResizeFactor (ImageCfg e) where
  resizeFactor s = def {
    _imcFactorW = Just s,
    _imcFactorH = Just s
  }

instance CmbResizeFactorDim (ImageCfg e) where
  resizeFactorW w = def {
    _imcFactorW = Just w
  }
  resizeFactorH h = def {
    _imcFactorH = Just h
  }

data ImageSource
  = ImageMem Text
  | ImagePath Text
  deriving (Eq, Show)

data ImageState = ImageState {
  isImageSource :: ImageSource,
  isImageData :: Maybe (ByteString, Size)
} deriving (Eq, Show, Generic)

data ImageMessage
  = ImageLoaded ImageState
  | ImageFailed ImageLoadError

-- | Creates an image with the given local path or url.
image :: WidgetEvent e => Text -> WidgetNode s e
image path = image_ path def

-- | Creates an image with the given local path or url. Accepts config.
image_ :: WidgetEvent e => Text -> [ImageCfg e] -> WidgetNode s e
image_ path configs = defaultWidgetNode "image" widget where
  config = mconcat configs
  source = ImagePath path
  imageState = ImageState source Nothing
  widget = makeImage source config imageState

-- | Creates an image with the given binary data.
imageMem
  :: WidgetEvent e
  => Text            -- ^ The logical name of the image.
  -> ByteString      -- ^ The image data as 4-byte RGBA blocks.
  -> Size            -- ^ The size of the image.
  -> WidgetNode s e  -- ^ The created image widget.
imageMem name imgData imgSize = imageMem_ name imgData imgSize def

-- | Creates an image with the given binary data. Accepts config.
imageMem_
  :: WidgetEvent e
  => Text            -- ^ The logical name of the image.
  -> ByteString      -- ^ The image data as 4-byte RGBA blocks.
  -> Size            -- ^ The size of the image.
  -> [ImageCfg e]    -- ^ The configuration of the image.
  -> WidgetNode s e  -- ^ The created image widget.
imageMem_ name imgData imgSize configs = defaultWidgetNode "image" widget where
  config = mconcat configs
  source = ImageMem name
  imageState = ImageState source (Just (imgData, imgSize))
  widget = makeImage source config imageState

makeImage
  :: WidgetEvent e => ImageSource -> ImageCfg e -> ImageState -> Widget s e
makeImage imgSource config state = widget where
  widget = createSingle state def {
    singleUseScissor = True,
    singleInit = init,
    singleMerge = merge,
    singleDispose = dispose,
    singleHandleMessage = handleMessage,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  isImageMem = case imgSource of
    ImageMem{} -> True
    _ -> False

  imgName source = case source of
    ImageMem path -> path
    ImagePath path -> path

  init wenv node = result where
    wid = node ^. L.info . L.widgetId
    path = node ^. L.info . L.path
    imgPath = imgName imgSource
    reqs = [RunTask wid path $ handleImageLoad config wenv imgPath]
    result = case imgSource of
      ImageMem _ -> resultNode node
      ImagePath _ -> resultReqs node reqs

  merge wenv newNode oldNode oldState = result where
    wid = newNode ^. L.info . L.widgetId
    path = newNode ^. L.info . L.path
    oldSource = isImageSource oldState
    imgPath = imgName imgSource
    prevPath = imgName oldSource
    sameImgNode = newNode
      & L.widget .~ makeImage imgSource config oldState
    newMemReqs = [ RemoveRendererImage prevPath ]
    newImgReqs = [
        RemoveRendererImage prevPath,
        RunTask wid path (handleImageLoad config wenv imgPath)
      ]
    result
      | oldSource == imgSource = resultNode sameImgNode
      | isImageMem = resultReqs newNode newMemReqs
      | otherwise = resultReqs newNode newImgReqs

  dispose wenv node = resultReqs node reqs where
    wid = node ^. L.info . L.widgetId
    path = node ^. L.info . L.path
    imgPath = imgName imgSource
    reqs = [
        RemoveRendererImage imgPath,
        RunTask wid path (handleImageDispose wenv imgPath)
      ]

  handleMessage wenv node target message = result where
    result = cast message >>= useImage node

  useImage node (ImageFailed msg) = result where
    evts = fmap ($ msg) (_imcLoadError config)
    result = Just $ resultEvts node evts
  useImage node (ImageLoaded newState) = result where
    newNode = node
      & L.widget .~ makeImage imgSource config newState
    result = Just $ resultReqs newNode [ResizeWidgets]

  getSizeReq wenv node = (sizeW, sizeH) where
    Size w h = maybe def snd (isImageData state)
    factorW = fromMaybe 1 (_imcFactorW config)
    factorH = fromMaybe 1 (_imcFactorH config)
    sizeW
      | abs factorW < 0.01 = fixedSize w
      | otherwise = expandSize w factorW
    sizeH
      | abs factorH < 0.01 = fixedSize h
      | otherwise = expandSize h factorH

  render wenv node renderer = do
    imageDef <- getImage renderer imgPath

    when (imageLoaded && isNothing imageDef) $
      addImage renderer imgPath imgSize imgBytes imgFlags

    when imageLoaded $
      showImage renderer imgPath imgFlags imgSize imgRect imgRadius alpha
    where
      style = activeStyle wenv node
      border = style ^. L.border
      radius = style ^. L.radius
      contentArea = getContentArea style node
      alpha = fromMaybe 1 (_imcTransparency config)
      fitMode = fromMaybe FitNone (_imcFit config)
      alignH = fromMaybe ALeft (_imcAlignH config)
      alignV = fromMaybe ATop (_imcAlignV config)
      imgPath = imgName imgSource
      imgFlags = _imcFlags config
      imgRect = fitImage contentArea imgSize fitMode alignH alignV
      imgRadius = subtractBorderFromRadius border <$> radius
      ImageState _ imgData = state
      imageLoaded = isJust imgData
      (imgBytes, imgSize) = fromJust imgData

showImage
  :: Renderer
  -> Text
  -> [ImageFlag]
  -> Size
  -> Rect
  -> Maybe Radius
  -> Double
  -> IO ()
showImage renderer imgPath imgFlags imgSize rect radius alpha = do
  beginPath renderer
  setFillImagePattern renderer imgPath topLeft size angle alpha
  drawRoundedRect renderer rect (fromMaybe def radius)
  fill renderer
  where
    Rect x y w h = rect
    Size dw dh = imgSize
    iw = if ImageRepeatX `elem` imgFlags then dw else w
    ih = if ImageRepeatY `elem` imgFlags then dh else h
    topLeft = Point x y
    size = Size iw ih
    angle = 0

fitImage :: Rect -> Size -> ImageFit -> AlignH -> AlignV -> Rect
fitImage viewport imageSize fitMode alignH alignV = case fitMode of
  FitNone -> alignImg iw ih
  FitFill -> alignImg w h
  FitWidth -> alignImg w (w * ih / iw)
  FitHeight -> alignImg (h * iw / ih) h
  where
    Rect x y w h = viewport
    Size iw ih = imageSize
    alignImg nw nh = alignInRect viewport (Rect x y nw nh) alignH alignV

handleImageLoad :: ImageCfg e -> WidgetEnv s e -> Text -> IO ImageMessage
handleImageLoad config wenv path = do
  -- Get the image's MVar. One MVar per image name/path is created, to allow
  -- loading images in parallel. The main MVar is only taken until the image's
  -- MVar is retrieved/created.
  sharedMap <- takeMVar sharedMapMVar
  sharedImgMVar <- case useShared (Map.lookup key sharedMap) of
    Just mvar -> return mvar
    Nothing -> newMVar emptyImgState
  putMVar sharedMapMVar (sharedMap & at key ?~ WidgetShared sharedImgMVar)

  -- Take the image's MVar until done
  sharedImg <- takeMVar sharedImgMVar
  (result, newSharedImg) <- case sharedImg of
    Just (oldState, oldCount) -> do
      return (ImageLoaded oldState, Just (oldState, oldCount + 1))
    Nothing -> do
      res <- loadImage path

      case res >>= decodeImage of
        Left loadError -> return (ImageFailed loadError, Nothing)
        Right dimg -> do
          let newState = makeImgState config wenv path dimg
          return (ImageLoaded newState, Just (newState, 1))

  putMVar sharedImgMVar newSharedImg
  return result
  where
    key = imgKey path
    sharedMapMVar = wenv ^. L.widgetShared
    emptyImgState :: Maybe (ImageState, Int)
    emptyImgState = Nothing

handleImageDispose :: WidgetEnv s e -> Text -> IO ()
handleImageDispose wenv path = do
  sharedMap <- takeMVar sharedMapMVar
  newSharedMap <- case useShared (Map.lookup key sharedMap) of
    Just mvar -> do
      sharedImg <- takeMVar mvar
      return $ case sharedImg of
        Just (oldState :: ImageState, oldCount :: Int)
          | oldCount > 1 ->
              sharedMap & at key ?~ WidgetShared (oldState, oldCount - 1)
        _ -> sharedMap & at key .~ Nothing
    Nothing -> return sharedMap
  putMVar sharedMapMVar newSharedMap
  where
    sharedMapMVar = wenv ^. L.widgetShared
    key = imgKey path

imgKey :: Text -> Text
imgKey path = "image-widget-key-" <> path

loadImage :: Text -> IO (Either ImageLoadError ByteString)
loadImage path
  | not (isUrl path) = loadLocal path
  | otherwise = loadRemote path

decodeImage :: ByteString -> Either ImageLoadError DynamicImage
decodeImage bs = either (Left . ImageInvalid) Right (Pic.decodeImage bs)

loadLocal :: Text -> IO (Either ImageLoadError ByteString)
loadLocal name = do
  let path = T.unpack name
  content <- BS.readFile path

  if BS.length content == 0
    then return . Left . ImageLoadFailed $ "Failed to load: " ++ path
    else return . Right $ content

loadRemote :: Text -> IO (Either ImageLoadError ByteString)
loadRemote name = do
  let path = T.unpack name
  eresp <- try $ getUrl path

  return $ case eresp of
    Left e -> remoteException path e
    Right r -> Right $ respBody r
  where
    respBody r = BSL.toStrict $ r ^. responseBody
    getUrl = getWith (defaults & checkResponse ?~ (\_ _ -> return ()))

remoteException
  :: String -> HttpException -> Either ImageLoadError ByteString
remoteException path (HttpExceptionRequest _ (StatusCodeException r _)) =
  Left . ImageLoadFailed $ respErrorMsg path $ show (respCode r)
remoteException path _ =
  Left . ImageLoadFailed $ respErrorMsg path "Unknown"

respCode :: Response a -> Int
respCode r = r ^. responseStatus . statusCode

respErrorMsg :: String -> String -> String
respErrorMsg path code = "Status: " ++ code ++ " - Path: " ++ path

makeImgState
  :: ImageCfg e
  -> WidgetEnv s e
  -> Text
  -> DynamicImage
  -> ImageState
makeImgState config wenv name dimg = newState where
  img = Pic.convertRGBA8 dimg
  cw = imageWidth img
  ch = imageHeight img
  size = Size (fromIntegral cw) (fromIntegral ch)
  bs = vectorToByteString $ imageData img
  newState = ImageState {
    isImageSource = ImagePath name,
    isImageData = Just (bs, size)
  }

isUrl :: Text -> Bool
isUrl url = T.isPrefixOf "http://" lurl || T.isPrefixOf "https://" lurl where
  lurl = T.toLower url
