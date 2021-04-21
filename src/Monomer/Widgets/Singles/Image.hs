{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Singles.Image (
  ImageLoadError(..),
  image,
  image_,
  fitNone,
  fitFill,
  fitWidth,
  fitHeight
) where

import Codec.Picture (DynamicImage, Image(..))
import Codec.Serialise
import Control.Applicative ((<|>))
import Control.Exception (try)
import Control.Lens ((&), (^.), (.~), (%~), (?~))
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Default
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
import qualified Network.Wreq as Wreq
import qualified Data.Text as T

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
  _imcFit :: Maybe ImageFit,
  _imcTransparency :: Maybe Double
}

instance Default (ImageCfg e) where
  def = ImageCfg {
    _imcLoadError = [],
    _imcFit = Nothing,
    _imcTransparency = Nothing
  }

instance Semigroup (ImageCfg e) where
  (<>) i1 i2 = ImageCfg {
    _imcLoadError = _imcLoadError i1 ++ _imcLoadError i2,
    _imcFit = _imcFit i2 <|> _imcFit i1,
    _imcTransparency = _imcTransparency i2 <|> _imcTransparency i1
  }

instance Monoid (ImageCfg e) where
  mempty = def

instance CmbTransparency (ImageCfg e) where
  transparency alpha = def {
    _imcTransparency = Just alpha
  }

instance CmbOnLoadError (ImageCfg e) ImageLoadError e where
  onLoadError err = def {
    _imcLoadError = [err]
  }

fitNone :: ImageCfg e
fitNone = def { _imcFit = Just FitNone }

fitFill :: ImageCfg e
fitFill = def { _imcFit = Just FitFill }

fitWidth :: ImageCfg e
fitWidth = def { _imcFit = Just FitWidth }

fitHeight :: ImageCfg e
fitHeight = def { _imcFit = Just FitHeight }

data ImageState = ImageState {
  isImagePath :: String,
  isImageData :: Maybe (ByteString, Size)
} deriving (Eq, Show, Generic, Serialise)

instance WidgetModel ImageState where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

data ImageMessage
  = ImageLoaded ImageState
  | ImageFailed ImageLoadError

image :: WidgetEvent e => Text -> WidgetNode s e
image path = image_ path def

image_ :: WidgetEvent e => Text -> [ImageCfg e] -> WidgetNode s e
image_ tpath configs = defaultWidgetNode "image" widget where
  path = T.unpack tpath
  config = mconcat configs
  imageState = ImageState path Nothing
  widget = makeImage path config imageState

makeImage :: WidgetEvent e => String -> ImageCfg e -> ImageState -> Widget s e
makeImage imgPath config state = widget where
  widget = createSingle state def {
    singleUseScissor = True,
    singleInit = init,
    singleRestore = restore,
    singleDispose = dispose,
    singleHandleMessage = handleMessage,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  init wenv node = resultReqs node reqs where
    wid = node ^. L.info . L.widgetId
    path = node ^. L.info . L.path
    reqs = [RunTask wid path $ handleImageLoad wenv imgPath]

  restore wenv oldState oldInfo newNode = result where
    wid = newNode ^. L.info . L.widgetId
    path = newNode ^. L.info . L.path
    newImgReqs = [ RunTask wid path $ do
        removeImage wenv imgPath
        handleImageLoad wenv imgPath
      ]
    sameImgNode = newNode
      & L.widget .~ makeImage imgPath config oldState
    result
      | isImagePath oldState == imgPath = resultWidget sameImgNode
      | otherwise = resultReqs newNode newImgReqs

  dispose wenv node = resultReqs node reqs where
    wid = node ^. L.info . L.widgetId
    path = node ^. L.info . L.path
    renderer = _weRenderer wenv
    reqs = [RunTask wid path $ removeImage wenv imgPath]

  handleMessage wenv target message node = result where
    result = cast message >>= useImage node

  useImage node (ImageFailed msg) = result where
    evts = fmap ($ msg) (_imcLoadError config)
    result = Just $ resultEvts node evts
  useImage node (ImageLoaded newState) = result where
    newNode = node
      & L.widget .~ makeImage imgPath config newState
    result = Just $ resultReqs newNode [ResizeWidgets]

  getSizeReq wenv currState node = sizeReq where
    Size w h = maybe def snd (isImageData currState)
    factor = 1
    sizeReq = (expandSize w factor, expandSize h factor)

  render renderer wenv node = do
    when (imageLoaded && imageExists) $
      drawImage renderer imgPath imageRect alpha
    when (imageLoaded && not imageExists) $
      drawNewImage renderer imgPath imgSize imgBytes imageRect alpha
    where
      style = activeStyle wenv node
      contentArea = getContentArea style node
      alpha = fromMaybe 1 (_imcTransparency config)
      fitMode = fromMaybe FitNone (_imcFit config)
      imageRect = fitImage fitMode imgSize contentArea
      ImageState _ imgData = state
      imageLoaded = isJust imgData
      (imgBytes, imgSize) = fromJust imgData
      imageExists = isJust (getImage renderer imgPath)

fitImage :: ImageFit -> Size -> Rect -> Rect
fitImage fitMode imageSize viewport = case fitMode of
  FitNone -> Rect x y iw ih
  FitFill -> Rect x y w h
  FitWidth -> Rect x y w ih
  FitHeight -> Rect x y iw h
  where
    Rect x y w h = viewport
    Size iw ih = imageSize

handleImageLoad :: WidgetEnv s e -> String -> IO ImageMessage
handleImageLoad wenv path =
  if isNothing prevImage
    then do
      res <- loadImage path

      case res >>= decodeImage of
        Left loadError -> return (ImageFailed loadError)
        Right dimg -> registerImg wenv path dimg
    else do
      addImage renderer path size imgData
      return $ ImageLoaded newState
  where
    renderer = _weRenderer wenv
    prevImage = getImage renderer path
    ImageDef _ size imgData = fromJust prevImage
    newState = ImageState {
      isImagePath = path,
      isImageData = Just (imgData, size)
    }

loadImage :: String -> IO (Either ImageLoadError ByteString)
loadImage path
  | not (isUrl path) = loadLocal path
  | otherwise = loadRemote path

decodeImage :: ByteString -> Either ImageLoadError DynamicImage
decodeImage bs = either (Left . ImageInvalid) Right (Pic.decodeImage bs)

loadLocal :: String -> IO (Either ImageLoadError ByteString)
loadLocal path = do
  content <- BS.readFile path

  if BS.length content == 0
    then return . Left . ImageLoadFailed $ "Failed to load: " ++ path
    else return . Right $ content

loadRemote :: String -> IO (Either ImageLoadError ByteString)
loadRemote path = do
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

removeImage :: WidgetEnv s e -> String -> IO (Maybe ImageMessage)
removeImage wenv path = do
  deleteImage renderer path
  return Nothing
  where
    renderer = _weRenderer wenv

registerImg
  :: WidgetEnv s e
  -> String
  -> DynamicImage
  -> IO ImageMessage
registerImg wenv name dimg = do
  addImage renderer name size bs
  return $ ImageLoaded newState
  where
    renderer = _weRenderer wenv
    img = Pic.convertRGBA8 dimg
    cw = imageWidth img
    ch = imageHeight img
    size = Size (fromIntegral cw) (fromIntegral ch)
    bs = vectorToByteString $ imageData img
    newState = ImageState {
      isImagePath = name,
      isImageData = Just (bs, size)
    }

isUrl :: String -> Bool
isUrl url = isPrefixOf "http://" lurl || isPrefixOf "https://" lurl where
  lurl = map toLower url
