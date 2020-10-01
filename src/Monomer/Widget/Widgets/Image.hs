module Monomer.Widget.Widgets.Image (image) where

import Debug.Trace

import Codec.Picture (DynamicImage, Image(..))
import Control.Lens ((^.))
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Default
import Data.Maybe
import Data.List (isPrefixOf)
import Data.Typeable (cast)
import Data.Vector.Storable.ByteString (vectorToByteString)
import Network.Wreq

import qualified Codec.Picture as Pic
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Wreq as Wreq

import Monomer.Common.Geometry
import Monomer.Graphics.Drawing
import Monomer.Graphics.Types
import Monomer.Widget.BaseSingle
import Monomer.Widget.Types
import Monomer.Widget.Util

newtype ImageState = ImageState {
  isImageData :: Maybe (ByteString, Size)
}

data ImageMessage
  = ImageLoaded ImageState
  | ImageFailed String

imageState :: ImageState
imageState = ImageState Nothing

image :: String -> WidgetInstance s e
image path = defaultWidgetInstance "image" (makeImage path imageState)

makeImage :: String -> ImageState -> Widget s e
makeImage imgPath state = widget where
  widget = createSingle def {
    singleInit = init,
    singleDispose = dispose,
    singleHandleMessage = handleMessage,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  init wenv inst = resultReqs reqs inst where
    path = _wiPath inst
    reqs = [RunTask path $ handleImageLoad wenv imgPath]

  dispose wenv inst = resultReqs reqs inst where
    path = _wiPath inst
    renderer = _weRenderer wenv
    reqs = [RunTask path $ removeImage wenv imgPath]

  handleMessage wenv target message inst = result where
    result = cast message >>= useImage inst

  useImage inst (ImageFailed msg) = traceShow msg Nothing
  useImage inst (ImageLoaded newState) = result where
    newInst = inst {
      _wiWidget = makeImage imgPath newState
    }
    result = Just $ resultReqs [Resize] newInst

  getSizeReq wenv inst = sizeReq where
    theme = activeTheme wenv inst
    style = activeStyle wenv inst
    size = maybe def snd (isImageData state)
    sizeReq = SizeReq size FlexibleSize FlexibleSize

  render renderer wenv inst = do
    when (imageLoaded && not imageExists) $
      addImage renderer imgPath ImageAddKeep imgSize imgBytes

    drawStyledImage renderer contentRect style imgPath
    where
      style = activeStyle wenv inst
      contentRect = getContentRect style inst
      ImageState imgData = state
      imageLoaded = isJust imgData
      (imgBytes, imgSize) = fromJust imgData
      imageExists = existsImage renderer imgPath

handleImageLoad :: WidgetEnv s e -> String -> IO ImageMessage
handleImageLoad wenv path = do
  res <- loadImage path
  registerImg wenv path res

loadImage :: String -> IO (Either String DynamicImage)
loadImage path
  | not (isUrl path) = Pic.readImage path
  | otherwise = loadRemote path

loadRemote :: String -> IO (Either String DynamicImage)
loadRemote path = do
  r <- Wreq.get path

  if respCode r == 200
    then return . Pic.decodeImage $ respBody r
    else return . Left $ errorMessage r
  where
    respCode r = r ^. responseStatus . statusCode
    respBody r = BSL.toStrict $ r ^. responseBody
    errorMessage r = "Status: " ++ show (respCode r)

removeImage :: WidgetEnv s e -> String -> IO (Maybe ImageMessage)
removeImage wenv path = do
  deleteImage renderer path
  return Nothing
  where
    renderer = _weRenderer wenv

registerImg
  :: WidgetEnv s e
  -> String
  -> Either String DynamicImage
  -> IO ImageMessage
registerImg wenv name (Left msg) = return $ ImageFailed msg
registerImg wenv name (Right dimg) = do
  addImage renderer name ImageAddKeep size bs
  return $ ImageLoaded newState
  where
    renderer = _weRenderer wenv
    img = Pic.convertRGBA8 dimg
    cw = imageWidth img
    ch = imageHeight img
    size = Size (fromIntegral cw) (fromIntegral ch)
    bs = vectorToByteString $ imageData img
    newState = ImageState {
      isImageData = Just (bs, size)
    }

isUrl :: String -> Bool
isUrl url = isPrefixOf "http://" lurl || isPrefixOf "https://" lurl where
  lurl = map toLower url
