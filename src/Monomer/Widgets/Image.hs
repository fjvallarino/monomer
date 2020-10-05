module Monomer.Widgets.Image (
  image,
  image_,
  fitNone,
  fitFill,
  fitWidth,
  fitHeight
) where

import Debug.Trace

import Codec.Picture (DynamicImage, Image(..))
import Control.Applicative ((<|>))
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

import Monomer.Core.BasicTypes
import Monomer.Core.Combinators
import Monomer.Core.Style
import Monomer.Core.WidgetTypes
import Monomer.Core.Util
import Monomer.Graphics.Drawing
import Monomer.Graphics.Types
import Monomer.Widgets.Single

data ImageFit
  = FitNone
  | FitFill
  | FitWidth
  | FitHeight
  deriving (Eq, Show)

data ImageCfg = ImageCfg {
  _imcFit :: Maybe ImageFit,
  _imcTransparency :: Maybe Double
}

instance Default ImageCfg where
  def = ImageCfg {
    _imcFit = Nothing,
    _imcTransparency = Nothing
  }

instance Semigroup ImageCfg where
  (<>) i1 i2 = ImageCfg {
    _imcFit = _imcFit i2 <|> _imcFit i1,
    _imcTransparency = _imcTransparency i2 <|> _imcTransparency i1
  }

instance Monoid ImageCfg where
  mempty = def

instance Transparency ImageCfg where
  transparency alpha = def {
    _imcTransparency = Just alpha
  }

fitNone :: ImageCfg
fitNone = def { _imcFit = Just FitNone }

fitFill :: ImageCfg
fitFill = def { _imcFit = Just FitFill }

fitWidth :: ImageCfg
fitWidth = def { _imcFit = Just FitWidth }

fitHeight :: ImageCfg
fitHeight = def { _imcFit = Just FitHeight }

newtype ImageState = ImageState {
  isImageData :: Maybe (ByteString, Size)
}

data ImageMessage
  = ImageLoaded ImageState
  | ImageFailed String

imageState :: ImageState
imageState = ImageState Nothing

image :: String -> WidgetInstance s e
image path = image_ path def

image_ :: String -> [ImageCfg] -> WidgetInstance s e
image_ path configs = defaultWidgetInstance "image" widget where
  config = mconcat configs
  widget = makeImage path config imageState

makeImage :: String -> ImageCfg -> ImageState -> Widget s e
makeImage imgPath config state = widget where
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
      _wiWidget = makeImage imgPath config newState
    }
    result = Just $ resultReqs [Resize] newInst

  getSizeReq wenv inst = sizeReq where
    theme = activeTheme wenv inst
    style = activeStyle wenv inst
    Size w h = maybe def snd (isImageData state)
    factor = 1
    sizeReq = (FlexSize w factor, FlexSize h factor)

  render renderer wenv inst = do
    when (imageLoaded && not imageExists) $
      addImage renderer imgPath ImageAddKeep imgSize imgBytes

    when imageLoaded $
      drawInScissor renderer True contentRect $
        drawImage renderer imgPath imageRect alpha
    where
      style = activeStyle wenv inst
      contentRect = getContentRect style inst
      alpha = fromMaybe 1 (_imcTransparency config)
      fitMode = fromMaybe FitNone (_imcFit config)
      imageRect = fitImage fitMode imgSize contentRect
      ImageState imgData = state
      imageLoaded = isJust imgData
      (imgBytes, imgSize) = fromJust imgData
      imageExists = existsImage renderer imgPath

fitImage :: ImageFit -> Size -> Rect -> Rect
fitImage fitMode imageSize renderArea = case fitMode of
  FitNone -> Rect x y iw ih
  FitFill -> Rect x y w h
  FitWidth -> Rect x y w ih
  FitHeight -> Rect x y iw h
  where
    Rect x y w h = renderArea
    Size iw ih = imageSize

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
