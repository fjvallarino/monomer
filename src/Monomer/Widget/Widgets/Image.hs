module Monomer.Widget.Widgets.Image (image) where

import Debug.Trace

import Codec.Picture (DynamicImage, Image(..), convertRGBA8, readImage)
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Typeable (cast)
import Data.Vector.Storable.ByteString (vectorToByteString)

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Graphics.Drawing
import Monomer.Graphics.Renderer
import Monomer.Graphics.Types
import Monomer.Widget.BaseSingle
import Monomer.Widget.Types
import Monomer.Widget.Util

newtype ImageState = ImageState {
  isSize :: Maybe Size
}

data ImageMessage
  = ImageLoaded Size
  | ImageFailed

imageState :: ImageState
imageState = ImageState Nothing

image :: String -> WidgetInstance s e
image path = defaultWidgetInstance "image" (makeImage path imageState)

makeImage :: String -> ImageState -> Widget s e
makeImage imgPath state = widget where
  widget = createSingle def {
    singleGetState = makeState state,
    singleInit = init,
    singleMerge = merge,
    singleHandleMessage = handleMessage,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  init wenv inst = trace "Init!" resultReqs reqs inst where
    path = _wiPath inst
    platform = _wePlatform wenv
    handleLoadImage = loadImage wenv imgPath
    reqs = [RunTask path handleLoadImage]

  merge wenv oldState widgetInst = resultWidget $ case useState oldState of
    Just newState -> widgetInst {
      _wiWidget = makeImage imgPath newState
    }
    Nothing -> widgetInst

  handleMessage wenv target message inst = result where
    result = cast message
      >>= useImage inst

  useImage inst ImageFailed = Nothing
  useImage inst (ImageLoaded newSize) = Just $ resultReqs [Resize] newInst where
    newInst = inst {
      _wiWidget = makeImage imgPath (ImageState $ Just newSize)
    }

  getSizeReq wenv inst = sizeReq where
    theme = activeTheme wenv inst
    style = activeStyle wenv inst
    size = fromMaybe def (isSize state)
    sizeReq = SizeReq size FlexibleSize FlexibleSize

  render renderer wenv inst = do
    drawStyledBackground renderer renderArea style
    drawStyledImage renderer renderArea style imgPath
    where
      renderArea = _wiRenderArea inst
      style = activeStyle wenv inst

loadImage :: WidgetEnv s e -> String -> IO ImageMessage
loadImage wenv path = do
  res <- readImage path
  registerImg wenv path res

registerImg
  :: WidgetEnv s e
  -> String
  -> Either String DynamicImage
  -> IO ImageMessage
registerImg wenv name Left{} = return ImageFailed
registerImg wenv name (Right dimg) = do
  addImage renderer name cw ch False bs
  return $ ImageLoaded size
  where
    renderer = _weRenderer wenv
    img = convertRGBA8 dimg
    cw = imageWidth img
    ch = imageHeight img
    size = Size (fromIntegral cw) (fromIntegral ch)
    bs = vectorToByteString $ imageData img
