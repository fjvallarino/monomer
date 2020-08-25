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
import Monomer.Graphics.Types
import Monomer.Widget.BaseSingle
import Monomer.Widget.Types
import Monomer.Widget.Util

newtype ImageMessage = ImageMessage {
  unImageMessage :: Maybe ImageHandle
}

image :: FilePath -> WidgetInstance s e
image imgPath = defaultWidgetInstance "image" (makeImage imgPath Nothing)

makeImage :: FilePath -> Maybe ImageHandle -> Widget s e
makeImage imgPath image = widget where
  widget = createSingle def {
    singleInit = init,
    singleHandleMessage = handleMessage,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  init wenv inst = resultReqs reqs inst where
    path = _wiPath inst
    platform = _wePlatform wenv
    handleLoadImage = do
      img <- loadImage wenv imgPath
      return $ ImageMessage img
    reqs = [RunTask path handleLoadImage]

  handleMessage wenv target message inst = result where
    result = cast message
      >>= unImageMessage
      >>= useImage inst

  useImage inst img = Just $ resultReqs [Resize] newInst where
    newInst = inst {
      _wiWidget = makeImage imgPath (Just img)
    }

  getSizeReq wenv inst = sizeReq where
    theme = activeTheme wenv inst
    style = activeStyle wenv inst
    size = case image of
      Just img -> _imageSize img
      Nothing -> def
    sizeReq = SizeReq size FlexibleSize FlexibleSize

  render renderer wenv inst = do
    drawStyledBackground renderer renderArea style

    forM_ image (drawStyledImage renderer renderArea style)
    where
      renderArea = _wiRenderArea inst
      style = activeStyle wenv inst

loadImage :: WidgetEnv s e -> FilePath -> IO (Maybe ImageHandle)
loadImage wenv path = do
  res <- readImage path
  return $ registerImg wenv res

registerImg :: WidgetEnv s e -> Either String DynamicImage -> Maybe ImageHandle
registerImg _ Left{} = Nothing
registerImg wenv (Right dimg) = _wpCreateImage platform w h bs where
  platform = _wePlatform wenv
  img = convertRGBA8 dimg
  w = imageWidth img
  h = imageHeight img
  bs = vectorToByteString $ imageData img
