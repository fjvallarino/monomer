module Monomer.Widget.Widgets.Image (image) where

import Debug.Trace

import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Data.Typeable (Typeable, cast)

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Graphics.Drawing
import Monomer.Graphics.Types
import Monomer.Widget.BaseSingle
import Monomer.Widget.Types
import Monomer.Widget.Util

newtype ImageMessage = ImageMessage {
  unImageMessage :: Maybe Image 
}

image :: Text -> WidgetInstance s e
image imgPath = defaultWidgetInstance "image" (makeImage imgPath Nothing)

makeImage :: Text -> Maybe Image -> Widget s e
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
    localPath = LocalPath imgPath
    loadImage = return $ ImageMessage (_wpLoadImage platform localPath)
    reqs = [RunTask path loadImage]

  handleMessage wenv target message inst = trace "Called!" result where
    result = cast message
      >>= unImageMessage
      >>= useImage inst

  useImage inst img = trace ("Yeah! " ++ show img) Just $ resultReqs [Resize] newInst where
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
