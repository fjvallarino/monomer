{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Widgets.CirclesGrid where

import Control.Lens
--import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import System.Random

import Monomer.Graphics.ColorTable
import Monomer.Widgets.Single

import qualified Monomer.Lens as L

newtype CirclesGridCfg = CirclesGridCfg {
  _cgcItemWidth :: Double
} deriving (Eq, Show)

instance Default CirclesGridCfg where
  def = CirclesGridCfg {
    _cgcItemWidth = 25
  }

data CirclesGridState = CirclesGridState {
  _cgcMouseX :: Double,
  _cgcMouseY :: Double
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''CirclesGridCfg
makeLensesWith abbreviatedFields ''CirclesGridState

circlesGrid :: CirclesGridCfg -> WidgetNode s e
circlesGrid cfg = defaultWidgetNode "circlesGrid" widget where
  widget = makeCirclesGrid cfg (CirclesGridState 0 0)

makeCirclesGrid :: CirclesGridCfg -> CirclesGridState -> Widget s e
makeCirclesGrid cfg state = widget where
  widget = createSingle state def {
    singleUseScissor = True,
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  handleEvent wenv node target evt = case evt of
    Move (Point x y) -> Just (resultReqs newNode [RenderOnce]) where
      newState = CirclesGridState x y
      newNode = node
        & L.widget .~ makeCirclesGrid cfg newState
    _ -> Nothing

  getSizeReq wenv node = (expandSize 100 1, expandSize 100 1)

  render wenv node renderer = do
    setStdGen (mkStdGen 42)

    mapM_ (drawCircle renderer state vp iw cols) [0..cols * rows - 1]
    where
      vp = node ^. L.info . L.viewport
      iw = cfg ^. itemWidth
      cols = round (vp ^. L.w / iw)
      rows = round (vp ^. L.h / iw)

drawCircle
  :: Renderer -> CirclesGridState -> Rect -> Double -> Int -> Int -> IO ()
drawCircle renderer state vp iw cols idx = do
  colorIdx :: Double <- randomIO
  offsetX <- randomIO
  offsetY <- randomIO
  let color = colors !! floor (fromIntegral (length colors) * colorIdx)
  let colorFill = color & L.a .~ 0.3
  beginPath renderer
  setStrokeWidth renderer 2
  setStrokeColor renderer color
  setFillColor renderer colorFill
  renderEllipse renderer (rect offsetX offsetY)
  fill renderer
  stroke renderer
  where
    colors = [magenta, orange, yellow, green]
    sizeFactor = 1.1 * state ^. mouseY / vp ^. L.h
    randFactor = state ^. mouseX / vp ^. L.w
    currw = sizeFactor * iw
    szDiff = (1 - sizeFactor) * iw
    x = vp ^. L.x + iw * fromIntegral (idx `rem` cols)
    y = vp ^. L.y + iw * fromIntegral (idx `div` cols)
    rect ox oy = Rect rx ry currw currw where
      rx = x + randFactor * (ox - 0.5) * iw
      ry = y + randFactor * (oy - 0.5) * iw
