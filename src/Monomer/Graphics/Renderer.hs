{-# LANGUAGE ExistentialQuantification #-}

module Monomer.Graphics.Renderer where

import Control.Monad
import Data.Text (Text)

import Monomer.Common.Geometry
import Monomer.Common.Tree
import Monomer.Graphics.Types

data Winding = CW | CCW deriving (Eq, Show)

data Renderer m = (Monad m) => Renderer {
  beginPath :: m (),
  -- Context management
  saveContext :: m (),
  restoreContext :: m (),
  -- Overlays
  createOverlay :: m () -> m (),
  runOverlays :: m (),
  -- Scissor operations
  setScissor :: Rect -> m (),
  resetScissor :: m (),
  -- Strokes
  stroke :: m (),
  strokeColor :: Color -> m (),
  strokeWidth :: Double -> m (),
  -- Fill
  fill :: m (),
  fillColor :: Color -> m (),
  fillLinearGradient :: Point -> Point -> Color -> Color -> m (),
  -- Drawing
  moveTo :: Point -> m (),
  line :: Point -> Point -> m (),
  lineTo :: Point -> m (),
  rect :: Rect -> m (),
  arc :: Point -> Double -> Double -> Double -> Winding -> m (),
  quadTo :: Point -> Point -> m (),
  ellipse :: Rect -> m (),
  -- Text
  text :: Rect -> Font -> FontSize -> Align -> Text -> m Rect,
  textBounds :: Font -> FontSize -> Text -> Size
}
