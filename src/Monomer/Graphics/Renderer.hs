{-# LANGUAGE ExistentialQuantification #-}

module Monomer.Graphics.Renderer where

import Control.Monad

import qualified Data.Text as T

import Monomer.Common.Types
import Monomer.Data.Tree
import Monomer.Graphics.Types

data WidgetRenderType = RenderNormal | RenderPost deriving (Eq, Show)

data Renderer m = (Monad m) => Renderer {
  beginWidget :: Path -> WidgetRenderType -> m (),
  endWidget :: Path -> WidgetRenderType -> m (),
  beginPath :: m (),
  -- Context management
  saveContext :: m (),
  restoreContext :: m (),
  -- Scissor operations
  scissor :: Rect -> m (),
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
  arc :: Point -> Double -> Double -> Double -> m (),
  quadTo :: Point -> Point -> m (),
  ellipse :: Rect -> m (),
  -- Text
  text :: Rect -> Font -> FontSize -> Align -> T.Text -> m (Rect),
  textBounds :: Font -> FontSize -> T.Text -> m Size
}
