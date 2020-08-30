{-# LANGUAGE ExistentialQuantification #-}

module Monomer.Graphics.Renderer where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Sequence (Seq)

import Monomer.Common.Geometry
import Monomer.Graphics.Types

data Renderer = Renderer {
  -- Frame
  beginFrame :: Int -> Int -> IO (),
  endFrame :: IO (),
  -- Path
  beginPath :: IO (),
  closePath :: IO (),
  -- Context management
  saveContext :: IO (),
  restoreContext :: IO (),
  -- Overlays
  createOverlay :: IO () -> IO (),
  renderOverlays :: IO (),
  -- Scissor operations
  setScissor :: Rect -> IO (),
  resetScissor :: IO (),
  -- Strokes
  stroke :: IO (),
  setStrokeColor :: Color -> IO (),
  setStrokeWidth :: Double -> IO (),
  -- Fill
  fill :: IO (),
  setFillColor :: Color -> IO (),
  setFillLinearGradient :: Point -> Point -> Color -> Color -> IO (),
  -- Drawing
  moveTo :: Point -> IO (),
  renderLine :: Point -> Point -> IO (),
  renderLineTo :: Point -> IO (),
  renderRect :: Rect -> IO (),
  renderArc :: Point -> Double -> Double -> Double -> Winding -> IO (),
  renderQuadTo :: Point -> Point -> IO (),
  renderEllipse :: Rect -> IO (),
  -- Text
  computeTextSize :: Font -> FontSize -> Text -> Size,
  computeGlyphsPos :: Font -> FontSize -> Text -> Seq GlyphPos,
  renderText :: Rect -> Font -> FontSize -> Align -> Text -> IO Rect,
  -- Image
  addImage :: String -> Int -> Int -> Bool -> ByteString -> IO (),
  updateImage :: String -> ByteString -> IO (),
  deleteImage :: String -> IO (),
  renderImage :: Rect -> String -> IO ()
}
