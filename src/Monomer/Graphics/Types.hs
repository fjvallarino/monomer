module Monomer.Graphics.Types where

import Data.ByteString (ByteString)
import Data.Default
import Data.Text (Text)
import Data.Sequence (Seq)

import Monomer.Core.BasicTypes

data Winding
  = CW
  | CCW
  deriving (Eq, Show)

data Color = Color {
  _colorR :: Int,
  _colorG :: Int,
  _colorB :: Int,
  _colorA :: Double
} deriving (Show, Eq)

instance Semigroup Color where
  (<>) _ c2 = c2

instance Default Color where
  def = Color 255 255 255 1.0

newtype Font
  = Font { unFont :: Text }
  deriving (Eq, Show)

newtype FontSize
  = FontSize { unFontSize :: Double }
  deriving (Eq, Show)

instance Default Font where
  def = Font "sans"

instance Default FontSize where
  def = FontSize 32

data Align
  = Align AlignH AlignV
  deriving (Show, Eq)

instance Default Align where
  def = Align ACenter AMiddle

data AlignH
  = ALeft
  | ACenter
  | ARight
  deriving (Show, Eq)

instance Default AlignH where
  def = ACenter

data AlignV
  = ATop
  | AMiddle
  | ABottom
  deriving (Show, Eq)

instance Default AlignV where
  def = AMiddle

data GlyphPos = GlyphPos {
  _glpXMin :: Double,
  _glpXMax :: Double,
  _glpW :: Double
} deriving (Eq, Show)

instance Default GlyphPos where
  def = GlyphPos {
    _glpXMin = 0,
    _glpXMax = 0,
    _glpW = 0
  }

data ImageAddAction
  = ImageAddKeep
  | ImageAddReplace
  deriving (Eq, Show)

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
  computeTextRect :: Rect -> Font -> FontSize -> Align -> Text -> Rect,
  renderText :: Rect -> Font -> FontSize -> Align -> Text -> IO Rect,
  -- Image
  addImage :: String -> ImageAddAction -> Size -> ByteString -> IO (),
  updateImage :: String -> ByteString -> IO (),
  deleteImage :: String -> IO (),
  existsImage :: String -> Bool,
  renderImage :: String -> Rect -> Double -> IO ()
}
