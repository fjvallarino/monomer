{-# LANGUAGE DeriveGeneric #-}

module Monomer.Graphics.Types where

import Data.ByteString (ByteString)
import Data.Default
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Sequence (Seq)
import GHC.Generics

import qualified Data.ByteString as BS
import qualified Data.Text as T

import Monomer.Core.BasicTypes

defaultFontName :: Text
defaultFontName = "Regular"

data Winding
  = CW
  | CCW
  deriving (Eq, Show, Generic)

data Color = Color {
  _colorR :: {-# UNPACK #-} !Int,
  _colorG :: {-# UNPACK #-} !Int,
  _colorB :: {-# UNPACK #-} !Int,
  _colorA :: {-# UNPACK #-} !Double
} deriving (Eq, Show, Generic)

instance Default Color where
  def = Color 255 255 255 1.0

data FontDef = FontDef {
  _fntName :: !Text,
  _fntPath :: !Text
} deriving (Eq, Show, Generic)

newtype Font
  = Font { unFont :: Text }
  deriving (Eq, Show, Generic)

instance IsString Font where
  fromString s = Font (T.pack s)

newtype FontSize
  = FontSize { unFontSize :: Double }
  deriving (Eq, Show, Generic)

instance Default Font where
  def = Font defaultFontName

instance Default FontSize where
  def = FontSize 32

data AlignH
  = ALeft
  | ACenter
  | ARight
  deriving (Eq, Show, Generic)

instance Default AlignH where
  def = ACenter

data AlignV
  = ATop
  | AMiddle
  | ABottom
  deriving (Eq, Show, Generic)

instance Default AlignV where
  def = AMiddle

data AlignTH
  = ATLeft
  | ATCenter
  | ATRight
  deriving (Eq, Show, Generic)

instance Default AlignTH where
  def = ATCenter

data AlignTV
  = ATTop
  | ATMiddle
  | ATBottom
  | ATBaseline
  deriving (Eq, Show, Generic)

instance Default AlignTV where
  def = ATMiddle

data GlyphPos = GlyphPos {
  _glpGlyph :: {-# UNPACK #-} !Char,
  _glpXMin :: {-# UNPACK #-} !Double,
  _glpXMax :: {-# UNPACK #-} !Double,
  _glpW :: {-# UNPACK #-} !Double
} deriving (Eq, Show, Generic)

instance Default GlyphPos where
  def = GlyphPos {
    _glpGlyph = ' ',
    _glpXMin = 0,
    _glpXMax = 0,
    _glpW = 0
  }

data TextMode
  = SingleLine
  | MultiLine
  deriving (Eq, Show, Generic)

data TextTrim
  = TrimSpaces
  | KeepSpaces
  deriving (Eq, Show, Generic)

data TextOverflow
  = Ellipsis
  | ClipText
  deriving (Eq, Show)

data TextMetrics = TextMetrics {
  _txmAsc :: {-# UNPACK #-} !Double,
  _txmDesc :: {-# UNPACK #-} !Double,
  _txmLineH :: {-# UNPACK #-} !Double
} deriving (Eq, Show, Generic)

instance Default TextMetrics where
  def = TextMetrics {
    _txmAsc = 0,
    _txmDesc = 0,
    _txmLineH = 0
  }

data TextLine = TextLine {
  _tlText :: !Text,
  _tlSize :: !Size,
  _tlRect :: !Rect,
  _tlGlyphs :: !(Seq GlyphPos),
  _tlMetrics :: !TextMetrics
} deriving (Eq, Show, Generic)

data ImageFlag
  = ImageNearest
  | ImageRepeatX
  | ImageRepeatY
  deriving (Eq, Show, Generic)

data ImageDef = ImageDef {
  _idfName :: String,
  _idfSize :: Size,
  _idfImgData :: BS.ByteString,
  _idfFlags :: [ImageFlag]
} deriving (Eq, Show, Generic)

-- Angles are always expressed in degrees, not radians
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
  -- Raw overlays
  createRawOverlay :: IO () -> IO (),
  renderRawOverlays :: IO (),
  -- Scissor
  intersectScissor :: Rect -> IO (),
  -- Translation
  setTranslation :: Point -> IO (),
  -- Scale
  setScale :: Point -> IO (),
  -- Rotation
  setRotation :: Double -> IO (),
  -- Alpha
  setGlobalAlpha :: Double -> IO (),
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
  computeTextMetrics :: Font -> FontSize -> TextMetrics,
  computeTextSize :: Font -> FontSize -> Text -> Size,
  computeGlyphsPos :: Font -> FontSize -> Text -> Seq GlyphPos,
  renderText :: Point -> Font -> FontSize -> Text -> IO (),
  -- Image
  getImage :: String -> Maybe ImageDef,
  addImage :: String -> Size -> ByteString -> [ImageFlag] -> IO (),
  updateImage :: String -> Size -> ByteString -> IO (),
  deleteImage :: String -> IO (),
  renderImage :: String -> Rect -> Double -> IO (),
  renderNewImage :: String -> Rect -> Double -> Size -> ByteString -> [ImageFlag] -> IO ()
}
