{-|
Module      : Monomer.Graphics.Types
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Basic types for Graphics.

Angles are always expressed in degrees, not radians.
-}
{-# LANGUAGE DeriveGeneric #-}

module Monomer.Graphics.Types where

import Data.ByteString (ByteString)
import Data.Default
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Sequence (Seq)
import GHC.Generics

import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Common

-- | Direction in which triangles and arcs are drawn.
data Winding
  = CW
  | CCW
  deriving (Eq, Show, Generic)

-- | An RGBA color.
data Color = Color {
  _colorR :: {-# UNPACK #-} !Int,
  _colorG :: {-# UNPACK #-} !Int,
  _colorB :: {-# UNPACK #-} !Int,
  _colorA :: {-# UNPACK #-} !Double
} deriving (Eq, Show, Generic)

instance Default Color where
  def = Color 255 255 255 1.0

-- | The definition of a font.
data FontDef = FontDef {
  _fntName :: !Text,  -- ^ The logic name. Will be used when defining styles.
  _fntPath :: !Text   -- ^ The path in the filesystem.
} deriving (Eq, Show, Generic)

-- | The name of a loaded font.
newtype Font
  = Font { unFont :: Text }
  deriving (Eq, Show, Generic)

instance IsString Font where
  fromString s = Font (T.pack s)

instance Default Font where
  def = Font "Regular"

-- | The size of a font.
newtype FontSize
  = FontSize { unFontSize :: Double }
  deriving (Eq, Show, Generic)

instance Default FontSize where
  def = FontSize 32

data RectSide
  = SideLeft
  | SideRight
  | SideTop
  | SideBottom
  deriving (Eq, Show)

-- | Horizontal alignment flags.
data AlignH
  = ALeft
  | ACenter
  | ARight
  deriving (Eq, Show, Generic)

instance Default AlignH where
  def = ACenter

-- | Vertical alignment flags.
data AlignV
  = ATop
  | AMiddle
  | ABottom
  deriving (Eq, Show, Generic)

instance Default AlignV where
  def = AMiddle

-- | Text horizontal alignment flags.
data AlignTH
  = ATLeft
  | ATCenter
  | ATRight
  deriving (Eq, Show, Generic)

instance Default AlignTH where
  def = ATCenter

-- | Text vertical alignment flags.
data AlignTV
  = ATTop
  | ATMiddle
  | ATAscender
  | ATLowerX
  | ATBottom
  | ATBaseline
  deriving (Eq, Show, Generic)

instance Default AlignTV where
  def = ATLowerX

-- | Information of a text glyph instance.
data GlyphPos = GlyphPos {
  _glpGlyph :: {-# UNPACK #-} !Char,   -- ^ The represented character.
  _glpXMin :: {-# UNPACK #-} !Double,  -- ^ The min x coordinate.
  _glpXMax :: {-# UNPACK #-} !Double,  -- ^ The max x coordinate.
  _glpYMin :: {-# UNPACK #-} !Double,  -- ^ The min x coordinate.
  _glpYMax :: {-# UNPACK #-} !Double,  -- ^ The max x coordinate.
  _glpW :: {-# UNPACK #-} !Double,     -- ^ The glyph width.
  _glpH :: {-# UNPACK #-} !Double      -- ^ The glyph height.
} deriving (Eq, Show, Generic)

instance Default GlyphPos where
  def = GlyphPos {
    _glpGlyph = ' ',
    _glpXMin = 0,
    _glpXMax = 0,
    _glpYMin = 0,
    _glpYMax = 0,
    _glpW = 0,
    _glpH = 0
  }

-- | Text flags for single or multiline.
data TextMode
  = SingleLine
  | MultiLine
  deriving (Eq, Show, Generic)

-- | Text flags for trimming or keeping sapces.
data TextTrim
  = TrimSpaces
  | KeepSpaces
  deriving (Eq, Show, Generic)

-- | Text flags for clipping or using ellipsis.
data TextOverflow
  = Ellipsis
  | ClipText
  deriving (Eq, Show)

-- | Text metrics.
data TextMetrics = TextMetrics {
  _txmAsc :: {-# UNPACK #-} !Double,    -- ^ The height above the baseline.
  _txmDesc :: {-# UNPACK #-} !Double,   -- ^ The height below the baseline.
  _txmLineH :: {-# UNPACK #-} !Double,  -- ^ The total height.
  _txmLowerX :: {-# UNPACK #-} !Double  -- ^ The height of lowercase x.
} deriving (Eq, Show, Generic)

instance Default TextMetrics where
  def = TextMetrics {
    _txmAsc = 0,
    _txmDesc = 0,
    _txmLineH = 0,
    _txmLowerX = 0
  }

-- | A text line with associated rendering information.
data TextLine = TextLine {
  _tlText :: !Text,              -- ^ The represented text.
  _tlSize :: !Size,              -- ^ The size the formatted text takes.
  _tlRect :: !Rect,              -- ^ The rect the formatted text occupies.
  _tlGlyphs :: !(Seq GlyphPos),  -- ^ The glyphs for each character.
  _tlMetrics :: !TextMetrics     -- ^ The text metrics for the given font/size.
} deriving (Eq, Show, Generic)

instance Default TextLine where
  def = TextLine {
    _tlText = "",
    _tlSize = def,
    _tlRect = def,
    _tlGlyphs = Seq.empty,
    _tlMetrics = def
  }

-- | Flags for a newly created image.
data ImageFlag
  = ImageNearest
  | ImageRepeatX
  | ImageRepeatY
  deriving (Eq, Show, Generic)

-- | The definition of a loaded image.
data ImageDef = ImageDef {
  _idfName :: String,            -- ^ The logic name of the image.
  _idfSize :: Size,              -- ^ The dimensions of the image.
  _idfImgData :: BS.ByteString,  -- ^ The image data as RGBA 4-bytes blocks.
  _idfFlags :: [ImageFlag]       -- ^ The image flags.
} deriving (Eq, Show, Generic)

data FontManager = FontManager {
  -- | Returns the text metrics of a given font and size.
  computeTextMetrics :: Font -> FontSize -> TextMetrics,
  -- | Returns the text size of the text given font and size.
  computeTextSize :: Font -> FontSize -> Text -> Size,
  -- | Returns the glyphs of the text given font and size.
  computeGlyphsPos :: Font -> FontSize -> Text -> Seq GlyphPos
}

-- | Low level rendering definitions.
data Renderer = Renderer {
  -- | Begins a new frame.
  beginFrame :: Int -> Int -> IO (),
  -- | Finishes a frame, consolidating the drawing operations since beginFrame.
  endFrame :: IO (),
  -- | Begins a new path
  beginPath :: IO (),
  -- | Finishes an active path by closing it with a line.
  closePath :: IO (),
  -- | Saves current context (scissor, offset, scale, rotation, etc).
  saveContext :: IO (),
  -- | Restores a previously saved context.
  restoreContext :: IO (),
  -- | Creates an overlay. These are rendered after the regular frame has been
  -- | displayed. Useful, for instance, for a dropdown or context menu.
  createOverlay :: IO () -> IO (),
  -- | Renders the added overlays and clears them.
  renderOverlays :: IO (),
  {-|
  Creates a render task which does not rely on the abstractions provided by the
  Renderer. Well suited for pure OpenGL/Vulkan/Metal.

  This runs _before_ overlays of any type, and it's useful for the content of
  widgets created with low level APIs.
  -}
  createRawTask :: IO () -> IO (),
  -- | Renders the added raw tasks and clears its queue.
  renderRawTasks :: IO (),
  {-|
  Creates an overlay which does not rely on the abstractions provided by the
  Renderer. Well suited for pure OpenGL/Vulkan/Metal.

  This runs _after_ overlays based on Renderer.
  -}
  createRawOverlay :: IO () -> IO (),
  -- | Renders the added raw overlays and clears its queue.
  renderRawOverlays :: IO (),
  -- | Sets, or intersects, a scissor which will limit the visible area.
  intersectScissor :: Rect -> IO (),
  -- | Translates all further drawing operations by the given offset.
  setTranslation :: Point -> IO (),
  -- | Scales all further drawing operations by the given size.
  setScale :: Point -> IO (),
  -- | Rotates all further drawing operations by the given angle.
  setRotation :: Double -> IO (),
  -- | Applies the given alpha to all further drawing operations.
  setGlobalAlpha :: Double -> IO (),
  -- | Draws an active path as a non filled stroke.
  stroke :: IO (),
  -- | Sets the color of the next stroke actions.
  setStrokeColor :: Color -> IO (),
  -- | Sets the width of the next stroke actions.
  setStrokeWidth :: Double -> IO (),
  -- | Draws an active path as a filled object.
  fill :: IO (),
  -- | Sets the color of the next fill actions.
  setFillColor :: Color -> IO (),
  -- | Sets the width of the next fill actions.
  setFillLinearGradient :: Point -> Point -> Color -> Color -> IO (),
  -- | Moves the head to the given point. Useful for starting a set of lines.
  moveTo :: Point -> IO (),
  -- | Renders a line between to points.
  renderLine :: Point -> Point -> IO (),
  -- | Renders a line from head to a given point.
  renderLineTo :: Point -> IO (),
  -- | Renders a rectangle.
  renderRect :: Rect -> IO (),
  -- | Renders an arc (center, radius, angle start, angle, end, winding).
  renderArc :: Point -> Double -> Double -> Double -> Winding -> IO (),
  -- | Quadratic bezier segment from head via control point to target.
  renderQuadTo :: Point -> Point -> IO (),
  -- | Renders an ellipse.
  renderEllipse :: Rect -> IO (),
  -- | Renders the given text at a specific point.
  renderText :: Point -> Font -> FontSize -> Text -> IO (),
  -- | Returns the image definitio of a loaded image, if any.
  getImage :: String -> Maybe ImageDef,
  -- | Adds an image, providing name, size, image data and flags.
  addImage :: String -> Size -> ByteString -> [ImageFlag] -> IO (),
  -- | Updates an image, providing name, size and image data (must match
  -- | previous size).
  updateImage :: String -> Size -> ByteString -> IO (),
  -- | Removes an existing image.
  deleteImage :: String -> IO (),
  -- | Renders an existing image.
  renderImage :: String -> Rect -> Double -> IO (),
  -- | Renders an image after adding it, providing the same arguments as addImage.
  renderNewImage :: String -> Rect -> Double -> Size -> ByteString -> [ImageFlag] -> IO ()
}
