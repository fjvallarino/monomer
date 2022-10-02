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
{-# LANGUAGE Strict #-}

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
} deriving (Eq, Show, Ord, Generic)

instance Default Color where
  def = Color 255 255 255 1.0

data FontDef
  = FontDefFile
    { _fntFontName :: !Text  -- ^ The logic name. Will be used when defining styles.
    , _fntFontPath :: !Text  -- ^ The path in the filesystem.
    }
  | FontDefMem
    { _fntFontName :: !Text         -- ^ The logic name. Will be used when defining styles.
    , _fntFontBytes :: !ByteString  -- ^ The bytes of the loaded font.
    }
  deriving (Eq, Show, Generic)

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

-- | The spacing of a font. Zero represents the default spacing of the font.
newtype FontSpace
  = FontSpace { unFontSpace :: Double }
  deriving (Eq, Show, Generic)

instance Default FontSpace where
  def = FontSpace 0

-- | Represents the sides of a rectangle.
data RectSide
  = SideLeft
  | SideRight
  | SideTop
  | SideBottom
  deriving (Eq, Show)

-- | Represents the corners of a rectangle.
data RectCorner
  = CornerTL
  | CornerTR
  | CornerBR
  | CornerBL
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
  _glpX :: {-# UNPACK #-} !Double,     -- ^ The x coordinate used for rendering.
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
    _glpX = 0,
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
  _tlFont :: !Font,              -- ^ The font name.
  _tlFontSize :: !FontSize,      -- ^ The font size.
  _tlFontSpaceH :: !FontSpace,   -- ^ The font spacing.
  _tlFontSpaceV :: !FontSpace,   -- ^ The vertical line spacing.
  _tlMetrics :: !TextMetrics,    -- ^ The text metrics for the given font/size.
  _tlText :: !Text,              -- ^ The represented text.
  _tlSize :: !Size,              -- ^ The size the formatted text takes.
  _tlRect :: !Rect,              -- ^ The rect the formatted text occupies.
  _tlGlyphs :: !(Seq GlyphPos)   -- ^ The glyphs for each character.
} deriving (Eq, Show, Generic)

instance Default TextLine where
  def = TextLine {
    _tlFont = def,
    _tlFontSize = def,
    _tlFontSpaceH = def,
    _tlFontSpaceV = def,
    _tlMetrics = def,
    _tlText = "",
    _tlSize = def,
    _tlRect = def,
    _tlGlyphs = Seq.empty
  }

-- | Flags for a newly created image.
data ImageFlag
  = ImageNearest
  | ImageRepeatX
  | ImageRepeatY
  deriving (Eq, Show, Generic)

-- | The definition of a loaded image.
data ImageDef = ImageDef {
  _idfName :: Text,              -- ^ The logic name of the image.
  _idfSize :: Size,              -- ^ The dimensions of the image.
  _idfImgData :: BS.ByteString,  -- ^ The image data as RGBA 4-bytes blocks.
  _idfFlags :: [ImageFlag]       -- ^ The image flags.
} deriving (Eq, Show, Generic)

{-|
Text metrics related functions.

Two different versions of each function exist:

- Default one, without underscore, does not apply scaling.
- Version with a trailing underscore, that receives an extra scale argument.

In case the text is going to be rendered with a scale factor applied on
'Renderer' (by calling 'setScale'), it is recommended to apply the scale here
too (otherwise there will be differences in size and positioning). In most use
cases these functions will never be called, preferring the non underscore
versions.
-}
data FontManager = FontManager {
  -- | Returns the text metrics of a given font and size.
  computeTextMetrics :: Font -> FontSize -> TextMetrics,
  -- | Returns the text metrics of a given font and size, applying scale.
  computeTextMetrics_ :: Double -> Font -> FontSize -> TextMetrics,
  -- | Returns the size of the line of text given font and size.
  computeTextSize :: Font -> FontSize -> FontSpace -> Text -> Size,
  -- | Returns the size of the line of text given font and size, applying scale.
  computeTextSize_ :: Double -> Font -> FontSize -> FontSpace -> Text -> Size,
  -- | Returns the glyphs of the line of text given font and size.
  computeGlyphsPos :: Font -> FontSize -> FontSpace -> Text -> Seq GlyphPos,
  -- | Returns the glyphs of the line of text given font and size, applying scale.
  computeGlyphsPos_ :: Double -> Font -> FontSize -> FontSpace -> Text -> Seq GlyphPos
}

-- | Low level rendering definitions.
data Renderer = Renderer {
  -- | Begins a new frame.
  beginFrame :: Double -> Double -> IO (),
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
  --   displayed. Useful, for instance, for a dropdown or context menu.
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
  {-|
  Sets the winding of the shape to be drawn. Setting CCW (counter clockwise)
  means the shape will be solid. Setting CW (clockwise) means the shape will be
  a hole inside a larger solid path.
  -}
  setPathWinding :: Winding -> IO (),
  -- | Draws an active path as a non filled stroke.
  stroke :: IO (),
  -- | Sets the width of the next stroke actions.
  setStrokeWidth :: Double -> IO (),
  -- | Sets the color of the next stroke actions.
  setStrokeColor :: Color -> IO (),
  -- | Sets a linear gradient stroke from Point to Point, Color to Color.
  setStrokeLinearGradient :: Point -> Point -> Color -> Color -> IO (),
  {-|
  Sets a radial gradient stroke with center point Point, inner and outer radius,
  inner and outer Color.
  -}
  setStrokeRadialGradient :: Point -> Double -> Double -> Color -> Color -> IO (),
  {-|
  Sets a box gradient stroke with box area, corner radius, feather, inner and
  outer Color
  -}
  setStrokeBoxGradient :: Rect -> Double -> Double -> Color -> Color -> IO (),
  {-|
  Sets an image pattern stroke, with top given by Point, size of a single image
  given by size, rotation and alpha.
  -}
  setStrokeImagePattern :: Text -> Point -> Size -> Double -> Double -> IO (),
  -- | Draws an active path as a filled object.
  fill :: IO (),
  -- | Sets the color of the next fill actions.
  setFillColor :: Color -> IO (),
  -- | Sets a linear gradient fill from Point to Point, Color to Color.
  setFillLinearGradient :: Point -> Point -> Color -> Color -> IO (),
  {-|
  Sets a radial gradient fill with center point Point, inner and outer radius,
  inner and outer Color.
  -}
  setFillRadialGradient :: Point -> Double -> Double -> Color -> Color -> IO (),
  {-|
  Sets a box gradient fill with box area, corner radius, feather, inner and
  outer Color
  -}
  setFillBoxGradient :: Rect -> Double -> Double -> Color -> Color -> IO (),
  {-|
  Sets an image pattern fill, with top given by Point, size of a single image
  given by size, rotation and alpha.
  -}
  setFillImagePattern :: Text -> Point -> Size -> Double -> Double -> IO (),
  -- | Moves the head to the given point. Useful for starting a set of lines.
  moveTo :: Point -> IO (),
  -- | Renders a line between to points.
  renderLine :: Point -> Point -> IO (),
  -- | Renders a line from head to a given point.
  renderLineTo :: Point -> IO (),
  -- | Renders a rectangle.
  renderRect :: Rect -> IO (),
  -- | Renders a rectangle with rounded corners.
  renderRoundedRect :: Rect -> Double -> Double -> Double -> Double -> IO (),
  -- | Renders an arc (center, radius, angle start, angle, end, winding).
  renderArc :: Point -> Double -> Double -> Double -> Winding -> IO (),
  -- | Quadratic bezier segment from head via control point to target.
  renderQuadTo :: Point -> Point -> IO (),
  -- | Renders an ellipse.
  renderEllipse :: Rect -> IO (),
  {-|
  Renders the given text line at a specific point, with the provided font, size
  and horizontal spacing.
  -}
  renderText :: Point -> Font -> FontSize -> FontSpace -> Text -> IO (),
  -- | Returns the image definition of a loaded image, if any.
  getImage :: Text -> IO (Maybe ImageDef),
  -- | Adds an image, providing name, size, image data and flags.
  addImage :: Text -> Size -> ByteString -> [ImageFlag] -> IO (),
  -- | Updates an image, providing name, size and image data (must match
  --   previous size).
  updateImage :: Text -> Size -> ByteString -> IO (),
  -- | Removes an existing image.
  deleteImage :: Text -> IO ()
}
