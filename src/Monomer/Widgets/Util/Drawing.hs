{-|
Module      : Monomer.Widgets.Util.Drawing
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Utility drawing functions. Built on top the lower level primitives provided by
"Monomer.Graphics.Types.Renderer".
-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widgets.Util.Drawing (
  drawInScissor,
  drawInTranslation,
  drawInScale,
  drawInRotation,
  drawInAlpha,
  drawTextLine,
  drawRect,
  drawRectBorder,
  drawArc,
  drawArcBorder,
  drawEllipse,
  drawEllipseBorder,
  drawArrowDown,
  drawTimesX,
  drawStyledAction,
  drawImage,
  drawNewImage
) where

import Control.Lens ((&), (^.), (^?), (^?!), (.~), non)
import Control.Monad (forM_, void, when)
import Data.ByteString (ByteString)
import Data.Default
import Data.Maybe
import Data.Text (Text)

import Monomer.Core
import Monomer.Core.StyleUtil
import Monomer.Graphics.Types

import qualified Monomer.Lens as L

-- | Performs the provided drawing operations with an active scissor, and then
-- | disables it.
drawInScissor
  :: Renderer  -- ^ The renderer.
  -> Bool      -- ^ Whether to apply the scissor (useful to selectively apply).
  -> Rect      -- ^ Scissor rect, where drawing will be visible.
  -> IO ()     -- ^ Drawing operations.
  -> IO ()     -- ^ The resulting action.
drawInScissor renderer False _ action = action
drawInScissor renderer True rect action = do
  saveContext renderer
  intersectScissor renderer rect
  action
  restoreContext renderer

-- | Performs the provided drawing operations displaced by the given offset.
drawInTranslation
  :: Renderer  -- ^ The renderer.
  -> Point     -- ^ The offset to apply.
  -> IO ()     -- ^ Drawing operations.
  -> IO ()     -- ^ The resulting action.
drawInTranslation renderer offset action = do
  saveContext renderer
  setTranslation renderer offset
  action
  restoreContext renderer

-- | Performs the provided drawing operations with the given resize scale.
drawInScale
  :: Renderer  -- ^ The renderer.
  -> Point     -- ^ The horizontal and vertical scale factor to apply.
  -> IO ()     -- ^ Drawing operations.
  -> IO ()     -- ^ The resulting action.
drawInScale renderer scale action = do
  saveContext renderer
  setScale renderer scale
  action
  restoreContext renderer

-- | Performs the provided drawing operations with the given rotation angle.
drawInRotation
  :: Renderer  -- ^ The renderer.
  -> Double    -- ^ The angle in degrees.
  -> IO ()     -- ^ Drawing operations.
  -> IO ()     -- ^ The resulting action.
drawInRotation renderer angle action = do
  saveContext renderer
  setRotation renderer angle
  action
  restoreContext renderer

-- | Performs the provided drawing operations with a global alpha applied.
drawInAlpha
  :: Renderer  -- ^ The renderer.
  -> Double    -- ^ The global alpha to apply.
  -> IO ()     -- ^ Drawing operations.
  -> IO ()     -- ^ The resulting action.
drawInAlpha renderer alpha action = do
  saveContext renderer
  setGlobalAlpha renderer alpha
  action
  restoreContext renderer

-- | Draws a TextLine with the provided style. Font and size must be the same
-- | as when the TextLine was created, but color and decorations can change.
drawTextLine
  :: Renderer    -- ^ The renderer.
  -> StyleState  -- ^ The style to apply.
  -> TextLine    -- ^ The TextLine with the text to render.
  -> IO ()       -- ^ The resulting action.
drawTextLine renderer style textLine = do
  setFillColor renderer fontColor
  renderText renderer txtOrigin font fontSize text

  when underline $ do
    drawLine renderer (Point tx uy) (Point tr uy) lw (Just fontColor)

  when overline $ do
    drawLine renderer (Point tx oy) (Point tr oy) lw (Just fontColor)

  when throughline $ do
    drawLine renderer (Point tx hy) (Point tr hy) lw (Just fontColor)
  where
    TextLine text size rect glyphs metrics = textLine
    TextMetrics asc desc lineH = metrics
    Rect tx ty tw th = rect
    tr = tx + tw
    tb = ty + th
    font = styleFont style
    fontSize = styleFontSize style
    fontColor = styleFontColor style
    alignV = styleTextAlignV style
    underline = style ^?! L.text . non def . L.underline . non False
    overline = style ^?! L.text . non def . L.overline . non False
    throughline = style ^?! L.text . non def . L.throughline . non False
    offset
      | alignV == ATBaseline = 0
      | otherwise = desc
    lw = max 1.5 (unFontSize fontSize / 20)
    by = ty + th + offset
    uy = by + 1.5 * lw
    oy = ty
    hy = by - asc * 0.35
    txtOrigin = Point tx by

-- | Draws a line with the given width and color.
drawLine
  :: Renderer     -- ^ The renderer.
  -> Point        -- ^ The start point.
  -> Point        -- ^ The end point.
  -> Double       -- ^ The line width.
  -> Maybe Color  -- ^ The color. If Nothing, the line will not be drawn.
  -> IO ()        -- ^ The resulting action.
drawLine _ _ _ _ Nothing = pure ()
drawLine renderer p1 p2 width (Just color) = do
  beginPath renderer
  setStrokeColor renderer color
  setStrokeWidth renderer width
  renderLine renderer p1 p2
  stroke renderer

-- | Draws a filled rect with the given color and radius.
drawRect
  :: Renderer      -- ^ The renderer.
  -> Rect          -- ^ The rectangle to be drawn.
  -> Maybe Color   -- ^ The color. If Nothing, the rect will not be drawn.
  -> Maybe Radius  -- ^ The optional radius config.
  -> IO ()         -- ^ The resulting action.
drawRect _ _ Nothing _ = pure ()
drawRect renderer rect (Just color) Nothing = do
  beginPath renderer
  setFillColor renderer color
  renderRect renderer rect
  fill renderer
drawRect renderer rect (Just color) (Just radius) = do
  beginPath renderer
  setFillColor renderer color
  drawRoundedRect renderer rect (fixRadius rect radius)
  fill renderer

-- | Draws a rect's border, with an optional radius.
drawRectBorder
  :: Renderer      -- ^ The renderer.
  -> Rect          -- ^ The rectangle to be drawn.
  -> Border        -- ^ The border config.
  -> Maybe Radius  -- ^ The optional radius config.
  -> IO ()         -- ^ The resulting action.
drawRectBorder renderer rect border Nothing =
  drawRectSimpleBorder renderer rect border
drawRectBorder renderer rect border (Just radius) =
  drawRectRoundedBorder renderer rect border (fixRadius rect radius)

-- | Draws a filled arc, delimited by a rect and within the given angles.
drawArc
  :: Renderer      -- ^ The renderer.
  -> Rect          -- ^ The rect delimiting the arc area.
  -> Double        -- ^ The start angle in degrees.
  -> Double        -- ^ The end angle in degrees.
  -> Winding       -- ^ The direction in which the arc is drawn.
  -> Maybe Color   -- ^ The color. If Nothing, the arc will not be drawn.
  -> IO ()         -- ^ The resulting action.
drawArc renderer rect start end winding Nothing = return ()
drawArc renderer rect start end winding (Just color) = do
  beginPath renderer
  setFillColor renderer color
  renderArc renderer center radius start end winding
  fill renderer
  where
    Rect rx ry rw rh = rect
    radius = min (rw / 2) (rh / 2)
    center = Point (rx + rw / 2) (ry + rh / 2)

-- | Draws an arc's border, delimited by a rect and within the given angles.
drawArcBorder
  :: Renderer      -- ^ The renderer.
  -> Rect          -- ^ The rect delimiting the arc area.
  -> Double        -- ^ The start angle in degrees.
  -> Double        -- ^ The end angle in degrees.
  -> Winding       -- ^ The direction in which the arc is drawn.
  -> Maybe Color   -- ^ The color. If Nothing, the arc will not be drawn.
  -> Double        -- ^ The arc width.
  -> IO ()         -- ^ The resulting action.
drawArcBorder renderer rect start end winding Nothing width = return ()
drawArcBorder renderer rect start end winding (Just color) width = do
  beginPath renderer
  setStrokeColor renderer color
  setStrokeWidth renderer width
  renderArc renderer center radius start end winding
  stroke renderer
  where
    Rect rx ry rw rh = rect
    radius = min ((rw - width) / 2) ((rh - width) / 2)
    center = Point (rx + rw / 2) (ry + rh / 2)

-- | Draws a filled ellipse, delimited by a rect.
drawEllipse
  :: Renderer      -- ^ The renderer.
  -> Rect          -- ^ The rect delimiting the ellipse.
  -> Maybe Color   -- ^ The color. If Nothing, the ellipse will not be drawn.
  -> IO ()         -- ^ The resulting action.
drawEllipse renderer rect Nothing = return ()
drawEllipse renderer rect (Just color) = do
  beginPath renderer
  setFillColor renderer color
  renderEllipse renderer rect
  fill renderer

-- | Draws an ellipse's border, delimited by a rect.
drawEllipseBorder
  :: Renderer      -- ^ The renderer.
  -> Rect          -- ^ The rect delimiting the ellipse.
  -> Maybe Color   -- ^ The color. If Nothing, the ellipse will not be drawn.
  -> Double        -- ^ The border width.
  -> IO ()         -- ^ The resulting action.
drawEllipseBorder renderer rect Nothing _ = return ()
drawEllipseBorder renderer rect (Just color) width =
  forM_ contentRect $ \finalRect -> do
    beginPath renderer
    setStrokeColor renderer color
    setStrokeWidth renderer width
    renderEllipse renderer finalRect
    stroke renderer
  where
    contentRect = subtractFromRect rect w w w w
    w = width / 2

-- | Draws a triangular arrow pointing down, delimited by the given rect.
drawArrowDown
  :: Renderer      -- ^ The renderer.
  -> Rect          -- ^ The rect delimiting the arrow.
  -> Maybe Color   -- ^ The color. If Nothing, the arrow will not be drawn.
  -> IO ()         -- ^ The resulting action.
drawArrowDown renderer rect Nothing = return ()
drawArrowDown renderer rect (Just color) = do
  beginPath renderer
  setFillColor renderer color
  moveTo renderer p1
  renderLineTo renderer p2
  renderLineTo renderer p3
  renderLineTo renderer p1
  fill renderer
  where
    Rect x y w h = rect
    p1 = Point x y
    p2 = Point (x + w) y
    p3 = Point (x + w / 2) (y + h)

-- | Draws an X, delimited by the given rect.
drawTimesX
  :: Renderer      -- ^ The renderer.
  -> Rect          -- ^ The rect delimiting the arrow.
  -> Double        -- ^ The width of the lines.
  -> Maybe Color   -- ^ The color. If Nothing, the X will not be drawn.
  -> IO ()         -- ^ The resulting action.
drawTimesX renderer rect lw Nothing = return ()
drawTimesX renderer rect lw (Just fgColor) = do
  beginPath renderer
  setFillColor renderer fgColor
  moveTo renderer (Point (x + hw) y)
  renderLineTo renderer (Point cx (cy - hw))
  renderLineTo renderer (Point (mx - hw) y)
  renderLineTo renderer (Point mx (y + hw))
  renderLineTo renderer (Point (cx + hw) cy)
  renderLineTo renderer (Point mx (my - hw))
  renderLineTo renderer (Point (mx - hw) my)
  renderLineTo renderer (Point cx (cy + hw))
  renderLineTo renderer (Point (x + hw) my)
  renderLineTo renderer (Point x (my - hw))
  renderLineTo renderer (Point (cx - hw) cy)
  renderLineTo renderer (Point x (y + hw))
  renderLineTo renderer (Point (x + hw) y)
  fill renderer
  where
    Rect x y w h = rect
    hw = lw / 2
    cx = x + w / 2
    cy = y + h / 2
    mx = x + w
    my = y + h

-- | Draws a set of operations after drawing the style's background, and
-- | before drawing the style's border.
drawStyledAction
  :: Renderer         -- ^ The renderer.
  -> Rect             -- ^ The rect where background and border will be drawn.
  -> StyleState       -- ^ The style defining background and border.
  -> (Rect -> IO ())  -- ^ The drawing actions. They receive the content area.
  -> IO ()            -- ^ The resulting action.
drawStyledAction renderer rect style action = do
  let StyleState{..} = style
  let contentRect = removeOuterBounds style rect

  drawRect renderer rect _sstBgColor _sstRadius

  forM_ contentRect action

  when (isJust _sstBorder) $
    drawRectBorder renderer rect (fromJust _sstBorder) _sstRadius

-- | Draws an already registered image in the provided location.
drawImage
  :: Renderer         -- ^ The renderer.
  -> String           -- ^ The name of the image.
  -> Rect             -- ^ The rect where the image will be drawn.
  -> Double           -- ^ The alpha to apply to the image.
  -> IO ()            -- ^ The resulting action.
drawImage renderer imgName rect alpha = action where
  action = renderImage renderer imgName rect alpha

-- | Draws a new image in the provided location.
drawNewImage
  :: Renderer         -- ^ The renderer.
  -> String           -- ^ The name of the image.
  -> Rect             -- ^ The rect where the image will be drawn.
  -> Double           -- ^ The alpha to apply to the image in this render.
  -> Size             -- ^ The image size.
  -> ByteString       -- ^ The image data as RGBA 4-byte blocks.
  -> [ImageFlag]      -- ^ The image flags.
  -> IO ()            -- ^ The resulting action.
drawNewImage renderer imgName rect alpha size imgData flags = action where
  action = renderNewImage renderer imgName rect alpha size imgData flags

-- Helpers
drawRoundedRect :: Renderer -> Rect -> Radius -> IO ()
drawRoundedRect renderer (Rect x y w h) Radius{..} =
  let
    xl = x
    xr = x + w
    yt = y
    yb = y + h
    x1 = x + radW _radTopLeft
    x2 = x + w - radW _radTopRight
    x3 = x + w - radW _radBottomRight
    x4 = x + radW _radBottomLeft
    y1 = y + radW _radTopLeft
    y2 = y + radW _radTopRight
    y3 = y + h - radW _radBottomRight
    y4 = y + h - radW _radBottomLeft
  in do
    moveTo renderer (Point x1 y1)

    when (isJust _radTopLeft) $
      renderArc renderer (Point x1 y1) (radW _radTopLeft) 180 270 CW
    renderLineTo renderer (Point x2 yt)

    when (isJust _radTopRight) $
      renderArc renderer (Point x2 y2) (radW _radTopRight) 270 0 CW
    renderLineTo renderer (Point xr y3)

    when (isJust _radBottomRight) $
      renderArc renderer (Point x3 y3) (radW _radBottomRight) 0 90 CW
    renderLineTo renderer (Point x4 yb)

    when (isJust _radBottomLeft) $
      renderArc renderer (Point x4 y4) (radW _radBottomLeft) 90 180 CW
    renderLineTo renderer (Point xl y1)

drawRectSimpleBorder :: Renderer -> Rect -> Border -> IO ()
drawRectSimpleBorder renderer rt@(Rect xl yt w h) border@Border{..} =
  let
    xr = xl + w
    yb = yt + h
    xlb = xl + halfWidth _brdLeft
    xrb = xr - halfWidth _brdRight
    ytb = yt + halfWidth _brdTop
    ybb = yb - halfWidth _brdBottom
    halfWidth bs
      | isJust bs = _bsWidth (fromJust bs) / 2
      | otherwise = 0
  in do
    strokeBorder renderer (p2 xl ytb) (p2 xr ytb) _brdTop
    strokeBorder renderer (p2 xrb yt) (p2 xrb yb) _brdRight
    strokeBorder renderer (p2 xr ybb) (p2 xl ybb) _brdBottom
    strokeBorder renderer (p2 xlb yb) (p2 xlb yt) _brdLeft

    drawRectCorners renderer rt border

drawRectCorners :: Renderer -> Rect -> Border -> IO ()
drawRectCorners renderer (Rect xl yt w h) Border{..} =
  let
    xr = xl + w
    yb = yt + h
    xlb2 = xl + sw _brdLeft
    xrb2 = xr - sw _brdRight
    ytb2 = yt + sw _brdTop
    ybb2 = yb - sw _brdBottom
    sw bs = maybe 0 _bsWidth bs
    borderL = _brdLeft
    borderR = _brdRight
    borderT = _brdTop
    borderB = _brdBottom
  in do
    drawCorner renderer (p2 xl yt) (p2 xlb2 ytb2) (p2 xlb2 yt) borderT borderL
    drawCorner renderer (p2 xrb2 yt) (p2 xrb2 ytb2) (p2 xr yt) borderT borderR
    drawCorner renderer (p2 xrb2 ybb2) (p2 xr yb) (p2 xr ybb2) borderR borderB
    drawCorner renderer (p2 xlb2 yb) (p2 xlb2 ybb2) (p2 xl yb) borderB borderL

drawCorner
  :: Renderer
  -> Point
  -> Point
  -> Point
  -> Maybe BorderSide
  -> Maybe BorderSide
  -> IO ()
drawCorner renderer p1 p2 p3 (Just bs1) (Just bs2) = do
  beginPath renderer
  setFillColor renderer (_bsColor bs1)
  moveTo renderer p1
  renderLineTo renderer p2
  renderLineTo renderer p3
  renderLineTo renderer p1
  fill renderer
drawCorner renderer p1 p2 p3 _ _ = return ()

drawRectRoundedBorder :: Renderer -> Rect -> Border -> Radius -> IO ()
drawRectRoundedBorder renderer rect border radius =
  let
    Rect xl yt w h = rect
    Border{..} = border
    Radius{..} = radius
    xr = xl + w
    yb = yt + h
    xlb = xl + halfWidth _brdLeft
    xrb = xr - halfWidth _brdRight
    ytb = yt + halfWidth _brdTop
    ybb = yb - halfWidth _brdBottom
    xlb2 = xl + 2 * halfWidth _brdLeft
    xrb2 = xr - 2 * halfWidth _brdRight
    ytb2 = yt + 2 * halfWidth _brdTop
    ybb2 = yb - 2 * halfWidth _brdBottom
    xt1 = xl + tlBorderSize border radius
    xt2 = xr - trBorderSize border radius
    yl1 = yt + tlBorderSize border radius
    yl2 = yb - blBorderSize border radius
    xb1 = xl + blBorderSize border radius
    xb2 = xr - brBorderSize border radius
    yr1 = yt + trBorderSize border radius
    yr2 = yb - brBorderSize border radius
    halfWidth bs
      | isJust bs = _bsWidth (fromJust bs) / 2
      | otherwise = 0
  in do
    strokeBorder renderer (p2 xt1 ytb) (p2 xt2 ytb) _brdTop
    strokeBorder renderer (p2 xrb yr1) (p2 xrb yr2) _brdRight
    strokeBorder renderer (p2 xb1 ybb) (p2 xb2 ybb) _brdBottom
    strokeBorder renderer (p2 xlb yl1) (p2 xlb yl2) _brdLeft

    drawRoundedCorner renderer
      (p2 xt1 yl1) (p2 xlb2 ytb2) (p2 xlb2 yl1) (p2 xt1 ytb2) 270
      _radTopLeft _brdLeft _brdTop
    drawRoundedCorner renderer
      (p2 xt2 yr1) (p2 xrb2 ytb2) (p2 xt2 ytb2) (p2 xrb2 yr1) 0
      _radTopRight _brdTop _brdRight
    drawRoundedCorner renderer
      (p2 xb2 yr2) (p2 xrb2 ybb2) (p2 xrb2 yr2) (p2 xb2 ybb2) 90
      _radBottomRight _brdRight _brdBottom
    drawRoundedCorner renderer
      (p2 xb1 yl2) (p2 xlb2 ybb2) (p2 xb1 ybb2) (p2 xlb2 yl2) 180
      _radBottomLeft _brdBottom _brdLeft

tlBorderSize :: Border -> Radius -> Double
tlBorderSize Border{..} Radius{..}
  | justLeft && justTop && isJust _radTopLeft = radW _radTopLeft
  | otherwise = 0
  where
    justLeft = isJust _brdLeft
    justTop = isJust _brdTop

trBorderSize :: Border -> Radius -> Double
trBorderSize Border{..} Radius{..}
  | justRight && justTop && isJust _radTopRight = radW _radTopRight
  | otherwise = 0
  where
    justRight = isJust _brdRight
    justTop = isJust _brdTop

blBorderSize :: Border -> Radius -> Double
blBorderSize Border{..} Radius{..}
  | justLeft && justBottom && justRad = radW _radBottomLeft
  | otherwise = 0
  where
    justLeft = isJust _brdLeft
    justBottom = isJust _brdBottom
    justRad = isJust _radBottomLeft

brBorderSize :: Border -> Radius -> Double
brBorderSize Border{..} Radius{..}
  | justRight && justBottom && justRad = radW _radBottomRight
  | otherwise = 0
  where
    justRight = isJust _brdRight
    justBottom = isJust _brdBottom
    justRad = isJust _radBottomRight

drawRoundedCorner
  :: Renderer
  -> Point
  -> Point
  -> Point
  -> Point
  -> Double
  -> Maybe RadiusCorner
  -> Maybe BorderSide
  -> Maybe BorderSide
  -> IO ()
drawRoundedCorner renderer c1 c2 p1 p2 deg (Just cor) (Just s1) (Just s2) = do
  let
    width1 = _bsWidth s1
    width2 = _bsWidth s2
    color1 = _bsColor s1
    color2 = _bsColor s2
    radSize = _rcrWidth cor

  beginPath renderer

  if color1 == color2
    then setFillColor renderer color1
    else setFillLinearGradient renderer p1 p2 color1 color2

  when (_rcrType cor == RadiusBoth) $
    renderArc renderer c1 radSize deg (deg - 90) CCW

  when (_rcrType cor == RadiusInner) $
    renderRectCorner renderer c1 radSize deg

  renderLineTo renderer p1

  if abs (width2 - width1) < 0.5
    then renderArc renderer c1 (radSize - width1) (deg - 90) deg CW
    else renderQuadTo renderer c2 p2

  closePath renderer
  fill renderer
drawRoundedCorner renderer c1 c2 p1 p2 deg _ _ _ = return ()

renderRectCorner :: Renderer -> Point -> Double -> Double -> IO ()
renderRectCorner renderer c1 width fromRad = do
  moveTo renderer p1
  renderLineTo renderer p2
  renderLineTo renderer p3
  where
    (dx, dy) = case fromRad of
      0 -> (width, -width)
      90 -> (width, width)
      180 -> (-width, width)
      _ -> (-width, -width)
    t1 = addPoint c1 (Point dx 0)
    t3 = addPoint c1 (Point 0 dy)
    p1 = if fromRad == 0 || fromRad == 180 then t1 else t3
    p2 = addPoint c1 (Point dx dy)
    p3 = if fromRad == 0 || fromRad == 180 then t3 else t1

strokeBorder :: Renderer -> Point -> Point -> Maybe BorderSide -> IO ()
strokeBorder renderer from to Nothing = pure ()
strokeBorder renderer from to (Just BorderSide{..}) = do
  beginPath renderer
  setStrokeColor renderer _bsColor
  setStrokeWidth renderer _bsWidth
  moveTo renderer from
  renderLineTo renderer to
  stroke renderer

p2 :: Double -> Double -> Point
p2 x y = Point x y

radW :: Maybe RadiusCorner -> Double
radW r = _rcrWidth (fromMaybe def r)

fixRadius :: Rect -> Radius -> Radius
fixRadius (Rect _ _ w h) (Radius tl tr bl br) = newRadius where
  fixC (RadiusCorner ctype cwidth)
    | cwidth * 2 < min w h = RadiusCorner ctype cwidth
    | otherwise = RadiusCorner ctype (min w h / 2)
  newRadius = Radius (fixC <$> tl) (fixC <$> tr) (fixC <$> bl) (fixC <$> br)
