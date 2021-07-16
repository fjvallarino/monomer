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
  drawRoundedRect,
  drawRectRoundedBorder
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (^?), (^?!), (.~), non)
import Control.Monad (forM_, void, when)
import Data.ByteString (ByteString)
import Data.Default
import Data.Maybe
import Data.Text (Text)

import Monomer.Core
import Monomer.Graphics.Types

import qualified Monomer.Common.Lens as L
import qualified Monomer.Core.Lens as L
import qualified Monomer.Graphics.Lens as L

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
  renderText renderer txtOrigin _tlFont _tlFontSize _tlFontSpaceH _tlText

  when underline $ do
    drawLine renderer (Point tx uy) (Point tr uy) lw (Just fontColor)

  when overline $ do
    drawLine renderer (Point tx oy) (Point tr oy) lw (Just fontColor)

  when throughline $ do
    drawLine renderer (Point tx hy) (Point tr hy) lw (Just fontColor)
  where
    TextLine{..} = textLine
    TextMetrics asc desc _ _ = _tlMetrics
    Rect tx ty tw th = _tlRect
    tr = tx + tw
    fontColor = styleFontColor style
    alignV = styleTextAlignV style
    underline = style ^?! L.text . non def . L.underline . non False
    overline = style ^?! L.text . non def . L.overline . non False
    throughline = style ^?! L.text . non def . L.throughline . non False
    offset
      | alignV == ATBaseline = 0
      | otherwise = desc
    {- There's not a scientific reason for choosing 1/20 as the scale, it just
    looked reasonably good as the line width on a set of different fonts. -}
    lw = max 1.5 (unFontSize _tlFontSize / 20)
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
  drawRoundedRect renderer rect radius
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
  drawRectRoundedBorder renderer rect border radius

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
  drawRect renderer rect _sstBgColor _sstRadius

  forM_ contentRect action

  when (isJust _sstBorder) $
    drawRectBorder renderer rect (fromJust _sstBorder) _sstRadius
  where
    StyleState{..} = style
    contentRect = removeOuterBounds style rect

drawRoundedRect :: Renderer -> Rect -> Radius -> IO ()
drawRoundedRect renderer rect radius =
  let
    Rect _ _ w h = rect
    Radius{..} = fixRadius rect radius
    midw = min w h / 2
    validTL = min midw (radW _radTopLeft)
    validTR = min midw (radW _radTopRight)
    validBR = min midw (radW _radBottomRight)
    validBL = min midw (radW _radBottomLeft)
  in do
    renderRoundedRect renderer rect validTL validTR validBR validBL

drawRectSimpleBorder :: Renderer -> Rect -> Border -> IO ()
drawRectSimpleBorder renderer (Rect x y w h) Border{..} =
  let
    ptl = Point x y
    ptr = Point (x + w) y
    pbr = Point (x + w) (y + h)
    pbl = Point x (y + h)
    borderL = _brdLeft
    borderR = _brdRight
    borderT = _brdTop
    borderB = _brdBottom
  in do
    (olt, otl, itl) <- drawRectCorner renderer CornerTL ptl borderL borderT
    (otr, ort, itr) <- drawRectCorner renderer CornerTR ptr borderT borderR
    (orb, obr, ibr) <- drawRectCorner renderer CornerBR pbr borderR borderB
    (obl, olb, ibl) <- drawRectCorner renderer CornerBL pbl borderB borderL
    drawQuad renderer otl otr itr itl borderT
    drawQuad renderer ort orb ibr itr borderR
    drawQuad renderer obr obl ibl ibr borderB
    drawQuad renderer olb olt itl ibl borderL

drawRectCorner
  :: Renderer
  -> RectCorner
  -> Point
  -> Maybe BorderSide
  -> Maybe BorderSide
  -> IO (Point, Point, Point)
drawRectCorner _ _ ocorner Nothing Nothing = return points where
  points = (ocorner, ocorner, ocorner)
drawRectCorner renderer cor ocorner ms1 ms2 = do
  beginPath renderer

  if color1 == color2
    then setFillColor renderer color1
    else setFillLinearGradient renderer g1 g2 color1 color2

  moveTo renderer o1
  renderLineTo renderer icorner
  renderLineTo renderer o2
  renderLineTo renderer ocorner
  closePath renderer

  fill renderer
  return (o1, o2, icorner)
  where
    Point cx cy = ocorner
    s1 = fromMaybe def ms1
    s2 = fromMaybe def ms2
    w1 = _bsWidth s1
    w2 = _bsWidth s2
    color1 = _bsColor (fromJust (ms1 <|> ms2))
    color2 = _bsColor (fromJust (ms2 <|> ms1))
    (o1, o2) = case cor of
      CornerTL -> (Point cx (cy + w2), Point (cx + w1) cy)
      CornerTR -> (Point (cx - w2) cy, Point cx (cy + w1))
      CornerBR -> (Point cx (cy - w2), Point (cx - w1) cy)
      CornerBL -> (Point (cx + w2) cy, Point cx (cy - w1))
    icorner = case cor of
      CornerTL -> Point (cx + w1) (cy + w2)
      CornerTR -> Point (cx - w2) (cy + w1)
      CornerBR -> Point (cx - w1) (cy - w2)
      CornerBL -> Point (cx + w2) (cy - w1)
    (g1, g2) = cornerGradientPoints ocorner icorner

drawRectRoundedBorder :: Renderer -> Rect -> Border -> Radius -> IO ()
drawRectRoundedBorder renderer rect border radius =
  let
    Rect xl yt w h = rect
    Border borL borR borT borB = border
    Radius radTL radTR radBL radBR = fixRadius rect radius
    xr = xl + w
    yb = yt + h
    hw = w / 2
    hh = h / 2
    midw = min w h / 2
    rtl = Rect xl yt hw hh
    rtr = Rect (xl + hw) yt hw hh
    rbr = Rect (xl + hw) (yt + hh) hw hh
    rbl = Rect xl (yt + hh) hw hh
    validTL = min midw (radW radTL)
    validTR = min midw (radW radTR)
    validBR = min midw (radW radBR)
    validBL = min midw (radW radBL)
    xt1 = xl + validTL
    yl1 = yt + validTL
    xt2 = xr - validTR
    yr1 = yt + validTR
    xb2 = xr - validBR
    yr2 = yb - validBR
    yl2 = yb - validBL
    xb1 = xl + validBL
  in do
    (lt1, lt2, tl1, tl2) <- drawRoundedCorner renderer CornerTL rtl (p2 xt1 yl1) radTL borL borT
    (tr1, tr2, rt1, rt2) <- drawRoundedCorner renderer CornerTR rtr (p2 xt2 yr1) radTR borT borR
    (rb1, rb2, br1, br2) <- drawRoundedCorner renderer CornerBR rbr (p2 xb2 yr2) radBR borR borB
    (bl1, bl2, lb1, lb2) <- drawRoundedCorner renderer CornerBL rbl (p2 xb1 yl2) radBL borB borL

    drawQuad renderer lb1 lt1 lt2 lb2 borL
    drawQuad renderer tl1 tr1 tr2 tl2 borT
    drawQuad renderer rt1 rb1 rb2 rt2 borR
    drawQuad renderer br1 bl1 bl2 br2 borB

drawRoundedCorner
  :: Renderer
  -> RectCorner
  -> Rect
  -> Point
  -> Maybe RadiusCorner
  -> Maybe BorderSide
  -> Maybe BorderSide
  -> IO (Point, Point, Point, Point)
drawRoundedCorner _ _ _ center _ Nothing Nothing = return points where
  points = (center, center, center, center)
drawRoundedCorner renderer cor bounds ocenter mrcor ms1 ms2 = do
  beginPath renderer

  if color1 == color2
    then setFillColor renderer color1
    else setFillLinearGradient renderer g1 g2 color1 color2

  if round orad == 0
    then drawRectArc renderer cor icenter w1 w2
    else renderArc renderer ocenter orad deg (deg - 90) CCW

  renderLineTo renderer o1

  if round orad > 0 && round irad > 0
    then do
      renderLineTo renderer i1
      renderArc renderer icenter irad (deg - 90) deg CW
      renderLineTo renderer i2
    else do
      renderLineTo renderer icenter

  renderLineTo renderer o2

  closePath renderer
  fill renderer

  return bordersCorners
  where
    Point ocx ocy = ocenter
    Point icx icy = icenter
    rcor = fromMaybe def mrcor
    s1 = fromMaybe def ms1
    s2 = fromMaybe def ms2
    w1 = _bsWidth s1
    w2 = _bsWidth s2
    color1 = _bsColor (fromJust (ms1 <|> ms2))
    color2 = _bsColor (fromJust (ms2 <|> ms1))
    minW = min w1 w2
    orad = max 0 (_rcrWidth rcor)
    irad = max 0 (orad - minW)
    omax1 = max orad w1
    omax2 = max orad w2
    cxmin = min ocx icx
    cxmax = max ocx icx
    cymin = min ocy icy
    cymax = max ocy icy
    restrict (p1, p2) = (rectBoundedPoint bounds p1, rectBoundedPoint bounds p2)
    (deg, icenter) = case cor of
      CornerTL -> (270, Point (ocx - orad + w1 + irad) (ocy - orad + w2 + irad))
      CornerTR -> (  0, Point (ocx + orad - w2 - irad) (ocy - orad + w1 + irad))
      CornerBR -> ( 90, Point (ocx + orad - w1 - irad) (ocy + orad - w2 - irad))
      CornerBL -> (180, Point (ocx - orad + w2 + irad) (ocy + orad - w1 - irad))
    (o1, o2) = restrict $ case cor of
      CornerTL -> (Point (ocx - omax1) cymax, Point cxmax (ocy - omax2))
      CornerTR -> (Point cxmin (ocy - omax2), Point (ocx + omax1) cymax)
      CornerBR -> (Point (ocx + omax1) cymin, Point cxmin (ocy + omax2))
      CornerBL -> (Point cxmax (ocy + omax2), Point (ocx - omax1) cymin)
    (i1, i2) = restrict $ case cor of
      CornerTL -> (Point (ocx - orad + w1) cymax, Point cxmax (ocy - orad + w2))
      CornerTR -> (Point cxmin (ocy - orad + w1), Point (ocx + orad - w2) cymax)
      CornerBR -> (Point (ocx + orad - w1) cymin, Point cxmin (ocy + orad - w2))
      CornerBL -> (Point cxmax (ocy + orad - w1), Point (ocx - orad + w2) cymin)
    bordersCorners
      | round orad == 0 = (o1, icenter, o2, icenter)
      | otherwise = (o1, i1, o2, i2)
    ocorner = Point (o1 ^. L.x) (o2 ^. L.y)
    icorner = Point (o2 ^. L.x) (o1 ^. L.y)
    (g1, g2)
      | cor `elem` [CornerTL, CornerBR] = cornerGradientPoints ocorner icorner
      | otherwise = cornerGradientPoints icorner ocorner

drawRectArc :: Renderer -> RectCorner -> Point -> Double -> Double -> IO ()
drawRectArc renderer corner c1 pw1 pw2 = do
  moveTo renderer (addPoint c1 p1)
  renderLineTo renderer (addPoint c1 p2)
  renderLineTo renderer (addPoint c1 p3)
  where
    nw1 = -pw1
    nw2 = -pw2
    (p1, p2, p3) = case corner of
      CornerTL -> (Point 0 nw2, Point nw1 nw2, Point nw1 0)
      CornerTR -> (Point pw2 0, Point pw2 nw1, Point 0 nw1)
      CornerBR -> (Point 0 pw2, Point pw1 pw2, Point pw1 0)
      CornerBL -> (Point nw2 0, Point nw2 pw1, Point 0 pw1)

drawQuad :: Renderer -> Point -> Point -> Point -> Point -> Maybe BorderSide -> IO ()
drawQuad renderer p1 p2 p3 p4 Nothing = pure ()
drawQuad renderer p1 p2 p3 p4 (Just BorderSide{..}) = do
  beginPath renderer
  setFillColor renderer _bsColor
  moveTo renderer p1
  renderLineTo renderer p2
  renderLineTo renderer p3
  renderLineTo renderer p4
  closePath renderer
  fill renderer

cornerGradientPoints :: Point -> Point -> (Point, Point)
cornerGradientPoints outer inner = (g1, g2) where
  Point ox oy = outer
  Point ix iy = inner
  Point mx my = midPoint outer inner
  (vx, vy) = (ix - ox, iy - oy)
  (nx, ny) = (vy, -vx)
  factor = 0.01
  g1 = Point (mx - factor * nx) (my - factor * ny)
  g2 = Point (mx + factor * nx) (my + factor * ny)

p2 :: Double -> Double -> Point
p2 x y = Point x y

radW :: Maybe RadiusCorner -> Double
radW r = _rcrWidth (fromMaybe def r)

fixRadius :: Rect -> Radius -> Radius
fixRadius (Rect _ _ w h) (Radius tl tr bl br) = newRadius where
  fixC (RadiusCorner cwidth)
    | cwidth < min w h / 2= RadiusCorner cwidth
    | otherwise = RadiusCorner (min w h / 2)
  newRadius = Radius (fixC <$> tl) (fixC <$> tr) (fixC <$> bl) (fixC <$> br)
