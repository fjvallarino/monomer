{-# LANGUAGE RecordWildCards #-}
{- HLINT ignore "Reduce duplication" -}

module Monomer.Graphics.Drawing (
  drawRect,
  drawRectBorder,
  drawEllipse,
  drawEllipseBorder,
  drawStyledAction,
  drawStyledBackground,
  drawStyledText,
  drawStyledText_,
  drawText,
  drawImage,
  drawStyledImage
) where

import Control.Monad (when, void)
import Data.Default
import Data.Maybe
import Data.Text (Text)

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.StyleUtil
import Monomer.Graphics.Types

drawRect :: Renderer -> Rect -> Maybe Color -> Maybe Radius -> IO ()
drawRect _ _ Nothing _ = pure ()
drawRect renderer viewport (Just color) Nothing = do
  beginPath renderer
  setFillColor renderer color
  renderRect renderer viewport
  fill renderer
drawRect renderer viewport (Just color) (Just radius) = do
  beginPath renderer
  setFillColor renderer color
  drawRoundedRect renderer viewport radius
  fill renderer

drawRectBorder :: Renderer -> Rect -> Border -> Maybe Radius -> IO ()
drawRectBorder renderer rect border Nothing =
  drawRectSimpleBorder renderer rect border
drawRectBorder renderer rect border (Just radius) =
  drawRectRoundedBorder renderer rect border radius

drawEllipse :: Renderer -> Rect -> Maybe Color -> IO ()
drawEllipse renderer rect Nothing = return ()
drawEllipse renderer rect (Just color) = do
  beginPath renderer
  setFillColor renderer color
  renderEllipse renderer rect
  fill renderer

drawEllipseBorder :: Renderer -> Rect -> Maybe Color -> Double -> IO ()
drawEllipseBorder renderer rect Nothing _ = return ()
drawEllipseBorder renderer rect (Just color) width = do
  beginPath renderer
  setStrokeColor renderer color
  setStrokeWidth renderer width
  renderEllipse renderer finalRect
  stroke renderer
  where
    finalRect = subtractFromRect rect w w w w
    w = width / 2

drawStyledAction :: Renderer -> Rect -> StyleState -> (Rect -> IO ()) -> IO ()
drawStyledAction renderer viewport style action = do
  let StyleState{..} = style
  let margin = _sstMargin
  let rect = subtractMargin viewport margin
  let contentRect = removeOuterBounds style viewport

  drawRect renderer rect _sstBgColor _sstRadius
  action contentRect

  when (isJust _sstBorder) $
    drawRectBorder renderer rect (fromJust _sstBorder) _sstRadius

drawStyledBackground :: Renderer -> Rect -> StyleState -> IO ()
drawStyledBackground renderer viewport style =
  drawStyledAction renderer viewport style (\_ -> return ())

drawStyledText :: Renderer -> Rect -> StyleState -> Text -> IO Rect
drawStyledText renderer viewport style txt = action where
  action = drawText renderer viewport tsColor tsFont tsFontSize tsAlign txt
  TextStyle{..} = fromMaybe def (_sstText style)
  tsColor = justDef _txsFontColor
  tsFont = justDef _txsFont
  tsFontSize = fromMaybe def _txsFontSize
  tsAlignH = justDef _txsAlignH
  tsAlignV = justDef _txsAlignV
  tsAlign = Align tsAlignH tsAlignV

drawStyledText_ :: Renderer -> Rect -> StyleState -> Text -> IO ()
drawStyledText_ renderer viewport style txt = void action where
  action = drawStyledText renderer viewport style txt

drawText
  :: Renderer
  -> Rect
  -> Color
  -> Font
  -> FontSize
  -> Align
  -> Text
  -> IO Rect
drawText renderer viewport color font fontSize align txt = do
    setFillColor renderer color
    renderText renderer viewport font fontSize align txt

drawImage :: Renderer -> Rect -> String -> IO ()
drawImage renderer viewport imgName = action where
  action = renderImage renderer viewport imgName

drawStyledImage :: Renderer -> Rect -> StyleState -> String -> IO ()
drawStyledImage renderer viewport style imgName = action where
  imgRect = removeOuterBounds style viewport
  action = renderImage renderer imgRect imgName

drawRoundedRect :: Renderer -> Rect -> Radius -> IO ()
drawRoundedRect renderer (Rect x y w h) Radius{..} =
  let
    xl = x
    xr = x + w
    yt = y
    yb = y + h
    x1 = x + justDef _radTopLeft
    x2 = x + w - justDef _radTopRight
    x3 = x + w - justDef _radBottomRight
    x4 = x + justDef _radBottomLeft
    y1 = y + justDef _radTopLeft
    y2 = y + justDef _radTopRight
    y3 = y + h - justDef _radBottomRight
    y4 = y + h - justDef _radBottomLeft
  in do
    moveTo renderer (Point x1 y1)

    when (isJust _radTopLeft) $
      renderArc renderer (Point x1 y1) (fromJust _radTopLeft) 180 270 CW
    renderLineTo renderer (Point x2 yt)

    when (isJust _radTopRight) $
      renderArc renderer (Point x2 y2) (justDef _radTopRight) 270 0 CW
    renderLineTo renderer (Point xr y3)

    when (isJust _radBottomRight) $
      renderArc renderer (Point x3 y3) (justDef _radBottomRight) 0 90 CW
    renderLineTo renderer (Point x4 yb)

    when (isJust _radBottomLeft) $
      renderArc renderer (Point x4 y4) (justDef _radBottomLeft) 90 180 CW
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
  | justLeft && justTop && isJust _radTopLeft = fromJust _radTopLeft
  | otherwise = 0
  where
    justLeft = isJust _brdLeft
    justTop = isJust _brdTop

trBorderSize :: Border -> Radius -> Double
trBorderSize Border{..} Radius{..}
  | justRight && justTop && isJust _radTopRight = fromJust _radTopRight
  | otherwise = 0
  where
    justRight = isJust _brdRight
    justTop = isJust _brdTop

blBorderSize :: Border -> Radius -> Double
blBorderSize Border{..} Radius{..}
  | justLeft && justBottom && justRad = fromJust _radBottomLeft
  | otherwise = 0
  where
    justLeft = isJust _brdLeft
    justBottom = isJust _brdBottom
    justRad = isJust _radBottomLeft

brBorderSize :: Border -> Radius -> Double
brBorderSize Border{..} Radius{..}
  | justRight && justBottom && justRad = fromJust _radBottomRight
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
  -> Maybe Double
  -> Maybe BorderSide
  -> Maybe BorderSide
  -> IO ()
drawRoundedCorner renderer c1 c2 p1 p2 deg (Just rad) (Just s1) (Just s2) = do
  let
    width1 = _bsWidth s1
    width2 = _bsWidth s2
    color1 = _bsColor s1
    color2 = _bsColor s2

  beginPath renderer

  if color1 == color2
    then setFillColor renderer color1
    else setFillLinearGradient renderer p1 p2 color1 color2

  renderArc renderer c1 rad deg (deg - 90) CCW
  renderLineTo renderer p1

  if abs (width2 - width1) < 0.5
    then renderArc renderer c1 (rad - width1) (deg - 90) deg CW
    else renderQuadTo renderer c2 p2

  closePath renderer
  fill renderer
drawRoundedCorner renderer c1 c2 p1 p2 deg _ _ _ = return ()

strokeBorder :: Renderer -> Point -> Point -> Maybe BorderSide -> IO ()
strokeBorder renderer from to Nothing = pure ()
strokeBorder renderer from to (Just BorderSide{..}) = do
  beginPath renderer
  setStrokeColor renderer _bsColor
  setStrokeWidth renderer _bsWidth
  moveTo renderer from
  renderLineTo renderer to
  stroke renderer

justDef :: (Default a) => Maybe a -> a
justDef val = fromMaybe def val

p2 :: Double -> Double -> Point
p2 x y = Point x y
