{-# LANGUAGE RecordWildCards #-}

module Monomer.Graphics.Drawing where

import Control.Monad (when, void, forM_)
import Data.Default
import Data.Maybe
import Data.Text (Text)

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Graphics.Renderer
import Monomer.Graphics.Types
import Monomer.Graphics.Util

drawStyledBackground :: (Monad m) => Renderer m -> Rect -> Style -> m ()
drawStyledBackground renderer viewport Style{..} = do
  let rect = subtractMargin viewport _styleMargin

  drawRect renderer rect _styleColor _styleRadius

  when (isJust _styleBorder) $
    drawStyledBorder renderer rect (fromJust _styleBorder) _styleRadius

drawRect
  :: (Monad m) => Renderer m -> Rect -> Maybe Color -> Maybe Radius -> m ()
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

drawRoundedRect :: (Monad m) => Renderer m -> Rect -> Radius -> m ()
drawRoundedRect renderer (Rect x y w h) Radius{..} =
  let
    xl = x
    xr = x + w
    yt = y
    yb = y + h
    x1 = x + justDef _radiusTopLeft
    x2 = x + w - justDef _radiusTopRight
    x3 = x + w - justDef _radiusBottomRight
    x4 = x + justDef _radiusBottomLeft
    y1 = y + justDef _radiusTopLeft
    y2 = y + justDef _radiusTopRight
    y3 = y + h - justDef _radiusBottomRight
    y4 = y + h - justDef _radiusBottomLeft
  in do
    moveTo renderer (Point x1 y1)

    when (isJust _radiusTopLeft) $
      renderArc renderer (Point x1 y1) (fromJust _radiusTopLeft) 180 270 CW
    renderLineTo renderer (Point x2 yt)

    when (isJust _radiusTopRight) $
      renderArc renderer (Point x2 y2) (justDef _radiusTopRight) 270 0 CW
    renderLineTo renderer (Point xr y3)

    when (isJust _radiusBottomRight) $
      renderArc renderer (Point x3 y3) (justDef _radiusBottomRight) 0 90 CW
    renderLineTo renderer (Point x4 yb)

    when (isJust _radiusBottomLeft) $
      renderArc renderer (Point x4 y4) (justDef _radiusBottomLeft) 90 180 CW
    renderLineTo renderer (Point xl y1)

drawStyledBorder
  :: (Monad m) => Renderer m -> Rect -> Border -> Maybe Radius -> m ()
drawStyledBorder renderer rect border Nothing =
  drawBorder renderer rect border
drawStyledBorder renderer rect border (Just radius) =
  drawRoundedBorder renderer rect border radius

drawBorder :: (Monad m) => Renderer m -> Rect -> Border -> m ()
drawBorder renderer rt@(Rect xl yt w h) border@Border{..} =
  let
    xr = xl + w
    yb = yt + h
    xlb = xl + sw _borderLeft
    xrb = xr - sw _borderRight
    ytb = yt + sw _borderTop
    ybb = yb - sw _borderBottom
    sw bs = if isJust bs then _borderSideWidth (fromJust bs) / 2 else 0
  in do
    strokeBorder renderer (p2 xl ytb) (p2 xr ytb) _borderTop
    strokeBorder renderer (p2 xrb yt) (p2 xrb yb) _borderRight
    strokeBorder renderer (p2 xr ybb) (p2 xl ybb) _borderBottom
    strokeBorder renderer (p2 xlb yb) (p2 xlb yt) _borderLeft

    drawCorners renderer rt border

drawCorners :: Monad m => Renderer m -> Rect -> Border -> m ()
drawCorners renderer (Rect xl yt w h) Border{..} =
  let
    xr = xl + w
    yb = yt + h
    xlb2 = xl + sw _borderLeft
    xrb2 = xr - sw _borderRight
    ytb2 = yt + sw _borderTop
    ybb2 = yb - sw _borderBottom
    sw bs = maybe 0 _borderSideWidth bs
    borderL = _borderLeft
    borderR = _borderRight
    borderT = _borderTop
    borderB = _borderBottom
  in do
    drawCorner renderer (p2 xl yt) (p2 xlb2 ytb2) (p2 xlb2 yt) borderT borderL
    drawCorner renderer (p2 xrb2 yt) (p2 xrb2 ytb2) (p2 xr yt) borderT borderR
    drawCorner renderer (p2 xrb2 ybb2) (p2 xr yb) (p2 xr ybb2) borderR borderB
    drawCorner renderer (p2 xlb2 yb) (p2 xlb2 ybb2) (p2 xl yb) borderB borderL

drawCorner
  :: (Monad m)
  => Renderer m
  -> Point
  -> Point
  -> Point
  -> Maybe BorderSide
  -> Maybe BorderSide
  -> m ()
drawCorner renderer p1 p2 p3 (Just bs1) (Just bs2) = do
  beginPath renderer
  setFillColor renderer (_borderSideColor bs1)
  moveTo renderer p1
  renderLineTo renderer p2
  renderLineTo renderer p3
  renderLineTo renderer p1
  fill renderer
drawCorner renderer p1 p2 p3 _ _ = return ()

drawRoundedBorder :: (Monad m) => Renderer m -> Rect -> Border -> Radius -> m ()
drawRoundedBorder renderer rect border@Border{..} radius@Radius{..} =
  let
    Rect xl yt w h = rect
    xr = xl + w
    yb = yt + h
    xlb = xl + swr _borderLeft
    xrb = xr - swr _borderRight
    ytb = yt + swr _borderTop
    ybb = yb - swr _borderBottom
    xlb2 = xl + 2 * swr _borderLeft
    xrb2 = xr - 2 * swr _borderRight
    ytb2 = yt + 2 * swr _borderTop
    ybb2 = yb - 2 * swr _borderBottom
    xt1 = xl + topLeftBorderSize border radius
    xt2 = xr - topRightBorderSize border radius
    yl1 = yt + topLeftBorderSize border radius
    yl2 = yb - bottomLeftBorderSize border radius
    xb1 = xl + bottomLeftBorderSize border radius
    xb2 = xr - bottomRightBorderSize border radius
    yr1 = yt + topRightBorderSize border radius
    yr2 = yb - bottomRightBorderSize border radius
    swr bs = if isJust bs then _borderSideWidth (fromJust bs) / 2 else 0
  in do
    strokeBorder renderer (p2 xt1 ytb) (p2 xt2 ytb) _borderTop
    strokeBorder renderer (p2 xrb yr1) (p2 xrb yr2) _borderRight
    strokeBorder renderer (p2 xb1 ybb) (p2 xb2 ybb) _borderBottom
    strokeBorder renderer (p2 xlb yl1) (p2 xlb yl2) _borderLeft

    drawRoundedCorner renderer
      (p2 xt1 yl1) (p2 xlb2 ytb2) (p2 xlb2 yl1) (p2 xt1 ytb2) 270
      _radiusTopLeft _borderLeft _borderTop
    drawRoundedCorner renderer
      (p2 xt2 yr1) (p2 xrb2 ytb2) (p2 xt2 ytb2) (p2 xrb2 yr1) 0
      _radiusTopRight _borderTop _borderRight
    drawRoundedCorner renderer
      (p2 xb2 yr2) (p2 xrb2 ybb2) (p2 xrb2 yr2) (p2 xb2 ybb2) 90
      _radiusBottomRight _borderRight _borderBottom
    drawRoundedCorner renderer
      (p2 xb1 yl2) (p2 xlb2 ybb2) (p2 xb1 ybb2) (p2 xlb2 yl2) 180
      _radiusBottomLeft _borderBottom _borderLeft

topLeftBorderSize :: Border -> Radius -> Double
topLeftBorderSize Border{..} Radius{..}
  | justLeft && justTop && isJust _radiusTopLeft = fromJust _radiusTopLeft
  | otherwise = 0
  where
    justLeft = isJust _borderLeft
    justTop = isJust _borderTop

topRightBorderSize :: Border -> Radius -> Double
topRightBorderSize Border{..} Radius{..}
  | justRight && justTop && isJust _radiusTopRight = fromJust _radiusTopRight
  | otherwise = 0
  where
    justRight = isJust _borderRight
    justTop = isJust _borderTop

bottomLeftBorderSize :: Border -> Radius -> Double
bottomLeftBorderSize Border{..} Radius{..}
  | justLeft && justBottom && justRad = fromJust _radiusBottomLeft
  | otherwise = 0
  where
    justLeft = isJust _borderLeft
    justBottom = isJust _borderBottom
    justRad = isJust _radiusBottomLeft

bottomRightBorderSize :: Border -> Radius -> Double
bottomRightBorderSize Border{..} Radius{..}
  | justRight && justBottom && justRad = fromJust _radiusBottomRight
  | otherwise = 0
  where
    justRight = isJust _borderRight
    justBottom = isJust _borderBottom
    justRad = isJust _radiusBottomRight

drawRoundedCorner
  :: (Monad m)
  => Renderer m
  -> Point
  -> Point
  -> Point
  -> Point
  -> Double
  -> Maybe Double
  -> Maybe BorderSide
  -> Maybe BorderSide
  -> m ()
drawRoundedCorner renderer c1 c2 p1 p2 deg (Just rad) (Just s1) (Just s2) = do
  let width1 = _borderSideWidth s1
      width2 = _borderSideWidth s2
      color1 = _borderSideColor s1
      color2 = _borderSideColor s2

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

strokeBorder
  :: (Monad m) => Renderer m -> Point -> Point -> Maybe BorderSide -> m ()
strokeBorder renderer from to Nothing = pure ()
strokeBorder renderer from to (Just BorderSide{..}) = do
  beginPath renderer
  setStrokeColor renderer _borderSideColor
  setStrokeWidth renderer _borderSideWidth
  moveTo renderer from
  renderLineTo renderer to
  stroke renderer

drawStyledText :: (Monad m) => Renderer m -> Rect -> Style -> Text -> m Rect
drawStyledText renderer viewport style txt = action where
  tsRect = contentRect viewport style
  action = drawText renderer tsRect (_styleText style) txt

drawStyledText_ :: (Monad m) => Renderer m -> Rect -> Style -> Text -> m ()
drawStyledText_ renderer viewport style txt = void action where
  rect = contentRect viewport style
  action = drawStyledText renderer rect style txt

drawText :: (Monad m) => Renderer m -> Rect -> Maybe TextStyle -> Text -> m Rect
drawText renderer viewport Nothing txt =
  drawText renderer viewport (Just mempty) txt
drawText renderer viewport (Just TextStyle{..}) txt = do
    setFillColor renderer tsColor
    renderText renderer viewport tsFont tsFontSize tsAlign txt
  where
    tsColor = fromMaybe defaultColor _textStyleColor
    tsFont = fromMaybe defaultFont _textStyleFont
    tsFontSize = fromMaybe defaultFontSize _textStyleFontSize
    tsAlignH = fromMaybe defaultAlignH _textStyleAlignH
    tsAlignV = fromMaybe defaultAlignV _textStyleAlignV
    tsAlign = Align tsAlignH tsAlignV

calcTextBounds
  :: (Font -> FontSize -> Text -> Size) -> Maybe TextStyle -> Text -> Size
calcTextBounds textBoundsFn Nothing txt
  = calcTextBounds textBoundsFn (Just mempty) txt
calcTextBounds textBoundsFn (Just TextStyle{..}) txt =
  let
    tsFont = fromMaybe defaultFont _textStyleFont
    tsFontSize = fromMaybe defaultFontSize _textStyleFontSize
  in
    textBoundsFn tsFont tsFontSize txt

tsTextColor :: Maybe TextStyle -> Color
tsTextColor Nothing = tsTextColor (Just mempty)
tsTextColor (Just ts) = fromMaybe defaultColor (_textStyleColor ts)

subtractBorder :: Rect -> Maybe Border -> Rect
subtractBorder rect Nothing = rect
subtractBorder (Rect x y w h) (Just (Border l r t b)) = Rect nx ny nw nh where
  nx = x + _borderSideWidth (justDef l)
  ny = y + _borderSideWidth (justDef t)
  nw = w - _borderSideWidth (justDef l) - _borderSideWidth (justDef r)
  nh = h - _borderSideWidth (justDef t) - _borderSideWidth (justDef b)

subtractMargin :: Rect -> Maybe Margin -> Rect
subtractMargin rect Nothing = rect
subtractMargin rect (Just (Margin l r t b)) = subtractFromRect rect l r t b

subtractPadding :: Rect -> Maybe Padding -> Rect
subtractPadding rect Nothing = rect
subtractPadding rect (Just (Padding l r t b)) = subtractFromRect rect l r t b

subtractFromRect
  :: Rect
  -> Maybe Double
  -> Maybe Double
  -> Maybe Double
  -> Maybe Double
  -> Rect
subtractFromRect (Rect x y w h) l r t b = Rect nx ny nw nh where
  nx = x + justDef l
  ny = y + justDef t
  nw = w - justDef l - justDef r
  nh = h - justDef t - justDef b

contentRect :: Rect -> Style -> Rect
contentRect viewport Style{..} = final where
  border = subtractBorder viewport _styleBorder
  margin = subtractMargin border _styleMargin
  padding = subtractPadding margin _stylePadding
  final = padding

justDef :: (Default a) => Maybe a -> a
justDef Nothing = def
justDef (Just val) = val

p2 :: Double -> Double -> Point
p2 x y = Point x y
