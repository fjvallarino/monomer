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

drawRect :: (Monad m) => Renderer m -> Rect -> Maybe Color -> Maybe Radius -> m ()
drawRect _ _ Nothing _ = pure ()
drawRect renderer viewport (Just color) Nothing = do
  beginPath renderer
  fillColor renderer color
  rect renderer viewport
  fill renderer
drawRect renderer viewport (Just color) (Just radius) = do
  beginPath renderer
  fillColor renderer color
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
    x3 = x + justDef _radiusBottomLeft
    x4 = x + w - justDef _radiusBottomRight
    y1 = y + justDef _radiusTopLeft
    y2 = y + h - justDef _radiusBottomLeft
    y3 = y + justDef _radiusTopRight
    y4 = y + h - justDef _radiusBottomRight
  in do
    arc renderer (Point x1 y1) (justDef _radiusTopLeft) 180 270 CW
    lineTo renderer (Point x2 yt)
    arc renderer (Point x2 y1) (justDef _radiusTopRight) 270 0 CW
    lineTo renderer (Point xr y2)
    arc renderer (Point x2 y2) (justDef _radiusBottomRight) 0 90 CW
    lineTo renderer (Point x1 yb)
    arc renderer (Point x1 y2) (justDef _radiusBottomLeft) 90 180 CW
    lineTo renderer (Point xl y1)

drawStyledBorder :: (Monad m) => Renderer m -> Rect -> Border -> Maybe Radius -> m ()
drawStyledBorder renderer rect border Nothing = drawBorder renderer rect border
drawStyledBorder renderer rect border (Just radius) = drawRoundedBorder renderer rect border radius

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
  in do
    drawCorner renderer (p2 xl yt) (p2 xlb2 ytb2) (p2 xlb2 yt) _borderTop _borderLeft
    drawCorner renderer (p2 xrb2 yt) (p2 xrb2 ytb2) (p2 xr yt) _borderTop _borderRight
    drawCorner renderer (p2 xrb2 ybb2) (p2 xr yb) (p2 xr ybb2) _borderRight _borderBottom
    drawCorner renderer (p2 xlb2 yb) (p2 xlb2 ybb2) (p2 xl yb) _borderBottom _borderLeft

drawCorner :: (Monad m) => Renderer m -> Point -> Point -> Point -> Maybe BorderSide -> Maybe BorderSide -> m ()
drawCorner renderer p1 p2 p3 (Just bs1) (Just bs2) = do
  beginPath renderer
  fillColor renderer (_borderSideColor bs1)
  moveTo renderer p1
  lineTo renderer p2
  lineTo renderer p3
  lineTo renderer p1
  fill renderer
drawCorner renderer p1 p2 p3 _ _ = return ()

drawRoundedBorder :: (Monad m) => Renderer m -> Rect -> Border -> Radius -> m ()
drawRoundedBorder renderer (Rect x y w h) Border{..} Radius{..} =
  let
    xl = x
    xr = x + w
    yt = y
    yb = y + h
    x1 = x + justDef _radiusTopLeft
    x2 = x + w - justDef _radiusTopRight
    x3 = x + justDef _radiusBottomLeft
    x4 = x + w - justDef _radiusBottomRight
    y1 = y + justDef _radiusTopLeft
    y2 = y + h - justDef _radiusBottomLeft
    y3 = y + justDef _radiusTopRight
    y4 = y + h - justDef _radiusBottomRight
  in do
    beginPath renderer
    moveTo renderer (Point xl y1)
    arc renderer (Point x1 y1) (justDef _radiusTopLeft) 180 270 CW
    lineTo renderer (Point x2 yt)
    arc renderer (Point x2 y1) (justDef _radiusTopRight) 270 0 CW
    lineTo renderer (Point xr y2)
    arc renderer (Point x2 y2) (justDef _radiusBottomRight) 0 90 CW
    lineTo renderer (Point x1 yb)
    arc renderer (Point x1 y2) (justDef _radiusBottomLeft) 90 180 CW
    lineTo renderer (Point xl y1)
    stroke renderer

strokeBorder :: (Monad m) => Renderer m -> Point -> Point -> Maybe BorderSide -> m ()
strokeBorder renderer from to Nothing = pure ()
strokeBorder renderer from to (Just BorderSide{..}) = do
  beginPath renderer
  strokeColor renderer _borderSideColor
  strokeWidth renderer _borderSideWidth
  moveTo renderer from
  lineTo renderer to
  stroke renderer

drawStyledText :: (Monad m) => Renderer m -> Rect -> Style -> Text -> m Rect
drawStyledText renderer viewport style txt = drawText renderer tsRect (_styleText style) txt where
  tsRect = contentRect viewport style

drawStyledText_ :: (Monad m) => Renderer m -> Rect -> Style -> Text -> m ()
drawStyledText_ renderer viewport style txt = void $ drawStyledText renderer rect style txt where
  rect = contentRect viewport style

drawText :: (Monad m) => Renderer m -> Rect -> Maybe TextStyle -> Text -> m Rect
drawText renderer viewport Nothing txt = drawText renderer viewport (Just mempty) txt
drawText renderer viewport (Just TextStyle{..}) txt = do
  let tsColor = fromMaybe defaultColor _textStyleColor
      tsFontSize = fromMaybe defaultFontSize _textStyleFontSize
      tsAlignH = fromMaybe defaultAlignH _textStyleAlignH
      tsAlignV = fromMaybe defaultAlignV _textStyleAlignV
      tsAlign = Align tsAlignH tsAlignV

  fillColor renderer tsColor
  text renderer viewport defaultFont tsFontSize tsAlign txt

calcTextBounds :: (Monad m) => Renderer m -> Maybe TextStyle -> Text -> Size
calcTextBounds renderer Nothing txt = calcTextBounds renderer (Just mempty) txt
calcTextBounds renderer (Just TextStyle{..}) txt =
  let
    tsFontSize = fromMaybe defaultFontSize _textStyleFontSize
  in
    textBounds renderer defaultFont tsFontSize txt

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

subtractFromRect :: Rect -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> Rect
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
