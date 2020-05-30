{-# LANGUAGE RecordWildCards #-}

module Monomer.Graphics.Drawing where

import qualified Data.Text as T

import Control.Monad (when, void)
import Data.Default
import Data.Maybe

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Graphics.Renderer
import Monomer.Graphics.Types
import Monomer.Graphics.Util

justDef :: (Default a) => Maybe a -> a
justDef Nothing = def
justDef (Just val) = val

drawBgRect :: (Monad m) => Renderer m -> Rect -> Style -> m ()
drawBgRect renderer rect Style{..} = do
  drawRect renderer rect _bgColor _bgRadius
  when (isJust _border) $ do
    drawRoundedBorder renderer rect (fromJust _border)

drawRect :: (Monad m) => Renderer m -> Rect -> Maybe Color -> Maybe Radius -> m ()
drawRect _ _ Nothing _ = pure ()
drawRect renderer rt (Just color) Nothing = do
  beginPath renderer
  fillColor renderer color
  rect renderer rt
  fill renderer
drawRect renderer rt (Just color) (Just radius) = do
  beginPath renderer
  fillColor renderer color
  drawRoundedRect renderer rt radius
  fill renderer

drawRoundedRect :: (Monad m) => Renderer m -> Rect -> Radius -> m ()
drawRoundedRect renderer (Rect x y w h) Radius{..} =
  let
    xl = x
    xr = x + w
    yt = y
    yb = y + h
    x1 = x + (justDef _rTopLeft)
    x2 = x + w - (justDef _rTopRight)
    x3 = x + (justDef _rBottomLeft)
    x4 = x + w - (justDef _rBottomRight)
    y1 = y + (justDef _rTopLeft)
    y2 = y + h - (justDef _rBottomLeft)
    y3 = y + (justDef _rTopRight)
    y4 = y + h - (justDef _rBottomRight)
  in do
    arc renderer (Point x1 y1) (justDef _rTopLeft) 180 270
    lineTo renderer (Point x2 yt) --
    arc renderer (Point x2 y1) (justDef _rTopRight) 270 0
    lineTo renderer (Point xr y2) --
    arc renderer (Point x2 y2) (justDef _rBottomRight) 0 90
    lineTo renderer (Point x1 yb) --
    arc renderer (Point x1 y2) (justDef _rBottomLeft) 90 180
    lineTo renderer (Point xl y1) --

drawRoundedBorder :: (Monad m) => Renderer m -> Rect -> Border -> m ()
drawRoundedBorder renderer (Rect x y w h) Border{..} =
  let
    Radius {..} = justDef _bRadius
    _minRadius = 0.5
    -- Border width
    btw = _bsWidth $ justDef _bTop
    bbw = _bsWidth $ justDef _bBottom
    blw = _bsWidth $ justDef _bLeft
    brw = _bsWidth $ justDef _bRight
    -- Radius
    rtl = justDef _rTopLeft
    rtr = justDef _rTopRight
    rbl = justDef _rBottomLeft
    rbr = justDef _rBottomRight
    -- Main points
    -- Top
    xtl1 = x +     (if rtl > _minRadius then rtl else 0)
    xtl2 = x +     (if rtl > _minRadius then rtl else 0) + blw
    xtr1 = x + w - (if rtr > _minRadius then rtr else 0)
    xtr2 = x + w - (if rtr > _minRadius then rtr else 0) - brw
    yt1  = y
    yt2  = y + btw
    -- Bottom
    xbl1 = x +     (if rbl > _minRadius then rbl else 0)
    xbl2 = x +     (if rbl > _minRadius then rbl else 0) + blw
    xbr1 = x + w - (if rbr > _minRadius then rbr else 0)
    xbr2 = x + w - (if rbr > _minRadius then rbr else 0) - brw
    yb1  = y + h
    yb2  = y + h - bbw
    -- Left
    xl1  = x
    xl2  = x + blw
    ytl1 = y +     (if rtl > _minRadius then rtl else 0)
    ytl2 = y +     (if rtl > _minRadius then rtl else 0) + btw
    ybl1 = y + h - (if rbl > _minRadius then rtl else 0)
    ybl2 = y + h - (if rbl > _minRadius then rtl else 0) - bbw
    -- Right
    xr1  = x + w
    xr2  = x + w - brw
    ytr1 = y +     (if rtr > _minRadius then rtr else 0)
    ytr2 = y +     (if rtr > _minRadius then rtr else 0) + btw
    ybr1 = y + h - (if rbr > _minRadius then rtr else 0)
    ybr2 = y + h - (if rbr > _minRadius then rtr else 0) - bbw
    drawTrapezoid borderSide p1 p2 p3 p4 =
      when (_bsWidth (justDef borderSide) > 0.5) $ do
        beginPath renderer
        fillColor renderer (_bsColor (fromJust borderSide))
        moveTo renderer p1
        lineTo renderer p2
        lineTo renderer p3
        lineTo renderer p4
        lineTo renderer p1
        fill renderer
    drawRadius s1 s2 p1 p2 p3 p4 cp1 cp2 = do
      beginPath renderer
      moveTo renderer p1
      quadTo renderer cp1 p2
      lineTo renderer p3
      quadTo renderer cp2 p4
      lineTo renderer p1

      if isJust s1 && isJust s2 && fromJust s1 /= fromJust s2 then
        fillLinearGradient renderer (midPoint p1 p4) (midPoint p2 p3) (_bsColor (fromJust s1)) (_bsColor (fromJust s2))
      else if (isJust s1) then
        fillColor renderer (_bsColor (fromJust s1))
      else 
        fillColor renderer (_bsColor (fromJust s2))
      
      fill renderer
  in do
    -- The 0.5 +/- are used to avoid breaks
    drawTrapezoid _bTop    (Point (xtl1 - 0.5) yt1) (Point (xtr1 + 0.5) yt1) (Point (xtr2 + 0.5) yt2) (Point (xtl2 - 0.5) yt2)
    drawTrapezoid _bBottom (Point (xbl1 - 0.5) yb1) (Point (xbr1 + 0.5) yb1) (Point (xbr2 + 0.5) yb2) (Point (xbl2 - 0.5) yb2)
    drawTrapezoid _bLeft   (Point xl1 (ytl1 - 0.5)) (Point xl1 (ybl1 + 0.5)) (Point xl2 (ybl2 + 0.5)) (Point xl2 (ytl2 - 0.5))
    drawTrapezoid _bRight  (Point xr1 (ytr1 - 0.5)) (Point xr1 (ybr1 + 0.5)) (Point xr2 (ybr2 + 0.5)) (Point xr2 (ytr2 - 0.5))

    when (rtl > 0.5) $
      drawRadius _bLeft   _bTop    (Point xl1 ytl1) (Point xtl1 yt1) (Point xtl2 yt2) (Point xl2 ytl2) (Point xl1 yt1) (Point xl2 yt2)
    when (rtr > 0.5) $
      drawRadius _bTop    _bRight  (Point xtr1 yt1) (Point xr1 ytr1) (Point xr2 ytr2) (Point xtr2 yt2) (Point xr1 yt1) (Point xr2 yt2)
    when (rbr > 0.5) $
      drawRadius _bRight  _bBottom (Point xr1 ybr1) (Point xbr1 yb1) (Point xbr2 yb2) (Point xr2 ybr2) (Point xr1 yb1) (Point xr2 yb2)
    when (rbl > 0.5) $
      drawRadius _bBottom _bLeft   (Point xbl1 yb1) (Point xl1 ybl1) (Point xl2 ybl2) (Point xbl2 yb2) (Point xl1 yb1) (Point xl2 yb2)

tsTextColor :: Maybe TextStyle -> Color
tsTextColor Nothing = tsTextColor (Just mempty)
tsTextColor (Just ts) = fromMaybe defaultColor (_tsColor ts)

drawText :: (Monad m) => Renderer m -> Rect -> Maybe TextStyle -> T.Text -> m Rect
drawText renderer viewport Nothing txt = drawText renderer viewport (Just mempty) txt
drawText renderer viewport (Just TextStyle{..}) txt = do
  let tsColor = fromMaybe defaultColor _tsColor
      tsFontSize = fromMaybe defaultFontSize _tsFontSize
      tsAlignH = fromMaybe defaultAlignH _tsAlignH
      tsAlignV = fromMaybe defaultAlignV _tsAlignV
      tsAlign = Align tsAlignH tsAlignV

  fillColor renderer tsColor
  text renderer viewport defaultFont tsFontSize tsAlign txt

drawText_ :: (Monad m) => Renderer m -> Rect -> Maybe TextStyle -> T.Text -> m ()
drawText_ renderer viewport style txt = do
  void $ drawText renderer viewport style txt

calcTextBounds :: (Monad m) => Renderer m -> Maybe TextStyle -> T.Text -> Size
calcTextBounds renderer Nothing txt = calcTextBounds renderer (Just mempty) txt
calcTextBounds renderer (Just TextStyle{..}) txt =
  let
    tsFontSize = fromMaybe defaultFontSize _tsFontSize
  in
    textBounds renderer defaultFont tsFontSize txt

subtractBorder :: Rect -> Border -> Rect
subtractBorder (Rect x y w h) (Border l r t b _) = Rect nx ny nw nh where
  nx = x + (_bsWidth (justDef l))
  ny = y + (_bsWidth (justDef t))
  nw = w - (_bsWidth (justDef l)) - (_bsWidth (justDef r))
  nh = h - (_bsWidth (justDef t)) - (_bsWidth (justDef b))

subtractPadding :: Rect -> Padding -> Rect
subtractPadding (Rect x y w h) (Padding l r t b) = Rect nx ny nw nh where
  nx = x + (justDef l)
  ny = y + (justDef t)
  nw = w - (justDef l) - (justDef r)
  nh = h - (justDef t) - (justDef b)
