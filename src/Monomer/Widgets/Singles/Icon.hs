{-|
Module      : Monomer.Widgets.Singles.Icon
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Icon widget. Used for showing some standard icos without the need of an asset.

Configs:

- width: the maximum width and height of the icon.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Singles.Icon (
  IconType(..),
  icon,
  icon_
) where

import Control.Lens ((^.))
import Control.Applicative ((<|>))
import Data.Default
import Data.Maybe

import qualified Data.Text as T

import Monomer.Graphics.Util

import Monomer.Widgets.Single

import qualified Monomer.Lens as L

-- | Different types of icons that can be displayed.
data IconType
  = IconClose
  | IconPlus
  | IconMinus
  deriving (Eq, Show)

newtype IconCfg = IconCfg {
  _icWidth :: Maybe Double
}

instance Default IconCfg where
  def = IconCfg {
    _icWidth = Nothing
  }

instance Semigroup IconCfg where
  (<>) i1 i2 = IconCfg {
    _icWidth = _icWidth i2 <|> _icWidth i1
  }

instance Monoid IconCfg where
  mempty = def

instance CmbWidth IconCfg where
  width w = def {
    _icWidth = Just w
  }

-- | Creates an icon of the given type.
icon :: IconType -> WidgetNode s e
icon iconType = icon_ iconType def

-- | Creates an icon of the given type. Accepts config.
icon_ :: IconType -> [IconCfg] -> WidgetNode s e
icon_ iconType configs = defaultWidgetNode widgetType widget where
  iconName = T.pack $ show iconType
  widgetType = WidgetType ("i" <> T.tail iconName)
  config = mconcat configs
  widget = makeImage iconType config

makeImage :: IconType -> IconCfg -> Widget s e
makeImage iconType config = widget where
  widget = createSingle () def {
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  getSizeReq wenv node = sizeReq where
    (w, h) = (16, 16)
    factor = 1
    sizeReq = (minSize w factor, minSize h factor)

  render wenv node renderer = do
    drawIcon renderer style iconType iconVp width
    where
      style = currentStyle wenv node
      contentArea = getContentArea node style
      vp = node ^. L.info . L.viewport
      dim = min (vp ^. L.w) (vp ^. L.h)
      width = fromMaybe (dim / 2) (_icWidth config)
      iconVp = centeredSquare contentArea

centeredSquare :: Rect -> Rect
centeredSquare (Rect x y w h) = Rect newX newY dim dim where
  dim = min w h
  newX = x + (w - dim) / 2
  newY = y + (h - dim) / 2

drawIcon :: Renderer -> StyleState -> IconType -> Rect -> Double -> IO ()
drawIcon renderer style iconType viewport lw = case iconType of
  IconClose ->
    drawTimesX renderer viewport lw (Just fgColor)

  IconPlus -> do
    beginPath renderer
    setFillColor renderer fgColor
    renderRect renderer (Rect (cx - hw) y lw h)
    renderRect renderer (Rect x (cy - hw) w lw)
    fill renderer

  IconMinus -> do
    beginPath renderer
    setFillColor renderer fgColor
    renderRect renderer (Rect x (cy - hw) w lw)
    fill renderer
  where
    Rect x y w h = viewport
    fgColor = fromMaybe (rgb 0 0 0) (style ^. L.fgColor)
    hw = lw / 2
    cx = x + w / 2
    cy = y + h / 2
    mx = x + w
    my = y + h
