{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Icon (
  IconType(..),
  icon,
  icon_
) where

import Control.Lens ((^.))
import Control.Applicative ((<|>))
import Data.Default
import Data.Maybe

import Monomer.Widgets.Single

import qualified Monomer.Lens as L

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

icon :: IconType -> WidgetNode s e
icon iconType = icon_ iconType def

icon_ :: IconType -> [IconCfg] -> WidgetNode s e
icon_ iconType configs = defaultWidgetNode widgetType widget where
  widgetType = WidgetType ('i' : tail (show iconType))
  config = mconcat configs
  widget = makeImage iconType config

makeImage :: IconType -> IconCfg -> Widget s e
makeImage iconType config = widget where
  widget = createSingle () def {
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  getSizeReq wenv currState node = sizeReq where
    (w, h) = (16, 16)
    factor = 1
    sizeReq = (FlexSize w factor, FlexSize h factor)

  render renderer wenv node = do
    drawIcon renderer style iconType iconVp width
    where
      style = activeStyle wenv node
      contentArea = getContentArea style node
      width = fromMaybe 8 (_icWidth config)
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
    fgColor = fromMaybe black (style ^. L.fgColor)
    hw = lw / 2
    cx = x + w / 2
    cy = y + h / 2
    mx = x + w
    my = y + h
