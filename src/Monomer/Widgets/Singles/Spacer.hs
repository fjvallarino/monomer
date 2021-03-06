{-# LANGUAGE FlexibleContexts #-}

module Monomer.Widgets.Singles.Spacer (
  spacer,
  spacer_,
  filler,
  filler_
) where

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Data.Default
import Data.Maybe
import Data.Tuple

import Monomer.Widgets.Single

import qualified Monomer.Lens as L

data SpacerCfg = SpacerCfg {
  _spcWidth :: Maybe Double,
  _spcHeight :: Maybe Double,
  _spcFactor :: Maybe Double
}

instance Default SpacerCfg where
  def = SpacerCfg {
    _spcWidth = Nothing,
    _spcHeight = Nothing,
    _spcFactor = Nothing
  }

instance Semigroup SpacerCfg where
  (<>) s1 s2 = SpacerCfg {
    _spcWidth = _spcWidth s2 <|> _spcWidth s1,
    _spcHeight = _spcHeight s2 <|> _spcHeight s1,
    _spcFactor = _spcFactor s2 <|> _spcFactor s1
  }

instance Monoid SpacerCfg where
  mempty = def

instance CmbWidth SpacerCfg where
  width w = def {
    _spcWidth = Just w
  }

instance CmbHeight SpacerCfg where
  height h = def {
    _spcHeight = Just h
  }

instance CmbResizeFactor SpacerCfg where
  resizeFactor f = def {
    _spcFactor = Just f
  }

spacer :: WidgetNode s e
spacer = spacer_ def

spacer_ :: [SpacerCfg] -> WidgetNode s e
spacer_ configs = defaultWidgetNode "spacer" widget where
  config = mconcat (resizeFactor 0 : configs)
  widget = makeSpacer config

filler :: WidgetNode s e
filler = filler_ def

filler_ :: [SpacerCfg] -> WidgetNode s e
filler_ configs = defaultWidgetNode "filler" widget where
  config = mconcat configs
  widget = makeSpacer config

makeSpacer :: SpacerCfg -> Widget s e
makeSpacer config = widget where
  widget = createSingle () def {
    singleGetSizeReq = getSizeReq
  }

  getSizeReq wenv currState node = sizeReq where
    direction = wenv ^. L.layoutDirection
    width = fromMaybe 5 (_spcWidth config)
    height = fromMaybe 5 (_spcHeight config)
    factor = fromMaybe 0.5 (_spcFactor config)
    isFixed = factor < 0.01
    fixedW = fixedSize width
    fixedH = fixedSize height
    flexW = flexSize width factor
    flexH = flexSize height factor
    expandW = expandSize width factor
    expandH = expandSize height factor
    sizeReq
      | isFixed && direction == LayoutNone = (fixedW, fixedH)
      | isFixed && direction == LayoutHorizontal = (fixedW, flexSize height 0.5)
      | isFixed = (flexSize width 0.5, fixedH)
      | direction == LayoutNone = (expandW, expandH)
      | direction == LayoutHorizontal = (expandW, flexH)
      | otherwise = (flexW, expandH)
