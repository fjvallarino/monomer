module Monomer.Widgets.Spacer (
  hspacer,
  hspacer_,
  vspacer,
  vspacer_,
  hfiller,
  hfiller_,
  vfiller,
  vfiller_
) where

import Control.Applicative ((<|>))
import Data.Default
import Data.Maybe
import Data.Tuple

import Monomer.Widgets.Single

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

hspacer :: WidgetNode s e
hspacer = spacer True

hspacer_ :: [SpacerCfg] -> WidgetNode s e
hspacer_ configs = spacer_ True configs

vspacer :: WidgetNode s e
vspacer = spacer False

vspacer_ :: [SpacerCfg] -> WidgetNode s e
vspacer_ configs = spacer_ False configs

spacer :: Bool -> WidgetNode s e
spacer isHorizontal = spacer_ isHorizontal def

spacer_ :: Bool -> [SpacerCfg] -> WidgetNode s e
spacer_ isHorizontal configs = defaultWidgetNode "spacer" widget where
  config = mconcat (resizeFactor 0 : configs)
  widget = makeSpacer isHorizontal config

hfiller :: WidgetNode s e
hfiller = filler True

hfiller_ :: [SpacerCfg] -> WidgetNode s e
hfiller_ configs = filler_ True configs

vfiller :: WidgetNode s e
vfiller = filler False

vfiller_ :: [SpacerCfg] -> WidgetNode s e
vfiller_ configs = filler_ False configs

filler :: Bool -> WidgetNode s e
filler isHorizontal = filler_ isHorizontal def

filler_ :: Bool -> [SpacerCfg] -> WidgetNode s e
filler_ isHorizontal configs = defaultWidgetNode "filler" widget where
  config = mconcat configs
  widget = makeSpacer isHorizontal config

makeSpacer :: Bool -> SpacerCfg -> Widget s e
makeSpacer isHorizontal config = widget where
  widget = createSingle () def {
    singleGetSizeReq = getSizeReq
  }

  getSizeReq wenv currState node = sizeReq where
    width = fromMaybe 5 (_spcWidth config)
    height = fromMaybe 5 (_spcHeight config)
    factor = fromMaybe 0.5 (_spcFactor config)
    fn = if isHorizontal then id else swap
    sizeReq
      | factor >= 0.01 = fn (expandSize width factor, flexSize height factor)
      | otherwise = fn (fixedSize width, flexSize height factor)
