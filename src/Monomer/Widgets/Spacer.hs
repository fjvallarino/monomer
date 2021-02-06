module Monomer.Widgets.Spacer (
  spacer,
  spacer_,
  filler,
  filler_
) where

import Control.Applicative ((<|>))
import Data.Default
import Data.Maybe

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

makeSpacer :: SpacerCfg  -> Widget s e
makeSpacer config = widget where
  widget = createSingle () def {
    singleGetSizeReq = getSizeReq
  }

  getSizeReq wenv currState node = sizeReq where
    width = fromMaybe 5 (_spcWidth config)
    height = fromMaybe 5 (_spcHeight config)
    factor = fromMaybe 0.5 (_spcFactor config)
    sizeReq
      | factor >= 0.01 = (FlexSize width factor, FlexSize height factor)
      | otherwise = (FixedSize width, FixedSize height)
