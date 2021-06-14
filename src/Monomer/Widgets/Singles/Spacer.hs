{-|
Module      : Monomer.Widgets.Singles.Spacer
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Spacer is used for adding a fixed space between two widgets.
Filler is used for taking all the unused space between two widgets. Useful for
alignment purposes.

Both adapt to the current layout direction, if any.

Configs:

- width: the max width for spacer, the reference for filler.
- resizeFactor: flexibility to have more or less spaced assigned.
-}
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

import qualified Monomer.Core.Lens as L

data SpacerCfg = SpacerCfg {
  _spcWidth :: Maybe Double,
  _spcFactor :: Maybe Double
}

instance Default SpacerCfg where
  def = SpacerCfg {
    _spcWidth = Nothing,
    _spcFactor = Nothing
  }

instance Semigroup SpacerCfg where
  (<>) s1 s2 = SpacerCfg {
    _spcWidth = _spcWidth s2 <|> _spcWidth s1,
    _spcFactor = _spcFactor s2 <|> _spcFactor s1
  }

instance Monoid SpacerCfg where
  mempty = def

instance CmbWidth SpacerCfg where
  width w = def {
    _spcWidth = Just w
  }

instance CmbResizeFactor SpacerCfg where
  resizeFactor f = def {
    _spcFactor = Just f
  }

-- | Creates a spacer widget.
spacer :: WidgetNode s e
spacer = spacer_ def

-- | Creates a spacer widget. Accepts config.
spacer_ :: [SpacerCfg] -> WidgetNode s e
spacer_ configs = defaultWidgetNode "spacer" widget where
  config = mconcat (resizeFactor 0 : configs)
  widget = makeSpacer config

-- | Creates a filler widget.
filler :: WidgetNode s e
filler = filler_ def

-- | Creates a filler widget. Accepts config.
filler_ :: [SpacerCfg] -> WidgetNode s e
filler_ configs = defaultWidgetNode "filler" widget where
  config = mconcat configs
  widget = makeSpacer config

makeSpacer :: SpacerCfg -> Widget s e
makeSpacer config = widget where
  widget = createSingle () def {
    singleGetSizeReq = getSizeReq
  }

  getSizeReq wenv node = sizeReq where
    direction = wenv ^. L.layoutDirection
    width = fromMaybe 5 (_spcWidth config)
    factor = fromMaybe 0.5 (_spcFactor config)
    isFixed = factor < 0.01
    flexSide = flexSize 5 0.5
    fixedW = fixedSize width
    flexW = flexSize width factor
    expandW = expandSize width factor
    sizeReq
      | isFixed && direction == LayoutNone = (fixedW, fixedW)
      | isFixed && direction == LayoutHorizontal = (fixedW, flexSide)
      | isFixed = (flexSide, fixedW)
      | direction == LayoutNone = (expandW, expandW)
      | direction == LayoutHorizontal = (expandW, flexW)
      | otherwise = (flexW, expandW)
