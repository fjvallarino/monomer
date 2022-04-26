{-|
Module      : Monomer.Widgets.Singles.SeparatorLine
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

SeparatorLine is used for adding a separator line between two widgets. It adapts
to the active layout direction, creating a vertical line on a horizontal layout
and viceversa.

@
hstack [
  label "Left half",
  separatorLine,
  label "Right half"
]
@

The separator line has the provided width in the direction orthogonal to the
parent layout, and takes all the available space in the other axis. In case of
wanting a shorter line, padding should be used.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module Monomer.Widgets.Singles.SeparatorLine (
  -- * Configuration
  SeparatorLineCfg,
  -- * Constructors
  separatorLine,
  separatorLine_
) where

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Data.Default
import Data.Maybe
import Data.Tuple

import Monomer.Widgets.Single

import qualified Monomer.Core.Lens as L

{-|
Configuration options for separatorLine:

- 'width': the max width of the line.
- 'resizeFactor': flexibility to have more or less spaced assigned.
-}
data SeparatorLineCfg = SeparatorLineCfg {
  _slcWidth :: Maybe Double,
  _slcFactor :: Maybe Double
}

instance Default SeparatorLineCfg where
  def = SeparatorLineCfg {
    _slcWidth = Nothing,
    _slcFactor = Nothing
  }

instance Semigroup SeparatorLineCfg where
  (<>) s1 s2 = SeparatorLineCfg {
    _slcWidth = _slcWidth s2 <|> _slcWidth s1,
    _slcFactor = _slcFactor s2 <|> _slcFactor s1
  }

instance Monoid SeparatorLineCfg where
  mempty = def

instance CmbWidth SeparatorLineCfg where
  width w = def {
    _slcWidth = Just w
  }

instance CmbResizeFactor SeparatorLineCfg where
  resizeFactor f = def {
    _slcFactor = Just f
  }

-- | Creates a separatorLine widget.
separatorLine :: WidgetNode s e
separatorLine = separatorLine_ def

-- | Creates a separatorLine widget. Accepts config.
separatorLine_ :: [SeparatorLineCfg] -> WidgetNode s e
separatorLine_ configs = defaultWidgetNode "separatorLine" widget where
  config = mconcat (resizeFactor 0 : configs)
  widget = makeSeparatorLine config

makeSeparatorLine :: SeparatorLineCfg -> Widget s e
makeSeparatorLine !config = widget where
  widget = createSingle () def {
    singleGetBaseStyle = getBaseStyle,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  getBaseStyle wenv node = Just style where
    style = collectTheme wenv L.separatorLineStyle

  getSizeReq wenv node = sizeReq where
    theme = currentTheme wenv node
    direction = wenv ^. L.layoutDirection
    width = fromMaybe (theme ^. L.separatorLineWidth) (_slcWidth config)
    factor = fromMaybe 0 (_slcFactor config)

    isFixed = factor < 0.01
    flexSide = flexSize 10 0.5
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

  render wenv node renderer = do
    beginPath renderer
    setFillColor renderer fgColor
    renderRect renderer lineRect
    fill renderer
    where
      theme = currentTheme wenv node
      style = currentStyle wenv node
      direction = wenv ^. L.layoutDirection
      fgColor = styleFgColor style
      width = fromMaybe (theme ^. L.separatorLineWidth) (_slcWidth config)

      Rect cx cy cw ch = getContentArea node style
      lineW = cx + (cw - width) / 2
      lineH = cy + (ch - width) / 2
      lineRect
        | direction == LayoutNone = Rect cx cy cw ch
        | direction == LayoutHorizontal = Rect lineW cy width ch
        | otherwise = Rect cx lineH cw width
