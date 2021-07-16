{-|
Module      : Monomer.Widgets.Util.Text
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions to text related operations in widgets.
-}
{-# LANGUAGE BangPatterns #-}

module Monomer.Widgets.Util.Text (
  getTextMetrics,
  getTextSize,
  getTextSize_,
  getTextRect,
  getTextGlyphs
) where

import Control.Lens ((&), (^.), (+~))
import Data.Default
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text (Text)

import Monomer.Core
import Monomer.Graphics

import qualified Monomer.Core.Lens as L

-- | Returns the text metrics of the active style.
getTextMetrics :: WidgetEnv s e -> StyleState -> TextMetrics
getTextMetrics wenv style = textMetrics where
  fontMgr = wenv ^. L.fontManager
  !textMetrics = computeTextMetrics fontMgr font fontSize
  font = styleFont style
  fontSize = styleFontSize style

-- | Returns the size of the text using the active style and default options.
getTextSize :: WidgetEnv s e -> StyleState -> Text -> Size
getTextSize wenv style !text = size where
  fontMgr = wenv ^. L.fontManager
  size = calcTextSize_ fontMgr style SingleLine KeepSpaces Nothing Nothing text

-- | Returns the size of the text using the active style.
getTextSize_
  :: WidgetEnv s e  -- ^ The widget environment.
  -> StyleState     -- ^ The active style.
  -> TextMode       -- ^ Whether to use single or multi line.
  -> TextTrim       -- ^ Whether to trim spacers or keep them.
  -> Maybe Double   -- ^ Maximum width (required for multi line).
  -> Maybe Int      -- ^ Max lines.
  -> Text           -- ^ Text to measure.
  -> Size           -- ^ The calculated size.
getTextSize_ wenv style mode trim mwidth mlines text = newSize where
  fontMgr = wenv ^. L.fontManager
  newSize = calcTextSize_ fontMgr style mode trim mwidth mlines text

-- | Returns the rectangle used by a single line of text.
getTextRect
  :: WidgetEnv s e  -- ^ The widget environment.
  -> StyleState     -- ^ The active style.
  -> Rect           -- ^ The bounding rect.
  -> AlignTH        -- ^ The horizontal alignment.
  -> AlignTV        -- ^ The vertical alignment.
  -> Text           -- ^ The text to measure.
  -> Rect           -- ^ The used rect. May be larger than the bounding rect.
getTextRect wenv style !rect !alignH !alignV !text = textRect where
  fontMgr = wenv ^. L.fontManager
  font = styleFont style
  fSize = styleFontSize style
  fSpcH = styleFontSpaceH style
  !textRect = calcTextRect fontMgr rect font fSize fSpcH alignH alignV text

-- | Returns the glyphs of a single line of text.
getTextGlyphs :: WidgetEnv s e -> StyleState -> Text -> Seq GlyphPos
getTextGlyphs wenv style !text = glyphs where
  fontMgr = wenv ^. L.fontManager
  font = styleFont style
  fSize = styleFontSize style
  fSpcH = styleFontSpaceH style
  !glyphs = computeGlyphsPos fontMgr font fSize fSpcH text
