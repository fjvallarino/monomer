{-# LANGUAGE BangPatterns #-}

module Monomer.Widgets.Util.Text (
  getTextMetrics,
  getTextSize,
  getTextSize_,
  getTextRect,
  getTextGlyphs
) where

import Control.Lens ((&), (^.), (+~))
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text (Text)

import Monomer.Core
import Monomer.Graphics

import Monomer.Lens as L

getTextMetrics :: WidgetEnv s e -> StyleState -> TextMetrics
getTextMetrics wenv style = textMetrics where
  renderer = _weRenderer wenv
  !textMetrics = computeTextMetrics renderer font fontSize
  font = styleFont style
  fontSize = styleFontSize style

getTextSize :: WidgetEnv s e -> StyleState -> Text -> Size
getTextSize wenv style !text = newSize where
  renderer = wenv ^. L.renderer
  newSize = calcTextSize_ renderer style SingleLine KeepSpaces Nothing text

getTextSize_
  :: WidgetEnv s e
  -> StyleState
  -> TextMode
  -> TextTrim
  -> Maybe Double
  -> Text
  -> Size
getTextSize_ wenv style mode trim mwidth text = newSize where
  renderer = wenv ^. L.renderer
  newSize = calcTextSize_ renderer style mode trim mwidth text

getTextRect
  :: WidgetEnv s e -> StyleState -> Rect -> AlignTH -> AlignTV -> Text -> Rect
getTextRect wenv style !rect !alignH !alignV !text = textRect where
  renderer = _weRenderer wenv
  font = styleFont style
  fontSize = styleFontSize style
  !textRect = computeTextRect renderer rect font fontSize alignH alignV text

getTextGlyphs :: WidgetEnv s e -> StyleState -> Text -> Seq GlyphPos
getTextGlyphs wenv style !text = glyphs where
  font = styleFont style
  fontSize = styleFontSize style
  !glyphs = computeGlyphsPos (_weRenderer wenv) font fontSize text
