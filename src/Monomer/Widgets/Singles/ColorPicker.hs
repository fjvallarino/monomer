{-|
Module      : Monomer.Widgets.Singles.ColorPicker
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Color picker using sliders and numeric fields.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Singles.ColorPicker (
  -- * Configuration
  ColorPickerCfg,
  -- * Constructors
  colorPicker,
  colorPicker_,
  colorPickerV,
  colorPickerV_,
  colorPickerD_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~), ALens', abbreviatedFields, makeLensesWith)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.Default
import Data.Maybe
import Data.Text (Text)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as B
import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Graphics

import Monomer.Widgets.Composite
import Monomer.Widgets.Containers.Box
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Containers.ZStack
import Monomer.Widgets.Singles.Image
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.NumericField
import Monomer.Widgets.Singles.Slider
import Monomer.Widgets.Singles.Spacer

import qualified Monomer.Lens as L

{-|
Configuration options for colorPicker:

- 'showAlpha': whether to allow modifying the alpha channel or not.
- 'onFocus': event to raise when focus is received.
- 'onFocusReq': 'WidgetRequest' to generate when focus is received.
- 'onBlur': event to raise when focus is lost.
- 'onBlurReq': 'WidgetRequest' to generate when focus is lost.
- 'onChange': event to raise when any of the values changes.
- 'onChangeReq': 'WidgetRequest' to generate when any of the values changes.
-}
data ColorPickerCfg s e = ColorPickerCfg {
  _cpcShowAlpha :: Maybe Bool,
  _cpcOnFocusReq :: [Path -> WidgetRequest s e],
  _cpcOnBlurReq :: [Path -> WidgetRequest s e],
  _cpcOnChangeReq :: [Color -> WidgetRequest s e]
}

instance Default (ColorPickerCfg s e) where
  def = ColorPickerCfg {
    _cpcShowAlpha = Nothing,
    _cpcOnFocusReq = [],
    _cpcOnBlurReq = [],
    _cpcOnChangeReq = []
  }

instance Semigroup (ColorPickerCfg s e) where
  (<>) a1 a2 = def {
    _cpcShowAlpha = _cpcShowAlpha a2 <|> _cpcShowAlpha a1,
    _cpcOnFocusReq = _cpcOnFocusReq a1 <> _cpcOnFocusReq a2,
    _cpcOnBlurReq = _cpcOnBlurReq a1 <> _cpcOnBlurReq a2,
    _cpcOnChangeReq = _cpcOnChangeReq a1 <> _cpcOnChangeReq a2
  }

instance Monoid (ColorPickerCfg s e) where
  mempty = def

instance CmbShowAlpha (ColorPickerCfg s e) where
  showAlpha_ show = def {
    _cpcShowAlpha = Just show
  }

instance WidgetEvent e => CmbOnFocus (ColorPickerCfg s e) e Path where
  onFocus fn = def {
    _cpcOnFocusReq = [RaiseEvent . fn]
  }

instance CmbOnFocusReq (ColorPickerCfg s e) s e Path where
  onFocusReq req = def {
    _cpcOnFocusReq = [req]
  }

instance WidgetEvent e => CmbOnBlur (ColorPickerCfg s e) e Path where
  onBlur fn = def {
    _cpcOnBlurReq = [RaiseEvent . fn]
  }

instance CmbOnBlurReq (ColorPickerCfg s e) s e Path where
  onBlurReq req = def {
    _cpcOnBlurReq = [req]
  }

instance WidgetEvent e => CmbOnChange (ColorPickerCfg s e) Color e where
  onChange fn = def {
    _cpcOnChangeReq = [RaiseEvent . fn]
  }

instance CmbOnChangeReq (ColorPickerCfg s e) s e Color where
  onChangeReq req = def {
    _cpcOnChangeReq = [req]
  }

data ColorPickerEvt
  = PickerFocus Path
  | PickerBlur Path
  | ColorChanged Int
  | AlphaChanged Double
  deriving (Eq, Show)

-- | Creates a color picker using the given lens.
colorPicker
  :: (WidgetModel s, WidgetEvent e)
  => ALens' s Color
  -> WidgetNode s e
colorPicker field = colorPicker_ field def

-- | Creates a color picker using the given lens. Accepts config.
colorPicker_
  :: (WidgetModel s, WidgetEvent e)
  => ALens' s Color
  -> [ColorPickerCfg s e]
  -> WidgetNode s e
colorPicker_ field configs = colorPickerD_ wlens configs [] where
  wlens = WidgetLens field

-- | Creates a color picker using the given value and 'onChange' event handler.
colorPickerV
  :: (WidgetModel s, WidgetEvent e)
  => Color
  -> (Color -> e)
  -> WidgetNode s e
colorPickerV value handler = colorPickerV_ value handler def

{-|
Creates a color picker using the given value and 'onChange' event handler.
Accepts config.
-}
colorPickerV_
  :: (WidgetModel s, WidgetEvent e)
  => Color
  -> (Color -> e)
  -> [ColorPickerCfg s e]
  -> WidgetNode s e
colorPickerV_ value handler configs = colorPickerD_ wdata newCfgs [] where
  wdata = WidgetValue value
  newCfgs = onChange handler : configs

-- | Creates a color picker providing a 'WidgetData' instance and config.
colorPickerD_
  :: (WidgetModel s, WidgetEvent e)
  => WidgetData s Color
  -> [ColorPickerCfg s e]
  -> [CompositeCfg Color ColorPickerEvt s e]
  -> WidgetNode s e
colorPickerD_ wdata cfgs cmpCfgs = newNode where
  cfg = mconcat cfgs
  uiBuilder = buildUI cfg
  evtHandler = handleEvent cfg
  newNode = compositeD_ "colorPicker" wdata uiBuilder evtHandler cmpCfgs

buildUI
  :: ColorPickerCfg sp ep
  -> WidgetEnv Color ColorPickerEvt
  -> Color
  -> WidgetNode Color ColorPickerEvt
buildUI config wenv model = mainTree where
  showAlpha = fromMaybe False (_cpcShowAlpha config)
  colorSample = zstack [
      patternImage 2 10 (rgb 255 255 255) (rgb 150 150 150),
      filler `styleBasic` [bgColor model]
    ] `styleBasic` [width 32]

  compRow lensCol evt lbl minV maxV = hstack [
      label lbl,
      spacer_ [width 5],
      hslider_ lensCol minV maxV [onChange evt, onFocus PickerFocus,
        onBlur PickerBlur]
        `styleBasic` [paddingV 5],
      spacer_ [width 5],
      numericField_ lensCol [minValue minV, maxValue maxV, onChange evt,
        onFocus PickerFocus, onBlur PickerBlur]
        `styleBasic` [width 40, padding 0, textRight]
    ]

  colorRow lens lbl = compRow lens ColorChanged lbl 0 255
  alphaRow lens lbl = compRow lens AlphaChanged lbl 0 1

  mainTree = hstack_ [sizeReqUpdater clearExtra] [
      vstack [
        colorRow L.r "R",
        spacer_ [width 2],
        colorRow L.g "G",
        spacer_ [width 2],
        colorRow L.b "B",
        spacer_ [width 2] `nodeVisible` showAlpha,
        alphaRow L.a "A" `nodeVisible` showAlpha
      ],
      spacer_ [width 5],
      box_ [alignTop] colorSample `styleBasic` [flexHeight 50]
    ] `styleBasic` [padding 0]

handleEvent
  :: (WidgetModel sp, WidgetEvent ep)
  => ColorPickerCfg sp ep
  -> WidgetEnv Color ColorPickerEvt
  -> WidgetNode Color ColorPickerEvt
  -> Color
  -> ColorPickerEvt
  -> [EventResponse Color ColorPickerEvt sp ep]
handleEvent cfg wenv node model evt = case evt of
  PickerFocus prev
    | not (isNodeParentOfPath node prev) -> reportFocus prev
  PickerBlur next
    | not (isNodeParentOfPath node next) -> reportBlur next
  ColorChanged _ -> reportChange
  AlphaChanged _ -> reportChange
  _ -> []
  where
    report reqs = RequestParent <$> reqs
    reportFocus prev = report (($ prev) <$> _cpcOnFocusReq cfg)
    reportBlur next = report (($ next) <$> _cpcOnBlurReq cfg)
    reportChange = report (($ model) <$> _cpcOnChangeReq cfg)

patternImage :: WidgetEvent e => Int -> Int -> Color -> Color -> WidgetNode s e
patternImage steps blockW col1 col2 = newImg where
  row1 = encodeRow steps blockW col1 col2
  row2 = encodeRow steps blockW col2 col1
  builder = mconcat (replicate steps (row1 <> row2))

  imgData = BL.toStrict $ toLazyByteString builder
  imgLen = fromIntegral (steps * blockW)
  imgSize = Size imgLen imgLen
  imgConfig = [fitFill, imageRepeatX, imageRepeatY]

  newImg = imageMem_ "colorPickerAlphaBg" imgData imgSize imgConfig

encodeRow :: Int -> Int -> Color -> Color -> Builder
encodeRow steps blockW col1 col2 = builder where
  line = encodeLine steps blockW col1 col2
  builder = mconcat (replicate blockW line)

encodeLine :: Int -> Int -> Color -> Color -> Builder
encodeLine steps blockW col1 col2 = builder where
  p1 = mconcat $ replicate blockW (encodeColor col1)
  p2 = mconcat $ replicate blockW (encodeColor col2)
  builder = mconcat $ replicate (steps `div` 2) (p1 <> p2)

encodeColor :: Color -> Builder
encodeColor (Color r g b a) = mconcat [er, eg, eb, ea] where
  er = B.int8 $ fromIntegral r
  eg = B.int8 $ fromIntegral g
  eb = B.int8 $ fromIntegral b
  ea = B.int8 $ round (255 * a)
