{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Singles.ColorPicker (
  colorPicker,
  colorPicker_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~), abbreviatedFields, makeLensesWith)
import Data.Default
import Data.Maybe
import Data.Text (Text)

import Monomer.Core
import Monomer.Core.Combinators

import Monomer.Widgets.Composite
import Monomer.Widgets.Containers.Box
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.NumericField
import Monomer.Widgets.Singles.Slider
import Monomer.Widgets.Singles.Spacer

import qualified Monomer.Lens as L

newtype ColorPickerCfg = ColorPickerCfg {
  _cpcShowAlpha :: Maybe Bool
}

instance Default ColorPickerCfg where
  def = ColorPickerCfg {
    _cpcShowAlpha = Nothing
  }

instance Semigroup ColorPickerCfg where
  (<>) a1 a2 = ColorPickerCfg {
    _cpcShowAlpha = _cpcShowAlpha a2 <|> _cpcShowAlpha a1
  }

instance Monoid ColorPickerCfg where
  mempty = def

colorPickerAlpha :: Bool -> ColorPickerCfg
colorPickerAlpha show = def {
  _cpcShowAlpha = Just show
}

newtype ColorPickerState = ColorPickerState {
  _cpsColor :: Color
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''ColorPickerState

data ColorPickerEvt
  = ColorChanged
  | AlphaChanged
  deriving (Eq, Show)

colorPicker
  :: (WidgetModel sp, WidgetEvent ep)
  => WidgetNode sp ep
colorPicker = colorPicker_ def

colorPicker_
  :: (WidgetModel sp, WidgetEvent ep)
  => [ColorPickerCfg]
  -> WidgetNode sp ep
colorPicker_ configs = newNode where
  config = mconcat configs
  state = ColorPickerState (rgb 255 0 0)
  newNode = compositeExt "colorPicker" state (buildUI config) handleEvent

buildUI
  :: ColorPickerCfg
  -> WidgetEnv ColorPickerState ColorPickerEvt
  -> ColorPickerState
  -> WidgetNode ColorPickerState ColorPickerEvt
buildUI config wenv model = mainTree where
  showAlpha = fromMaybe False (_cpcShowAlpha config)
  colorSample = filler `style` [width 32, bgColor (model ^. color)]
  compRow lensCol lbl minV maxV = hstack [
      label lbl `style` [width 48],
      spacer_ [width 2],
      hslider (color . lensCol) minV maxV `style` [paddingV 5],
      spacer_ [width 2],
      numericField_ (color . lensCol) [minValue minV, maxValue maxV]
        `style` [width 40, padding 0, textRight]
    ]
  colorRow lens lbl = compRow lens lbl 0 255
  alphaRow lens lbl = compRow lens lbl 0 1
  mainTree = hstack_ [sizeReqUpdater clearExtra] [
      vstack [
        colorRow L.r "Red",
        spacer_ [height 2],
        colorRow L.g "Green",
        spacer_ [height 2],
        colorRow L.b "Blue",
        spacer_ [height 2] `visible` showAlpha,
        alphaRow L.a "Alpha" `visible` showAlpha
      ],
      spacer_ [width 2],
      box_ [alignTop] colorSample
    ] `style` [padding 0]

handleEvent
  :: WidgetEnv ColorPickerState ColorPickerEvt
  -> WidgetNode ColorPickerState ColorPickerEvt
  -> ColorPickerState
  -> e
  -> [EventResponse ColorPickerState ColorPickerEvt ep]
handleEvent wenv node model evt = []
