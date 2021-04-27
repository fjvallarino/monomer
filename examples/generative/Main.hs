{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Data.Default
import Data.Maybe
import Data.Text (Text)
import TextShow

import Monomer
import Monomer.Widgets.Single

import GenerativeTypes
import Widgets.BoxesPalette
import Widgets.CirclesGrid

import qualified Monomer.Lens as L

buildUI
  :: WidgetEnv GenerativeModel GenerativeEvt
  -> GenerativeModel
  -> WidgetNode GenerativeModel GenerativeEvt
buildUI wenv model = widgetTree where
  seedDropdown lens = textDropdown_ lens seedList seedDesc []
  widgetCircleCfg = vstack [
      label "Width",
      dial_ (circlesCfg . itemWidth) 10 50 [dragRate 0.5],
      label "Random seed",
      seedDropdown (circlesCfg . seed)
    ]
  widgetBoxCfg = vstack [
      label "Width",
      dial_ (boxesCfg . itemWidth) 10 50 [dragRate 0.5],
      label "Palette type",
      textDropdown (boxesCfg . paletteType) [1..4],
      label "Palette size",
      dial_ (boxesCfg . paletteSize) 1 50 [dragRate 0.5],
      label "Random seed",
      seedDropdown (boxesCfg . seed)
    ]
  widgetTree = vstack [
      hstack [
        label "Type: ",
        textDropdown_ activeGen genTypes genTypeDesc [] `key` "activeType",
        spacer,
        hstack [
          label "Show config: ",
          checkbox showCfg
        ] `style` [width 150]
      ] `style` [padding 3],
      zstack [
        hstack [
          circlesGrid (model ^. circlesCfg),
          widgetCircleCfg `visible` model ^. showCfg `style` [paddingH 3, width 150]
        ] `visible` (model ^. activeGen == CirclesGrid),
        hstack [
          boxesPalette (model ^. boxesCfg),
          widgetBoxCfg `visible` model ^. showCfg `style` [paddingH 3, width 150]
        ] `visible` (model ^. activeGen == BoxesPalette)
      ]
    ]

handleEvent
  :: WidgetEnv GenerativeModel GenerativeEvt
  -> WidgetNode GenerativeModel GenerativeEvt
  -> GenerativeModel
  -> GenerativeEvt
  -> [EventResponse GenerativeModel GenerativeEvt ()]
handleEvent wenv node model evt = case evt of
  GenerativeInit -> [setFocus wenv "activeType"]

main :: IO ()
main = do
  simpleApp model handleEvent buildUI config
  where
    model = GenerativeModel CirclesGrid False def def
    config = [
      appWindowTitle "Generative art",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent GenerativeInit
      ]

seedList :: [Maybe Int]
seedList = Nothing : (Just <$> [0..100])

seedDesc :: Maybe Int -> Text
seedDesc Nothing = "Random"
seedDesc (Just v) = showt v

genTypeDesc :: GenerativeType -> Text
genTypeDesc CirclesGrid = "Randomness in size and location for circles"
genTypeDesc BoxesPalette = "Randomness in palette for boxes"

setFocus :: WidgetEnv s e -> Text -> EventResponse s e ep
setFocus wenv key = Request (SetFocus widgetId) where
  widgetId = fromMaybe def (globalKeyWidgetId wenv key)
