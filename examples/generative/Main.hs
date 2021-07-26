{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Data.Default
import Data.List (intersperse)
import Data.Maybe
import Data.Text (Text)
import TextShow

import Monomer
import Monomer.Widgets.Single

import GenerativeTypes
import Widgets.BoxesPalette
import Widgets.CirclesGrid

import qualified Monomer.Lens as L

type GenerativeWenv = WidgetEnv GenerativeModel GenerativeEvt
type GenerativeNode = WidgetNode GenerativeModel GenerativeEvt

buildUI :: GenerativeWenv -> GenerativeModel -> GenerativeNode
buildUI wenv model = widgetTree where
  sectionBg = wenv ^. L.theme . L.sectionColor

  seedDropdown lens = textDropdown_ lens seedList seedDesc []

  widgetCircleCfg = vstack $ intersperse spacer [
      label "Width",
      vstack [
        dial_ (circlesCfg . itemWidth) 20 50 [dragRate 0.5],
        labelS (model ^. circlesCfg . itemWidth) `style` [textSize 14, textCenter]
      ],
      label "Seed",
      seedDropdown (circlesCfg . seed)
    ]

  widgetBoxCfg = vstack $ intersperse spacer [
      label "Width",
      vstack [
        dial_ (boxesCfg . itemWidth) 20 50 [dragRate 0.5],
        labelS (model ^. boxesCfg . itemWidth) `style` [textSize 14, textCenter]
      ],
      label "Seed",
      seedDropdown (boxesCfg . seed),
      separatorLine,
      label "Palette type",
      textDropdown (boxesCfg . paletteType) [1..4],
      label "Palette size",
      vstack [
        dial_ (boxesCfg . paletteSize) 1 50 [dragRate 0.5],
        labelS (model ^. boxesCfg . paletteSize) `style` [textSize 14, textCenter]
      ]
    ]

  widgetTree = vstack [
      hstack [
        label "Type:",
        spacer,
        textDropdown_ activeGen genTypes genTypeDesc [] `key` "activeType",
        spacer,
        labeledCheckbox "Show config:" showCfg
      ] `style` [padding 20, bgColor sectionBg],
      zstack [
        hstack [
          circlesGrid (model ^. circlesCfg) `style` [padding 20],
          widgetCircleCfg
            `visible` model ^. showCfg
            `style` [padding 20, width 200, bgColor sectionBg]
        ] `visible` (model ^. activeGen == CirclesGrid),
        hstack [
          boxesPalette (model ^. boxesCfg) `style` [padding 20],
          widgetBoxCfg
            `visible` model ^. showCfg
            `style` [padding 20, width 200, bgColor sectionBg]
        ] `visible` (model ^. activeGen == BoxesPalette)
      ]
    ]

handleEvent
  :: GenerativeWenv
  -> GenerativeNode
  -> GenerativeModel
  -> GenerativeEvt
  -> [EventResponse GenerativeModel GenerativeEvt GenerativeModel ()]
handleEvent wenv node model evt = case evt of
  GenerativeInit -> [setFocusOnKey wenv "activeType"]

main :: IO ()
main = do
  startApp model handleEvent buildUI config
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
