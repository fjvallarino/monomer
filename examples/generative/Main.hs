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
  genTypeDesc CirclesGrid = "Randomness in size and location for circles"
  genTypeDesc BoxesPalette = "Randomness in palette for boxes"
  widgetTree = vstack [
      textDropdown_ activeGenerative generativeTypes genTypeDesc [] `key` "activeType",
      zstack [
        circlesGrid def `visible` (model ^. activeGenerative == CirclesGrid),
        boxesPalette def `visible` (model ^. activeGenerative == BoxesPalette)
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

setFocus :: WidgetEnv s e -> Text -> EventResponse s e ep
setFocus wenv key = Request (SetFocus widgetId) where
  widgetId = fromMaybe def (globalKeyWidgetId wenv key)

main :: IO ()
main = do
  simpleApp (GenerativeModel CirclesGrid) handleEvent buildUI config
  where
    config = [
      appWindowTitle "Generative art",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent GenerativeInit
      ]
