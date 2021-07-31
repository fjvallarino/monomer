{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tutorial06_Composite where

import Control.Concurrent (threadDelay)
import Control.Lens
import Data.List
import Data.Text (Text)
import Monomer
import TextShow

import qualified Data.Text as T
import qualified Monomer.Lens as L

data CompModel = CompModel {
  _listA :: [Int],
  _listB :: [Int]
} deriving (Eq, Show)

data CompEvent
  = DropToA Int
  | DropToB Int
  deriving (Eq, Show)

data AppModel = AppModel {
  _showDialog :: Bool,
  _parentModel :: CompModel,
  _dialogModel :: CompModel
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | ShowDialog
  | CloseDialog
  deriving (Eq, Show)

makeLenses 'CompModel
makeLenses 'AppModel

buildUIComp
  :: WidgetEnv CompModel CompEvent
  -> CompModel
  -> WidgetNode CompModel CompEvent
buildUIComp wenv model = widgetTree where
  sectionBg = rgbHex "#80B6FD"
  sectionHover = rgbHex "#A0D8FD"
  textDrag = rgbHex "#E0FFFF"

  itemA val = label ("Item: " <> showt val)
    `styleBasic` [textColor black, padding 5]

  dragItem val = draggable_ val
      [draggableStyle [bgColor textDrag, radius 5]]
      (itemA val)
    `styleBasic` [cursorHand]

  dragList items = vstack (dragItem <$> items)

  dropContainer target list = dropTarget_ target
      [dropTargetStyle [radius 10, bgColor sectionHover]]
      (dragList (model ^. list))
    `styleBasic` [minWidth 100, flexHeight 100, padding 5, radius 10, bgColor sectionBg]

  dropTargetA = dropContainer DropToA listA
  dropTargetB = dropContainer DropToB listB

  widgetTree = hstack [
      box dropTargetA `styleBasic` [paddingR 5],
      box dropTargetB `styleBasic` [paddingL 5]
    ]

handleEventComp
  :: WidgetEnv CompModel CompEvent
  -> WidgetNode CompModel CompEvent
  -> CompModel
  -> CompEvent
  -> [EventResponse CompModel CompEvent sp ep]
handleEventComp wenv node model evt = case evt of
  DropToA val -> [Model $ model
    & listA .~ sort (val : model ^. listA)
    & listB .~ delete val (model ^. listB)]

  DropToB val -> [Model $ model
    & listA .~ delete val (model ^. listA)
    & listB .~ sort (val : model ^. listB)]

compWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => ALens' sp CompModel
  -> WidgetNode sp ep
compWidget field = composite "compWidget" field buildUIComp handleEventComp

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  baseLayer = vstack [
      compWidget parentModel,
      spacer,
      hstack [
        button "Show Dialog" ShowDialog
      ]
    ] `styleBasic` [padding 10]

  closeIcon = icon IconClose
    `styleBasic` [width 16, height 16, fgColor black, cursorHand]

  dialogLayer = vstack [
      hstack [
        filler,
        box_ [alignTop, onClick CloseDialog] closeIcon
      ],
      spacer,
      compWidget dialogModel
    ] `styleBasic` [width 500, height 400, padding 10, radius 10, bgColor darkGray]

  widgetTree = zstack [
      baseLayer,
      box_ [alignCenter, alignMiddle] dialogLayer
        `visible` model ^. showDialog
        `styleBasic` [bgColor (gray & L.a .~ 0.8)]
    ]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  ShowDialog -> [Model $ model & showDialog .~ True]
  CloseDialog -> [Model $ model & showDialog .~ False]

main06 :: IO ()
main06 = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Tutorial 06 - Composite",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appInitEvent AppInit
      ]
    compModel = CompModel {
      _listA = [1..10],
      _listB = []
    }
    model = AppModel {
      _showDialog = False,
      _parentModel = compModel,
      _dialogModel = compModel
    }
