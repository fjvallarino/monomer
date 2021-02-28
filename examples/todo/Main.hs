{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Data.Default
import Data.Maybe
import Data.Text (Text)
import TextShow

import Monomer

import TodoTypes

buildUI
  :: WidgetEnv TodoModel TodoEvt
  -> TodoModel
  -> WidgetNode TodoModel TodoEvt
buildUI wenv model = widgetTree where
  todoView idx t = hstack [
      labelS (t ^. todoType) `style` [width 50],
      label (t ^. description) `style` [textThroughline_ (t ^. status == Done)],
      filler,
      labelS (t ^. status) `style` [width 100],
      button "Edit" (TodoEdit idx t) `style` [width 60],
      spacer,
      button "Delete" (TodoDelete idx) `style` [width 60]
    ] `style` [paddingV 2]
  todoEdit = vstack [
      hgrid [
        hstack [
          label "Type: ",
          spacer,
          textDropdownS (activeTodo . todoType) todoTypes `key` "todoType",
          spacer
        ],
        hstack [
          label "Status: ",
          spacer,
          textDropdownS (activeTodo . status) todoStatuses
        ]
      ],
      spacer,
      hstack [
        label "Description: ",
        spacer,
        textField (activeTodo . description) `key` "todoDesc"
      ],
      spacer,
      hstack [
        case model ^. action of
          TodoAdding -> button "Add" TodoAdd `style` [width 100]
          TodoEditing idx -> button "Save" (TodoSave idx) `style` [width 100]
          _ -> spacer,
        spacer,
        button "Cancel" TodoCancel `style` [width 100],
        filler
        ]
    ]
  widgetTree = vstack [
      keystroke [("Esc", TodoCancel)]
        (todoEdit `style` [padding 4]) `visible` (model ^. action /= TodoNone),
      scroll $ vstack (zipWith todoView [0..] (model ^. todos)) `style` [paddingH 4],
      filler,
      box_ [expandContent] (button "New" TodoNew `key` "todoNew") `style` [padding 4]
    ]

handleEvent
  :: WidgetEnv TodoModel TodoEvt
  -> WidgetNode TodoModel TodoEvt
  -> TodoModel
  -> TodoEvt
  -> [EventResponse TodoModel TodoEvt ()]
handleEvent wenv node model evt = case evt of
  TodoInit -> [setFocus wenv "todoNew"]
  TodoNew -> [
    Model $ model
      & action .~ TodoAdding
      & activeTodo .~ def,
    setFocus wenv "todoType"]
  TodoEdit idx td -> [
    Model $ model
      & action .~ TodoEditing idx
      & activeTodo .~ td,
    setFocus wenv "todoType"]
  TodoAdd -> [
    Model $ model
      & action .~ TodoNone
      & todos .~ (model ^. activeTodo : model ^. todos),
    setFocus wenv "todoNew"]
  TodoSave idx -> [
    Model $ model
      & action .~ TodoNone
      & todos . ix idx .~ (model ^. activeTodo),
    setFocus wenv "todoNew"]
  TodoDelete idx -> [
    Model $ model
      & action .~ TodoNone
      & todos .~ remove idx (model ^. todos),
    setFocus wenv "todoNew"]
  TodoCancel -> [
    Model $ model
      & action .~ TodoNone
      & activeTodo .~ def,
    setFocus wenv "todoNew"]

remove :: Int -> [a] -> [a]
remove idx ls = take idx ls ++ drop (idx + 1) ls

setFocus :: WidgetEnv s e -> Text -> EventResponse s e ep
setFocus wenv key = Request (SetFocus path) where
  path = fromMaybe rootPath (globalKeyPath wenv key)

initialTodos :: [Todo]
initialTodos = mconcat $ replicate 5 [
    Todo Home Done "Tidy up the room",
    Todo Home Pending "Buy groceries",
    Todo Home Pending "Pay the bills",
    Todo Work Pending "Check the status of project A",
    Todo Work Pending "Finish project B"
  ]

main :: IO ()
main = do
  simpleApp (TodoModel initialTodos def TodoNone) handleEvent buildUI config
  where
    config = [
      appWindowTitle "Todo list",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent TodoInit
      ]
