{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Data.Default
import Data.Maybe
import Data.Text (Text)
import TextShow

import Monomer

import TodoTypes

import qualified Monomer.Lens as L

buildUI
  :: WidgetEnv TodoModel TodoEvt
  -> TodoModel
  -> WidgetNode TodoModel TodoEvt
buildUI wenv model = widgetTree where
  todoView idx t = slideWidget `key` todoKey where
    todoKey = todoRowKey t
    todoRow = hstack [
        labelS (t ^. todoType) `style` [width 50],
        label (t ^. description) `style` [textThroughline_ (t ^. status == Done)],
        filler,
        labelS (t ^. status) `style` [width 100],
        button "Edit" (TodoEdit idx t) `style` [width 60],
        spacer,
        button "Delete" (TodoDeleteBegin idx t) `style` [width 60]
      ] `style` [paddingV 2]
    slideWidget = fadeOut_ [onFinished (TodoDelete idx t)] todoRow
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
  -> [EventResponse TodoModel TodoEvt TodoModel ()]
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
    Model $ addNewTodo wenv model,
    setFocus wenv "todoNew"]
  TodoSave idx -> [
    Model $ model
      & action .~ TodoNone
      & todos . ix idx .~ (model ^. activeTodo),
    setFocus wenv "todoNew"]
  TodoDeleteBegin idx todo -> [
    Message (WidgetKey (todoRowKey todo)) AnimationStart]
  TodoDelete idx todo -> [
    Model $ model
      & action .~ TodoNone
      & todos .~ remove idx (model ^. todos),
    setFocus wenv "todoNew"]
  TodoCancel -> [
    Model $ model
      & action .~ TodoNone
      & activeTodo .~ def,
    setFocus wenv "todoNew"]

addNewTodo :: WidgetEnv s e -> TodoModel -> TodoModel
addNewTodo wenv model = newModel where
  newTodo = model ^. activeTodo
    & todoId .~ wenv ^. L.timestamp
  newModel = model
    & action .~ TodoNone
    & todos .~ (newTodo : model ^. todos)

remove :: Int -> [a] -> [a]
remove idx ls = take idx ls ++ drop (idx + 1) ls

setFocus :: WidgetEnv s e -> Text -> EventResponse s e sp ep
setFocus wenv key = Request (SetFocus widgetId) where
  widgetId = fromMaybe def (globalKeyWidgetId wenv key)

todoRowKey :: Todo -> Text
todoRowKey todo = "todoRow" <> showt (todo ^. todoId)

initialTodos :: [Todo]
initialTodos = todos where
  items = mconcat $ replicate 1 [
    Todo 0 Home Done "Tidy up the room",
    Todo 0 Home Pending "Buy groceries",
    Todo 0 Home Pending "Pay the bills",
    Todo 0 Work Pending "Check the status of project A",
    Todo 0 Work Pending "Finish project B"
    ]
  todos = zipWith (\t idx -> t & todoId .~ idx) items [0..]

main :: IO ()
main = do
  startApp (TodoModel initialTodos def TodoNone) handleEvent buildUI config
  where
    config = [
      appWindowTitle "Todo list",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent TodoInit
      ]
