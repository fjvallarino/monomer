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
    todoDone = t ^. status == Done
    (todoBg, todoFg)
      | todoDone = (doneColor, editBgColor)
      | otherwise = (pendingColor, editBgColor)
    todoStatus = labelS (t ^. status)
      `style` [textSize 12, padding 4, paddingH 8, radius 12, bgColor todoBg, textColor todoFg]
    rowButton caption action = button caption action
      `style` [width 65, textColor gray, border 1 gray, bgColor transparent]
      `hover` [bgColor editBgColor]
      `focus` [border 1 lightSkyBlue]
    todoRow = hstack [
        vstack [
          labelS (t ^. todoType) `style` [textSize 12, textColor darkGray],
          spacer_ [width 5],
          label (t ^. description) `style` [textThroughline_ todoDone]
        ],
        filler,
        box_ [alignRight] todoStatus `style` [width 100],
        spacer,
        rowButton "Edit" (TodoEdit idx t),
        spacer,
        rowButton "Delete" (TodoDeleteBegin idx t)
      ] `style` [paddingT 10 | idx  > 0]
    slideWidget = fadeOut_ [onFinished (TodoDelete idx t)] todoRow
  todoEdit = vstack [
      hstack [
        label "Task:",
        spacer,
        textField (activeTodo . description) `key` "todoDesc"
      ],
      spacer,
      hgrid [
        hstack [
          label "Type:",
          spacer,
          textDropdownS (activeTodo . todoType) todoTypes `key` "todoType",
          spacer -- Added here to avoid grid expanding it to 1/3 total width
        ],
        hstack [
          label "Status:",
          spacer,
          textDropdownS (activeTodo . status) todoStatuses
        ]
      ],
      spacer,
      hstack [
        filler,
        case model ^. action of
          TodoAdding -> mainButton "Add" TodoAdd
          TodoEditing idx -> mainButton "Save" (TodoSave idx)
          _ -> spacer,
        spacer,
        button "Cancel" TodoCancel
        ]
    ] `style` [bgColor editBgColor, padding 20]
  todoList = vstack (zipWith todoView [0..] (model ^. todos))
  isEditing = model ^. action /= TodoNone
  newButton = mainButton "New" TodoNew `key` "todoNew"
    `visible` not isEditing
  widgetTree = vstack [
      keystroke [("Esc", TodoCancel)] todoEdit
        `visible` isEditing,
      scroll todoList `style` [padding 10],
      filler,
      box_ [alignRight] newButton
        `style` [bgColor editBgColor, padding 20]
    ]

todoRowKey :: Todo -> Text
todoRowKey todo = "todoRow" <> showt (todo ^. todoId)

doneColor :: Color
doneColor = rgbHex "#61C354"

pendingColor :: Color
pendingColor = rgbHex "#F5BE4F"

editBgColor :: Color
editBgColor = rgbHex "#404040"

handleEvent
  :: WidgetEnv TodoModel TodoEvt
  -> WidgetNode TodoModel TodoEvt
  -> TodoModel
  -> TodoEvt
  -> [EventResponse TodoModel TodoEvt TodoModel ()]
handleEvent wenv node model evt = case evt of
  TodoInit -> [setFocusOnKey wenv "todoDesc"]
  TodoNew -> [
    Model $ model
      & action .~ TodoAdding
      & activeTodo .~ def,
    setFocusOnKey wenv "todoDesc"]
  TodoEdit idx td -> [
    Model $ model
      & action .~ TodoEditing idx
      & activeTodo .~ td,
    setFocusOnKey wenv "todoDesc"]
  TodoAdd -> [
    Model $ addNewTodo wenv model,
    setFocusOnKey wenv "todoDesc"]
  TodoSave idx -> [
    Model $ model
      & action .~ TodoNone
      & todos . ix idx .~ (model ^. activeTodo),
    setFocusOnKey wenv "todoDesc"]
  TodoDeleteBegin idx todo -> [
    Message (WidgetKey (todoRowKey todo)) AnimationStart]
  TodoDelete idx todo -> [
    Model $ model
      & action .~ TodoNone
      & todos .~ remove idx (model ^. todos),
    setFocusOnKey wenv "todoDesc"]
  TodoCancel -> [
    Model $ model
      & action .~ TodoNone
      & activeTodo .~ def,
    setFocusOnKey wenv "todoDesc"]

addNewTodo :: WidgetEnv s e -> TodoModel -> TodoModel
addNewTodo wenv model = newModel where
  newTodo = model ^. activeTodo
    & todoId .~ wenv ^. L.timestamp
  newModel = model
    & action .~ TodoNone
    & todos .~ (newTodo : model ^. todos)

remove :: Int -> [a] -> [a]
remove idx ls = take idx ls ++ drop (idx + 1) ls

initialTodos :: [Todo]
initialTodos = todos where
  items = mconcat $ replicate 5 [
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
