{-# LANGUAGE BinaryLiterals #-}
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
import qualified Data.Text as T

type TodoWenv = WidgetEnv TodoModel TodoEvt
type TodoNode = WidgetNode TodoModel TodoEvt

todoRowKey :: Todo -> Text
todoRowKey todo = "todoRow" <> showt (todo ^. todoId)

todoRow :: TodoWenv -> TodoModel -> Int -> Todo -> TodoNode
todoRow wenv model idx t = slideWidget `key` todoKey where
  sectionBg = wenv ^. L.theme . L.sectionColor
  rowButtonColor = wenv ^. L.theme . L.userColorMap . at "rowButton" . non def
  rowSepColor = gray & L.a .~ 0.5

  todoKey = todoRowKey t
  todoDone = t ^. status == Done
  isLast = idx == length (model ^. todos) - 1

  (todoBg, todoFg)
    | todoDone = (doneBg, doneFg)
    | otherwise = (pendingBg, pendingFg)

  todoStatus = labelS (t ^. status)
    `style` [textFont "Medium", textSize 12, textAscender, textColor todoFg, padding 6, paddingH 8, radius 12, bgColor todoBg]

  rowButton caption action = button caption action
    `style` [textFont "Remix", textMiddle, textColor rowButtonColor, bgColor transparent, border 0 transparent]
    `hover` [bgColor sectionBg]

  todoInfo = hstack [
      vstack [
        labelS (t ^. todoType) `style` [textSize 12, textColor darkGray],
        spacer_ [width 5],
        label (t ^. description) `style` [textThroughline_ todoDone]
      ],
      filler,
      box_ [alignRight] todoStatus `style` [width 80],
      spacer,
      rowButton remixEdit2Line (TodoEdit idx t),
      spacer,
      rowButton remixDeleteBinLine (TodoDeleteBegin idx t)
    ] `style` (paddingV 15 : [borderB 1 rowSepColor | not isLast])

  slideWidget = fadeOut_ [onFinished (TodoDelete idx t)] todoInfo


todoEdit :: TodoWenv -> TodoModel -> TodoNode
todoEdit wenv model = editNode where
  sectionBg = wenv ^. L.theme . L.sectionColor

  saveTodoBtn = case model ^. action of
    TodoAdding -> mainButton "Add" TodoAdd
    TodoEditing idx -> mainButton "Save" (TodoSave idx)
    _ -> spacer

  editNode = vstack [
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
        saveTodoBtn `enabled` (model ^. activeTodo . description /= ""),
        spacer,
        button "Cancel" TodoCancel
        ]
    ] `style` [bgColor sectionBg, padding 20]

buildUI :: TodoWenv -> TodoModel -> TodoNode
buildUI wenv model = widgetTree where
  sectionBg = wenv ^. L.theme . L.sectionColor
  isEditing = model ^. action /= TodoNone

  countLabel = label caption `style` styles where
    caption = "Tasks (" <> showt (length $ model ^. todos) <> ")"
    styles = [textFont "Regular", textSize 16, padding 20, bgColor sectionBg]

  todoList = vstack (zipWith (todoRow wenv model) [0..] (model ^. todos))

  newButton = mainButton "New" TodoNew `key` "todoNew"
    `visible` not isEditing

  widgetTree = vstack [
      keystroke [("Esc", TodoCancel)] (todoEdit wenv model)
        `visible` isEditing,
      countLabel,
      scroll_ [] (todoList `style` [padding 20, paddingT 5]),
      filler,
      box_ [alignRight] newButton
        `style` [bgColor sectionBg, padding 20]
    ]

handleEvent
  :: TodoWenv
  -> TodoNode
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

initialTodos :: [Todo]
initialTodos = todos where
  items = mconcat $ replicate 2 [
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
      appTheme customLightTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Medium" "./assets/fonts/Roboto-Medium.ttf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appFontDef "Remix" "./assets/fonts/remixicon.ttf",
      appInitEvent TodoInit
      ]

doneBg = rgbHex "#CFF6E2"
doneFg = rgbHex "#459562"
pendingBg = rgbHex "#F5F0CC"
pendingFg = rgbHex "#827330"
grayLight = rgbHex "#9E9E9E"
grayDark = rgbHex "#393939"
grayDarker = rgbHex "#2E2E2E"

customLightTheme :: Theme
customLightTheme = lightTheme
--  & L.userColorMap . at "statusFont" ?~ grayDarker
  & L.userColorMap . at "rowButton" ?~ grayLight

customDarkTheme :: Theme
customDarkTheme = darkTheme
--  & L.userColorMap . at "statusFont" ?~ grayDarker
  & L.userColorMap . at "rowButton" ?~ gray

remove :: Int -> [a] -> [a]
remove idx ls = take idx ls ++ drop (idx + 1) ls
