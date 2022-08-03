{-|
Module      : Main
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Main module for the 'Todo' example.
-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Data.FileEmbed (embedFile)
import Monomer.Main.Types (appFontDefMem)

type TodoWenv = WidgetEnv TodoModel TodoEvt
type TodoNode = WidgetNode TodoModel TodoEvt

todoRowKey :: Todo -> Text
todoRowKey todo = "todoRow" <> showt (todo ^. todoId)

todoRow :: TodoWenv -> TodoModel -> Int -> Todo -> TodoNode
todoRow wenv model idx t = animRow `nodeKey` todoKey where
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
    `styleBasic` [textFont "Medium", textSize 12, textAscender, textColor todoFg, padding 6, paddingH 8, radius 12, bgColor todoBg]

  rowButton caption action = button caption action
    `styleBasic` [textFont "Remix", textMiddle, textColor rowButtonColor, bgColor transparent, border 0 transparent]
    `styleHover` [bgColor sectionBg]
    `styleFocus` [bgColor (sectionBg & L.a .~ 0.5)]
    `styleFocusHover` [bgColor sectionBg]

  todoInfo = hstack [
      vstack [
        labelS (t ^. todoType) `styleBasic` [textSize 12, textColor darkGray],
        spacer_ [width 5],
        label (t ^. description) `styleBasic` [textThroughline_ todoDone]
      ],
      filler,
      box_ [alignRight] todoStatus `styleBasic` [width 80],
      spacer,
      rowButton remixEdit2Line (TodoEdit idx t),
      spacer,
      rowButton remixDeleteBinLine (TodoDeleteBegin idx t)
    ] `styleBasic` (paddingV 15 : [borderB 1 rowSepColor | not isLast])

  animRow = animFadeOut_ [onFinished (TodoDelete idx t)] todoInfo

todoEdit :: TodoWenv -> TodoModel -> TodoNode
todoEdit wenv model = editNode where
  sectionBg = wenv ^. L.theme . L.sectionColor
  isValidInput = model ^. activeTodo . description /= ""

  (saveAction, saveLabel) = case model ^. action of
    TodoEditing idx -> (TodoSave idx, "Save")
    _ -> (TodoAdd, "Add")

  saveTodoBtn = mainButton saveLabel saveAction

  editFields = keystroke [("Enter", saveAction) | isValidInput] $ vstack [
      hstack [
        label "Task:",
        spacer,
        textField (activeTodo . description) `nodeKey` "todoDesc"
      ],
      spacer,
      hgrid [
        hstack [
          label "Type:",
          spacer,
          textDropdownS (activeTodo . todoType) todoTypes `nodeKey` "todoType",
          spacer -- Added here to avoid grid expanding it to 1/3 total width
        ],
        hstack [
          label "Status:",
          spacer,
          textDropdownS (activeTodo . status) todoStatuses
        ]
      ]
    ]

  editNode = keystroke [("Esc", TodoCancel)] $ vstack [
      editFields,
      spacer,
      hstack [
        filler,
        saveTodoBtn `nodeEnabled` isValidInput,
        spacer,
        button "Cancel" TodoCancel
        ]
    ] `styleBasic` [bgColor sectionBg, padding 20]

buildUI :: TodoWenv -> TodoModel -> TodoNode
buildUI wenv model = widgetTree where
  sectionBg = wenv ^. L.theme . L.sectionColor
  isEditing = model ^. action /= TodoNone

  countLabel = label caption `styleBasic` styles where
    caption = "Tasks (" <> showt (length $ model ^. todos) <> ")"
    styles = [textFont "Regular", textSize 16, padding 20, bgColor sectionBg]

  todoList = vstack (zipWith (todoRow wenv model) [0..] (model ^. todos))

  newButton = mainButton "New" TodoNew `nodeKey` "todoNew"
    `nodeVisible` not isEditing

  editLayer = content where
    dualSlide content = outer where
      inner = animSlideIn_ [slideTop, duration 200] content
        `nodeKey` "animEditIn"
      outer = animSlideOut_ [slideTop, duration 200, onFinished TodoHideEditDone] inner
        `nodeKey` "animEditOut"

    content = vstack [
        dualSlide (todoEdit wenv model),
        filler
      ] `styleBasic` [bgColor (grayDark & L.a .~ 0.5)]

  mainLayer = vstack [
      countLabel,
      scroll_ [] (todoList `styleBasic` [padding 20, paddingT 5]),
      filler,
      box_ [alignRight] newButton
        `styleBasic` [bgColor sectionBg, padding 20]
    ]

  widgetTree = zstack [
      mainLayer,
      editLayer `nodeVisible` isEditing
    ]

handleEvent
  :: TodoWenv
  -> TodoNode
  -> TodoModel
  -> TodoEvt
  -> [EventResponse TodoModel TodoEvt TodoModel ()]
handleEvent wenv node model evt = case evt of
  TodoInit -> [SetFocusOnKey "todoNew"]

  TodoNew -> [
    Event TodoShowEdit,
    Model $ model
      & action .~ TodoAdding
      & activeTodo .~ def,
    SetFocusOnKey "todoDesc"]

  TodoEdit idx td -> [
    Event TodoShowEdit,
    Model $ model
      & action .~ TodoEditing idx
      & activeTodo .~ td,
    SetFocusOnKey "todoDesc"]

  TodoAdd -> [
    Event TodoHideEdit,
    Model $ addNewTodo wenv model,
    SetFocusOnKey "todoNew"]

  TodoSave idx -> [
    Event TodoHideEdit,
    Model $ model
      & todos . ix idx .~ (model ^. activeTodo),
    SetFocusOnKey "todoNew"]

  TodoDeleteBegin idx todo -> [
    Message (WidgetKey (todoRowKey todo)) AnimationStart]

  TodoDelete idx todo -> [
    Model $ model
      & action .~ TodoNone
      & todos .~ remove idx (model ^. todos),
    SetFocusOnKey "todoNew"]

  TodoCancel -> [
    Event TodoHideEdit,
    Model $ model
      & activeTodo .~ def,
    SetFocusOnKey "todoNew"]

  TodoShowEdit -> [
    Message "animEditIn" AnimationStart,
    Message "animEditOut" AnimationStop
    ]

  TodoHideEdit -> [
    Message "animEditIn" AnimationStop,
    Message "animEditOut" AnimationStart
    ]

  TodoHideEditDone -> [
    Model $ model
      & action .~ TodoNone]

addNewTodo :: WidgetEnv s e -> TodoModel -> TodoModel
addNewTodo wenv model = newModel where
  newTodo = model ^. activeTodo
    & todoId .~ currentTimeMs wenv
  newModel = model
    & todos .~ (newTodo : model ^. todos)

initialTodos :: [Todo]
initialTodos = todos where
  items = mconcat $ replicate 1 [
    Todo 0 Home Done "Tidy up the room",
    Todo 0 Home Pending "Buy groceries",
    Todo 0 Home Pending "Pay the bills",
    Todo 0 Home Pending "Repair kitchen sink",
    Todo 0 Work Done "Check the status of project A",
    Todo 0 Work Pending "Finish project B",
    Todo 0 Work Pending "Send email to clients",
    Todo 0 Work Pending "Contact cloud services provider"
    ]
  todos = zipWith (\t idx -> t & todoId .~ idx) items [0..]

main :: IO ()
main = do
  startApp (TodoModel initialTodos def TodoNone) handleEvent buildUI config
  where
    config = [
      appWindowTitle "Todo list",
      appWindowIcon "./assets/images/icon.png",
      appTheme customDarkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDefMem "Medium" $(embedFile "./assets/fonts/Roboto-Medium.ttf"),
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
