{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

import Control.Concurrent (threadDelay)
import Control.Lens ((&), (^.), (.~), (%~))
import Data.Default
import TextShow

import qualified Data.Text as T

import System.Remote.Monitoring

import Monomer

import qualified Monomer.Lens as L

import KeysComposite
import TestComposite
import Types

main :: IO ()
main = do
  --forkServer "localhost" 28000

  let model = def
  let theme :: Theme = darkTheme
  --      & L.basic . L.fgColor .~ blue
  --      & L.hover . L.fgColor .~ white
  --      & L.focus . L.fgColor .~ white
  let config = [
        --windowSize (1280, 960),
        --windowSize (320, 240),
        --appWindowState MainWindowFullScreen,
        --appWindowState MainWindowMaximized,
        --appWindowState $ MainWindowNormal (640, 480),
        --appWindowResizable False,
        --appWindowBorder False,
        appMaxFps 10,
        appWindowTitle "This is my title",
        appUseHdpi True,
        appTheme theme,
        appInitEvent InitApp,
        appExitEvent CancelExitApp,
        appMainButton LeftBtn,
        appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
        appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
        appFontDef "Italic" "./assets/fonts/Roboto-Italic.ttf" ]

  simpleApp_ model handleAppEvent buildUI config
  --simpleApp model handleAppEvent buildUI

handleAppEvent
  :: WidgetEnv App AppEvent
  -> App
  -> AppEvent
  -> [AppEventResponse App AppEvent]
handleAppEvent wenv model evt = case evt of
  IncButton -> [Model (model & clickCount %~ (+1))]
--  PrintMessage txt -> Model (model & showAlert .~ True)
  PrintMessage txt -> [Task $ do
    print txt
    return Nothing]
  CheckboxSt st -> [Task $ do
    putStrLn $ "Checkbox is: " ++ show st
    return Nothing]
--  RadioSt st -> Task $ do
--    putStrLn $ "Radio is: " ++ show st
--    return Nothing
  RadioSt st -> [Model (model & fruit .~ st)]
  ImageMsg msg -> [Task $ do
    putStrLn $ "Image msg is: " ++ show msg
    return Nothing]
  DropdownVal txt -> [Task $ do
    threadDelay 200
    --putStrLn $ "Dropdown txt is: " ++ show txt
    return Nothing]
  DropdownIdx idx txt -> [Task $ do
    threadDelay 300
    --putStrLn $ "Dropdown (idx, txt) is: " ++ show (idx,  txt)
    return Nothing]
  ShowAlert -> [Model (model & showAlert .~ True)]
  CloseAlert -> [Model (model & showAlert .~ False)]
  ShowConfirm -> [Model (model & showConfirm .~ True)]
  AcceptConfirm -> [Model (model & showConfirm .~ False)]
  CancelConfirm -> [Model (model & showConfirm .~ False)]
  RunShortTask -> [Task $ do
    putStrLn "Running!"
    return $ Just (PrintMessage "Done!")]
  ChangeTitle title -> [Request (UpdateWindow (WindowSetTitle title))]
  ExitApp -> [Request $ ExitApplication True]
  CancelExitApp -> [] --[Request $ ExitApplication False]
  FullWindow -> [Request (UpdateWindow WindowSetFullScreen)]
  MaxWindow -> [Request (UpdateWindow WindowMaximize)]
  MinWindow -> [Request (UpdateWindow WindowMinimize), Event RestoreWindowSchedule]
  RestoreWindowSchedule -> [Task $ do
    threadDelay 2000000
    return $ Just RestoreWindow]
  RestoreWindow -> [Request (UpdateWindow WindowRestore)]
  ToFrontWindow -> [Request (UpdateWindow WindowBringToFront)]
  ToFrontWindowSchedule -> [Task $ do
    threadDelay 2000000
    return $ Just ToFrontWindow]
  _ -> []

buildUI :: WidgetEnv App AppEvent -> App -> WidgetNode App AppEvent
buildUI wenv model = trace "Creating UI" widgetTree where
  widgetInput = vstack [
      dropdown_ dropdown1 items label label [maxHeight 200],
      hgrid [
        label "Username: ",
        textField textField1,
        label ""
      ] `style` [padding 5],
      hgrid [
        label "Password: ",
        textField textField2,
        label ""
      ] `style` [padding 5, paddingT 0],
      hgrid [
        label "Integral: ",
        integralField_ integer1 [minValue 0],
        label ""
      ] `style` [padding 5, paddingT 0],
      hgrid [
        label "Floating: ",
        floatingField_ float1 [minValue (-20), maxValue 20],
        label ""
      ] `style` [padding 5, paddingT 0]
    ]
  widgetLV = vstack [
--      scroll $ vstack $ (\i -> box $ label ("Label: " <> showt i)) <$> [0..1000::Int]

      listView dropdown1 items label
      ,
      dropdown_ dropdown1 items label label [maxHeight 200]
    ]
  widgetWindow = vstack [
      hstack [
        label "Title: ",
        textField_ textField1 [onChange ChangeTitle]
      ],
      hstack [
        button "Fullscreen" FullWindow,
        button "Maximize" MaxWindow,
        button "Minimize" MinWindow,
        button "Restore" RestoreWindow,
        button "To Front" ToFrontWindowSchedule,
        button "Short" RunShortTask,
        button "Exit" ExitApp
      ]
    ]
  widgetTreeAlt
    | model ^. clickCount `mod` 2 == 0 = widgetTree10
    | otherwise = widgetTree11
  widgetTree10 = vstack [
      hstack [
        label ("Click count: " <> showt (model ^. clickCount)),
        button "Increase" IncButton
      ],
      image "assets/images/beach.jpg" `visible` False `key` "Beach",
      image "assets/images/pecans.jpg" `key` "Pecans"
    ]
  widgetTree11 = vstack [
      hstack [
        label ("Click count: " <> showt (model ^. clickCount)),
        button "Increase" IncButton
      ],
      image "assets/images/pecans.jpg" `visible` False `key` "Pecans",
      image "assets/images/beach.jpg" `key` "Beach"
    ]
  widgetTree5 = zstack_ [
      hgrid [
        label "test",
        vstack [
          textField textField1 `style` [bgColor blue],
          textField textField1 `style` [bgColor pink],
          textField textField1 `style` [bgColor orange]
        ]
      ],
      hstack_ [
        textField textField1 `style` [bgColor lightBlue, width 200]
      ] [ignoreEmptyClick True]
    ] [onlyTopActive False]
  widgetTree4 = hgrid [
      label "" `style` [bgColor blue],
      label "" `style` [bgColor gray],
      label "" `style` [bgColor blue],
      label "" `style` [bgColor orange],
      vstack [
        label "1" `style` [bgColor pink, border 1 pink],
        textField textField1 `style` [bgColor gray],
        label "2" `style` [bgColor pink, border 1 gray],
        textField textField1 `style` [textCenter, bgColor gray],
        label "3" `style` [bgColor pink],
        textField textField1 `style` [textRight, bgColor gray],
        label "4" `style` [bgColor pink]
      ],
      vstack [
        --textDropdown_ dropdown1 items id [onChange DropdownVal, onChangeIdx DropdownIdx],
        label "1" `style` [bgColor pink, border 1 pink]
      ] `visible` False,
      label "" `style` [bgColor orange],
      label "" `style` [bgColor gray],
      label "" `style` [bgColor blue],
      label "" `style` [bgColor gray]
    ]
  widgetTree3 = hgrid [
      label "Hi!\nThis\nis\na\nnew\ttest\n\n  Double!" `style` [bgColor pink, textBottom, textCenter],
      vgrid [
        label "1",
        label "2",
        label "3",
        label "4",
        label "5",
        label_ "This is a really long label used to check if line breaks and ellipsis areee implemented correctly" [textMultiLine] `style` [bgColor blue],
        label "6",
        label_ "This is a really long label used to check if line breaks and ellipsis are implemented correctly, using a longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong invalid word" [textClip] `style` [bgColor blue, textBottom, textRight]
      ],
      label "",
      label ""
    ]
  widgetTree2 = zstack [
      vstack [
        hstack [
          radio fruit Apple,
          radio fruit Orange,
          checkbox condition1
        ],
        dropdown_ dropdown1 items label label [maxHeight 200],
        label "Integral",
        integralField integer1,
        label "Floating",
        floatingField float1,
        --image "assets/images/pecans.jpg"
        listView_ dropdown1 items label [],
        --dropdown dropdown1 items id label
        label "Text",
        textField textField1
      ]-- `style` [padding 10]
    ]
  longMessage word = "Are you sure?\n\n\n\n" <> T.replicate 100 (word <> " ")
  widgetTree = zstack_ [
      widgetTreeFull,
      alert (longMessage "Alert") CloseAlert `visible` model ^. showAlert,
      confirm (longMessage "Confirm") AcceptConfirm CancelConfirm `visible` model ^. showConfirm
    ] [onlyTopActive False]
  widgetTreeFull = vstack [
      hstack [
        radioV (model ^. fruit) RadioSt Apple,
        radioV (model ^. fruit) RadioSt Orange,
        radioV (model ^. fruit) RadioSt Pear
      ] `key` "radio hstack",
      hgrid [
        button "Show Alert" ShowAlert,
        mainButton "Show Confirm" ShowConfirm
      ],
      hgrid [
        vstack [
          label "jLabel 1" `style` [bgColor darkGray],
          label "Label 12" `style` [bgColor lightGray],
          label "Label 123" `style` [bgColor darkGray],
          label "Label 1234" `style` [bgColor lightGray]
        ] `style` [bgColor red],
        vstack [
          label "jLabel 1" `style` [bgColor lightGray, textBottom],
          label "Label 12" `style` [bgColor darkGray],
          label "Label 123" `style` [bgColor lightGray],
          label "Label 1234" `style` [bgColor darkGray]
        ] `style` [bgColor blue]
      ] `style` [bgColor green],
      --label (model ^. textField1) `style` [bgColor lightBlue, textLeft],
      textField textField1 `style` [bgColor lightBlue, textLeft],
      hgrid [
        label_ "This is a really long label used to check what I did works fine" [textMultiLine, textEllipsis],
        label "Jj label"
      ] `hover` [bgColor red],
      hstack [
        scroll_ (
          image_ "assets/images/pecans.jpg" [fitFill] `style` [minWidth 200]
        ) []
        ,
        scroll_ (
          image_ "assets/images/pecans.jpg" [fitFill] `style` [minWidth 200]
        ) []
        ,
        scroll_ (
          image_ "assets/images/pecans.jpg" [fitFill] `style` [minWidth 200]
        ) []
        ,
        spacer_ [resizeFactor 1],
        image_ "https://picsum.photos/600/400" [fitFill, onLoadError ImageMsg]
      ],
      textDropdown_ dropdown1 items id [onChange DropdownVal, onChangeIdx DropdownIdx],
      button_ "Click\nme!" (PrintMessage "Button clicked") [textMultiLine]
    ] `key` "main vstack" `style` [borderT 20 red, borderL 10 blue, borderR 10 green, borderB 10 gray, iradius 50] --, padding 20
  items = fmap (\i -> "This is a long label: " <> showt i) [1..3::Int]
