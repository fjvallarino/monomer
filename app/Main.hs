{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

import Control.Concurrent (threadDelay)
import Control.Lens ((&), (^.), (.~), (?~), (%~))
import Data.Default
import Data.List (delete)
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
        appMaxFps 60,
        appWindowTitle "This is my title",
        appUseHdpi True,
        appTheme theme,
        appInitEvent InitApp,
        appDisposeEvent DisposeApp,
        appExitEvent CancelExitApp,
        appResizeEvent ResizeApp,
        appMainButton LeftBtn,
        --appStateFileMain "main-tree.ser",
        appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
        appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
        appFontDef "Italic" "./assets/fonts/Roboto-Italic.ttf" ]

  simpleApp model handleAppEvent buildUI config

handleAppEvent
  :: WidgetEnv App AppEvent
  -> WidgetNode App AppEvent
  -> App
  -> AppEvent
  -> [AppEventResponse App AppEvent]
handleAppEvent wenv node model evt = case evt of
  SliderPos pos -> [Task $ do
    print pos
    return IgnoreEvt]
  IncButton -> [Model (model & clickCount %~ (+1)),
    Task $ do
      threadDelay 1000000
      putStrLn "Done 1"
      return IgnoreEvt,
    Task $ do
      threadDelay 2000000
      putStrLn "Done 2"
      return IgnoreEvt
    ]
--  PrintMessage txt -> Model (model & showAlert .~ True)
  PrintMessage txt -> [Task $ do
    print txt
    return IgnoreEvt]
  CheckboxSt st -> [Task $ do
    putStrLn $ "Checkbox is: " ++ show st
    return IgnoreEvt]
--  RadioSt st -> Task $ do
--    putStrLn $ "Radio is: " ++ show st
--    return IgnoreEvt
  RadioSt st -> [Model (model & fruit .~ st)]
  ImageMsg msg -> [Task $ do
    putStrLn $ "Image msg is: " ++ show msg
    return IgnoreEvt]
  DropdownVal txt -> [Task $ do
    threadDelay 200
    --putStrLn $ "Dropdown txt is: " ++ show txt
    return IgnoreEvt]
  DropdownIdx idx txt -> [Task $ do
    threadDelay 300
    --putStrLn $ "Dropdown (idx, txt) is: " ++ show (idx,  txt)
    return IgnoreEvt]
  ShowAlert -> [Model (model & showAlert .~ True)]
  CloseAlert -> [Model (model & showAlert .~ False)]
  ShowConfirm -> [Model (model & showConfirm .~ True)]
  AcceptConfirm -> [Model (model & showConfirm .~ False)]
  CancelConfirm -> [Model (model & showConfirm .~ False)]
  RunShortTask -> [Task $ do
    putStrLn "Running!"
    return $ PrintMessage "Done!"]
  ChangeTitle title -> [Request (UpdateWindow (WindowSetTitle title))]
  InitApp -> [Task $ putStrLn "Init" >> return IgnoreEvt ]
  DisposeApp -> [Task $ putStrLn "Dispose" >> return IgnoreEvt ]
  ResizeApp newSize -> [Task $ print ("Resize", newSize) >> return IgnoreEvt ]
  ExitApp -> [Request $ ExitApplication True]
  CancelExitApp -> [] --[Request $ ExitApplication False]
  FullWindow -> [Request (UpdateWindow WindowSetFullScreen)]
  MaxWindow -> [Request (UpdateWindow WindowMaximize)]
  MinWindow -> [Request (UpdateWindow WindowMinimize), Event RestoreWindowSchedule]
  RestoreWindowSchedule -> [Task $ do
    threadDelay 2000000
    return RestoreWindow]
  RestoreWindow -> [Request (UpdateWindow WindowRestore)]
  ToFrontWindow -> [Request (UpdateWindow WindowBringToFront)]
  ToFrontWindowSchedule -> [Task $ do
    threadDelay 2000000
    return ToFrontWindow]
  DropTo1 idx -> [Model $ model
    & dragList2 .~ delete idx (model ^. dragList2)
    & dragList1 .~ model ^. dragList1 ++ [idx]]
  DropTo2 idx -> [Model $ model
    & dragList1 .~ delete idx (model ^. dragList1)
    & dragList2 .~ model ^. dragList2 ++ [idx]]
  StartAnimation -> [
      Message "anim1" AnimationStart,
      Message "anim2" AnimationStart
    ]
  StopAnimation -> [
      Message "anim1" AnimationStop,
      Message "anim2" AnimationStop
    ]
  IgnoreEvt -> []
  UpdateColor col -> trace "Change" []
  FocusColor prev -> trace "Focus" []
  BlurColor next -> trace "Blur" []
  _ -> []

buildUI :: WidgetEnv App AppEvent -> App -> WidgetNode App AppEvent
buildUI wenv model = traceShow "Creating UI" widgetDial where
  widgetSlider = vstack [
      image_ "assets/images/pecans.jpg" [fitFill, imageRepeatX],
      hstack [externalLink "Launch GitHub" "http://www.github.com"],
      colorPicker_ color [colorPickerAlpha True, onChange UpdateColor, onFocus FocusColor, onBlur BlurColor],
      labelS (model ^. int1),
      hslider_ int1 (-100) 100 [sliderRadius 10, sliderWidth 20],
      hstack [
        vslider_ int1 (-100) 100 [sliderRadius 3, sliderThumbVisible True]
      ]
    ] `style` [paddingT 1]
  widgetSimple = vstack [
      label $ "Count: " <> showt (model ^. clickCount),
      button "Increase" IncButton
    ]
  widgetAnimate = vstack [
      slideIn_ [leftSide] (label "Hello!!!!" `style` [bgColor red]) `key` "anim1",
      slideOut_ [leftSide] (label "Good bye!!!!" `style` [bgColor green]) `key` "anim2",
      hstack [
        labelS (model ^. clickCount),
        button "Increase" IncButton
      ],
      button "Start" StartAnimation,
      button "Stop" StopAnimation
    ]
  widgetButtons = vstack [
      button "Confirm" ShowConfirm
    ]
  widgetLabels = vstack [
      label "Underline |" `style` [textFont "Italic", textSize 100, textUnderline],
      label "Overline |" `style` [textFont "Italic", textSize 100, textOverline],
      label "Through |" `style` [textFont "Italic", textSize 100, textThroughline],
      label "This is a test: All styles |" `style` [textFont "Italic", textSize 100, textUnderline, textOverline, textThroughline]
    ]
  widgetScroll = vscroll (hgrid [
      vstack [
        scroll (image "assets/images/pecans.jpg") `style` [height 200],
        scroll (image "assets/images/pecans.jpg") `style` [height 200],
        scroll (image "assets/images/pecans.jpg") `style` [height 200],
        scroll (image "assets/images/pecans.jpg") `style` [height 200],
        widgetDrag,
        scroll (image "assets/images/pecans.jpg") `style` [height 200],
        scroll (image "assets/images/pecans.jpg") `style` [height 200],
        scroll (image "assets/images/pecans.jpg") `style` [height 200],
        widgetLVs,
        scroll (image "assets/images/pecans.jpg") `style` [height 200],
        scroll (image "assets/images/pecans.jpg") `style` [height 200],
        scroll (image "assets/images/pecans.jpg") `style` [height 200],
        listView dropdown1 items label `style` [height 300],
        scroll (image "assets/images/pecans.jpg") `style` [height 200],
        scroll (image "assets/images/pecans.jpg") `style` [height 200],
        scroll (image "assets/images/pecans.jpg") `style` [height 200]
      ],
      label "Test"
    ])
  mkDd i = textDropdown dropdown1 items
  widgetLVs = scroll $ vstack (mkDd <$> [1..40::Int])
  widgetThemeSwitch = hstack [
      label "Test",
      themeSwitch (darkTheme & L.basic . L.labelStyle . L.bgColor ?~ red) (label "Test")
    ]
  labelDrag idx = draggable_ idx [transparency 0.8, draggableStyle [bgColor pink, border 20 blue]] (tooltip ("TT: " <> showt idx) $ label ("Label: " <> showt idx) `hover` [cursorIcon CursorHand])
  widgetDrag = hgrid [
      dropTarget DropTo1 (scroll $ vstack (fmap labelDrag (model ^. dragList1))),
      dropTarget_ DropTo2 [dropTargetStyle [bgColor orange]] (scroll $ vstack (fmap labelDrag (model ^. dragList2)))
    ]
  widgetAlign = vstack [
      hstack [
        label "Label 1 - ja - ^&~@$" `style` [textSize 10, textBottom],
        label "Label 2 - ja - ^&~@$" `style` [textSize 20, textBottom],
        label "Label 3 - ja - ^&~@$" `style` [textSize 40, textBottom]
      ] `style` [bgColor orange]
    ]
  --widgetDialSingle = dial double1 (-100) 100
  widgetRow = hstack [
      vstack [
        label "Label 1",
        label "Label 2"
      ],
      filler,
      image "assets/images/pecans.jpg" `style` [width 50, height 50]
    ]
  widgetDial = vstack [
      textDropdown_ dropdown1 items id [],
      tooltip "Hello!\nThis is a long message, that will hopefully be split into several lines" $ label "Test",
      hstack [
        radioV (model ^. fruit) RadioSt Apple,
        radioV (model ^. fruit) RadioSt Orange,
        radioV (model ^. fruit) RadioSt Pear
      ] `key` "radio hstack",
      textField textField1,
      hstack [
        checkbox condition1,
        checkbox_ condition2 [checkboxMark CheckboxTimes]
      ],
      numericField_ rational1 [minValue (-100), maxValue 100],
      tooltip "Hello!\nThis is a long message, that will hopefully be split into several lines" $ label "Test",
      box_ [expandContent] widgetRow `hover` [bgColor gray, cursorIcon CursorHand],
      dial rational1 (-100) 100,
      hstack [
        button "Test" RunShortTask,
        mainButton "Test" RunShortTask
      ],
      image "assets/images/pecans.jpg",
      tooltip "Hello!\nThis is a long message, that will hopefully be split into several lines" (label "Test" `style` [border 1 pink]) `style` [bgColor orange, textSize 20]
    ]
  widgetSplit = hsplit (button "Button" RunShortTask, button "Button!!!" RunShortTask)
  widgetSplitH = keystroke [("C-a", ShowAlert), ("C-c", ShowConfirm), ("C-S-p", ShowConfirm)] $ hsplit (image "assets/images/pecans.jpg", widgetTree)
  widgetSplitV = vsplit (image "assets/images/pecans.jpg" `style` [rangeHeight 200 400], widgetTree `style` [rangeHeight 200 400])
  mkImg i = vstack [
      label ("Image: " <> showt i),
      image ("https://picsum.photos/600/400?ts=" <> showt i)
    ]
{--
  widgetImages = scroll $ vstack (mkImg <$> [0..9::Int])
  widgetSave = vstack [
      textField textField1,
      textDropdown dropdown1 items,
      textDropdown dropdown1 items,
      textDropdown dropdown1 items,
      textDropdown dropdown1 items,
      textDropdown dropdown1 items,
      scroll $ image_ "assets/images/pecans.jpg" [fitFill] `style` [width 1000, height 1000]
    ]
  widgetHover = vstack [
      hstack [
        label "Test" `hover` [bgColor red, textSize 32],
        label "Test" `hover` [bgColor green],
        textField textField1 `hover` [bgColor orange, textSize 32]
      ],
      textDropdown dropdown1 items,
      vstack $ fmap (\i -> label ("AAAA: " <> showt i) `hover` [textSize 40]) [1..10::Int],
      listView dropdown1 items label
    ]
  widgetIdChanged = vstack [
      button "Show label" IncButton,
      hstack $ [label "First" | model ^. clickCount > 0] ++ [
        label "Test",
        image_ "https://picsum.photos/600/400" [fitFill, onLoadError ImageMsg]
      ]
    ]
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
        numericField_ integer1 [minValue 0],
        label ""
      ] `style` [padding 5, paddingT 0],
      hgrid [
        label "Floating: ",
        numericField_ float1 [minValue (-20), maxValue 20],
        label ""
      ] `style` [padding 5, paddingT 0]
    ]
  widgetLV = vstack [
      scroll $ vstack $ (\i -> box $ label ("Label: " <> showt i)) <$> [0..1000::Int]
      label "aaa"
      , listView dropdown1 items label `style` [height 300]
      , scroll $ image "assets/images/pecans.jpg"
      , dropdown_ dropdown1 items label label [maxHeight 200]
    ]
--}
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
    | even (model ^. clickCount) = widgetTree10
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
  widgetTree5 = zstack_ [onlyTopActive False] [
      hgrid [
        vstack [
          label "test",
          button "Test" IncButton,
          textField textField1 `style` [bgColor blue],
          textField textField1 `style` [bgColor pink],
          textField textField1 `style` [bgColor orange]
        ]
      ],
      box_ [alignRight] $
        hstack [
          vgrid [
            label "",
            textField textField1 `style` [bgColor lightBlue, width 200]
          ]
        ] `style` [height 480]
    ]
{--
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
--}
  widgetTree3 = hgrid [
      label "Hi!\nThis\nis\na\nnew\ttest\n\n  Double!" `style` [bgColor pink, textBottom, textCenter],
      vgrid [
        label "1",
        label "2",
        label "3",
        label "4",
        label "5",
        label_ "This is a really long label used to check if line breaks and ellipsis are implemented correctly" [multiLine] `style` [bgColor blue],
        label "6",
        label "This is a really long label used to check if line breaks and ellipsis are implemented correctly, using a longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong invalid word" `style` [bgColor blue, textBottom, textRight]
      ],
      label "",
      label ""
    ]
{--
  widgetTree2 = zstack [
      vstack [
        hstack [
          radio fruit Apple,
          radio fruit Orange,
          checkbox condition1
        ],
        dropdown_ dropdown1 items label label [maxHeight 200],
        label "Integral",
        numericField integer1,
        label "Floating",
        numericField float1,
        --image "assets/images/pecans.jpg"
        listView_ dropdown1 items label [],
        --dropdown dropdown1 items id label
        label "Text",
        textField textField1
      ]-- `style` [padding 10]
    ]
--}
  longMessage word = "Are you sure?\n\n\n\n" <> T.replicate 100 (word <> " ")
  widgetTree = zstack [
      widgetTreeFull,
      alertMsg_ (longMessage "Alert") CloseAlert [titleCaption "Hey!"] `visible` model ^. showAlert,
      confirmMsg_ (longMessage "Confirm") AcceptConfirm CancelConfirm [titleCaption "Hey!"] `visible` model ^. showConfirm
    ]
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
      textDropdown_ dropdown1 items id [onChange DropdownVal, onChangeIdx DropdownIdx],
      hgrid [
        vstack [
          label "jLabel 1" `style` [bgColor darkGray] `hover` [textSize 40],
          label "Label 12" `style` [bgColor lightGray] `hover` [textSize 40],
          label "Label 123" `style` [bgColor darkGray] `hover` [textSize 40],
          label "Label 1234" `style` [bgColor lightGray] `hover` [textSize 40]
        ] `style` [bgColor red],
        vstack [
          label "jLabel 1" `style` [bgColor lightGray, textBottom],
          label "Label 12" `style` [bgColor darkGray],
          label "Label 123" `style` [bgColor lightGray],
          label "Label 1234" `style` [bgColor darkGray]
        ] `style` [bgColor blue]
      ] `style` [bgColor green],
      hgrid [
        label_ "This is a really long label used to check what I did works fine" [multiLine, ellipsis],
        label "Jj label" `hover` [textSize 40]
      ] `hover` [bgColor red],
      label (model ^. dropdown1) `style` [bgColor lightBlue, textLeft],
      textField textField1 `style` [bgColor lightBlue, textLeft],
      hstack [
          scroll_ [] $ image_ "assets/images/pecans.jpg" [fitFill],
          scroll_ [] $ image_ "assets/images/pecans.jpg" [fitFill],
          scroll_ [] $ image_ "assets/images/pecans.jpg" [fitFill],
          image_ "https://picsum.photos/1600/400" [fitFill, onLoadError ImageMsg] `style` [cursorIcon CursorInvalid]
        ],
      textDropdown_ dropdown1 items id [onChange DropdownVal, onChangeIdx DropdownIdx],
      button_ "Click\nme!" (PrintMessage "Button clicked") [] --multiLine, ellipsis
    ] `key` "main vstack" `style` [borderT 20 red, borderL 10 blue, borderR 10 green, borderB 10 gray, iradius 50] --, padding 20
  items = fmap (\i -> "This is a long label: " <> showt i) [1..100::Int]
