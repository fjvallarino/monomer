{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

import Control.Concurrent (threadDelay)
import Control.Lens ((&), (^.), (.~), (%~))
import Data.Default
import TextShow

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
        useHdpi True,
        appTheme theme,
        appInitEvent InitApp,
        fontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
        fontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
        fontDef "Italic" "./assets/fonts/Roboto-Italic.ttf" ]

  simpleApp_ model handleAppEvent buildUI config
  --simpleApp model handleAppEvent buildUI

handleAppEvent :: App -> AppEvent -> [AppEventResponse App AppEvent]
handleAppEvent model evt = case evt of
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
    return Nothing]
  _ -> []

buildUI :: App -> WidgetInstance App AppEvent
buildUI model = trace "Creating UI" widgetTree3 where
  widgetTree3 = hgrid [
      label "Hola\nEsta\nes\nuna\nnueva\tprueba\n\n  Doble!" `style` [bgColor pink, textBottom, textCenter],
      label "Este es un label bien largo para ver si se cortan correctamentereallyverybien las lineas" `style` [bgColor blue, textBottom, textRight],
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
        dropdown_ dropdown1 items id label [maxHeight 200],
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
  widgetTree = zstack [
      widgetTreeFull,
      alert "Message" CloseAlert `visible` model ^. showAlert,
      confirm "Message" AcceptConfirm CancelConfirm `visible` model ^. showConfirm
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
      label (model ^. textField1) `style` [bgColor lightBlue, textLeft],
      textField textField1 `style` [bgColor lightBlue, textLeft],
      hgrid [
        label_ "This is a really long label used to check what I did works fine" [textEllipsis],
        label "Jj label"
      ],
      hstack [
        scroll_ (
          scroll (image_ "assets/images/pecans.jpg" [fitFill] `style` [minWidth 200])
        ) []
        ,
        spacer_ [resizeFactor 1],
        image_ "https://picsum.photos/600/400" [fitFill, onLoadError ImageMsg]
      ],
      textDropdown_ textField1 items id [onChange DropdownVal, onChangeIdx DropdownIdx],
      button "Click me" (PrintMessage "Button clicked")
    ] `key` "main vstack" `style` [borderT 20 red, borderL 10 blue, borderR 10 green, borderB 10 gray, iradius 50] --, padding 20
  items = fmap showt [1..100::Int]
