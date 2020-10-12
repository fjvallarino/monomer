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
  let theme :: Theme = def
        & L.basic . L.fgColor .~ blue
        & L.hover . L.fgColor .~ white
        & L.focus . L.fgColor .~ white
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

handleAppEvent model evt = case evt of
  IncButton -> Model (model & clickCount %~ (+1))
  PrintMessage txt -> Task $ do
    print txt
    return Nothing
  CheckboxSt st -> Task $ do
    putStrLn $ "Checkbox is: " ++ show st
    return Nothing
--  RadioSt st -> Task $ do
--    putStrLn $ "Radio is: " ++ show st
--    return Nothing
  RadioSt st -> Model (model & fruit .~ st)
  ImageMsg msg -> Task $ do
    putStrLn $ "Image msg is: " ++ show msg
    return Nothing
  _ -> Model model

buildUI model = trace "Creating UI" widgetTree where
  widgetTree3 = vstack [
      label "Test" `style` [height 450],
      textDropdown textField2 items id `style` [bgColor lightBlue]
    ]
  widgetTree1 = textField textField1 `style` [bgColor lightBlue, textLeft]
  widgetTree = vstack [
      --hstack [
      --  radioV (model ^. fruit) RadioSt Apple,
      --  radioV (model ^. fruit) RadioSt Orange,
      --  radioV (model ^. fruit) RadioSt Pear
      --] `key` "radio hstack" `style` [bgColor gray],
      hgrid [
        vstack [
          label "jLabel 1" `style` [bgColor darkGray],
          label "Label 12" `style` [bgColor lightGray],
          label "Label 12" `style` [bgColor darkGray],
          label "Label 12" `style` [bgColor lightGray],
          label "Label 123" `style` [bgColor darkGray],
          label "Label 1234" `style` [bgColor lightGray]
        ],
        vstack [
          label "jLabel 1" `style` [bgColor lightGray, textBottom],
          label "Label 12" `style` [bgColor darkGray, textTop],
          label "Label 12" `style` [bgColor lightGray],
          label "Label 12" `style` [bgColor darkGray],
          label "Label 123" `style` [bgColor lightGray],
          label "Label 1234" `style` [bgColor darkGray]
        ]
      ],
      textField textField1 `style` [bgColor lightBlue, textLeft],
      hgrid [
        label_ "This is a really long label used to check what I did works fine" [textEllipsis],
        label "Jj label"
      ],
      hstack [
        label "test" `style` [bgColor gray]
      ],
      hstack [
        image_ "assets/images/pecans.jpg" [fitFill] `style` [minWidth 200],
        spacer_ [resizeFactor 1],
        image_ "https://picsum.photos/600/400" [fitFill, onLoadError ImageMsg]
      ],
      hstack [
        label "Test"
      ] `key` "label hstack" `style` [bgColor darkGray],
      textDropdown_ textField2 items id [bgColor red, bgColor green] `style` [bgColor lightBlue],
      button "Click me" (PrintMessage "Button clicked")
    ] `key` "main vstack" `style` [borderT 20 red, borderL 10 blue, borderR 10 green, borderB 10 gray, iradius 50] --, padding 20
  newLabel i = label ("New: " <> showt i) `style` [altColor i]
  altColor i = bgColor (if even i then gray else darkGray)
  labels = newLabel <$> [0..(model ^. clickCount - 1)]
  items = fmap showt [1..100::Int]
