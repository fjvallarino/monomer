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
  let theme = def
        & L.basic . L.fgColor .~ blue
        & L.hover . L.fgColor .~ white
        & L.focus . L.fgColor .~ white
  let config = [
        windowSize (1280, 960),
        useHdpi True,
        fontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
        fontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
        fontDef "Italic" "./assets/fonts/Roboto-Italic.ttf" ]

  simpleApp_ model (Just InitApp) theme handleAppEvent buildUI config

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
  _ -> Model model

buildUI model = trace "Creating UI" widgetTree where
  widgetTree = vstack [
      hstack [
        radioV (model ^. fruit) RadioSt Apple,
        radioV (model ^. fruit) RadioSt Orange,
        radioV (model ^. fruit) RadioSt Pear
      ] `key` "radio hstack" `style` [bgColor gray],
      vstack [
        hstack [label "Label 1", box_ (checkbox condition1) [] `style` [width 100, bgColor darkGray]],
        hstack [label "Label 12", box_ (checkbox condition2) [alignLeft] `style` [width 100, bgColor lightGray]],
        hstack [label "Label 123", box_ (checkbox condition3) [alignRight, alignBottom] `style` [width 100, bgColor darkGray]],
        hstack [label "Label 1234", box_ (checkbox_ condition1 [onChange CheckboxSt]) [] `style` [width 100, bgColor lightGray]]
      ],
      hgrid [
        label_ "This is a really long label used to check what I did works fine" [textEllipsis],
        label "Short label"
      ],
      hstack [
        label "test" `style` [bgColor gray]
      ],
      hstack [
        image_ "assets/images/pecans.jpg" [fitFill] `style` [minWidth 200],
        spacer_ [resizeFactor 1],
        image_ "https://picsum.photos/600/400" [fitFill]
      ],
      hstack [
        label "Test"
      ] `key` "label hstack" `style` [bgColor darkGray],
      button "Click me" (PrintMessage "Button clicked")
    ] `key` "main vstack" `style` [borderT 20 red, borderL 10 blue, borderR 10 green, borderB 10 gray, iradius 50] --, padding 20
  newLabel i = label ("New: " <> showt i) `style` [altColor i]
  altColor i = bgColor (if even i then gray else darkGray)
  labels = newLabel <$> [0..(model ^. clickCount - 1)]
  items = fmap showt [1..100::Int]
