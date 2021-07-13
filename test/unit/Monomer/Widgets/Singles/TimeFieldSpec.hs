{-|
Module      : Monomer.Widgets.Singles.TimeFieldSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for TimeField widget.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Singles.TimeFieldSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Data.Text (Text)
import Data.Time
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestUtil
import Monomer.TestEventUtil
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.Button
import Monomer.Widgets.Singles.TimeField

import qualified Monomer.Lens as L

data TestEvt
  = TimeChanged TimeOfDay
  | TimeValid Bool
  | GotFocus Path
  | LostFocus Path
  deriving (Eq, Show)

data TimeModel = TimeModel {
  _imTimeValue :: TimeOfDay,
  _imTimeValid :: Bool
} deriving (Eq, Show)

instance Default TimeModel where
  def = TimeModel {
    _imTimeValue = midnight,
    _imTimeValid = True
  }

data MTimeModel = MTimeModel {
  _imMTimeValue :: Maybe TimeOfDay,
  _imMTimeValid :: Bool
} deriving (Eq, Show)

instance Default MTimeModel where
  def = MTimeModel {
    _imMTimeValue = Nothing,
    _imMTimeValid = True
  }

makeLensesWith abbreviatedFields ''TimeModel
makeLensesWith abbreviatedFields ''MTimeModel

spec :: Spec
spec = describe "TimeField" $ do
  handleEventTime
  handleEventValueTime
  handleEventMouseDragTime
  getSizeReqTime

handleEventTime :: Spec
handleEventTime = describe "handleEventTime" $ do
  it "should remove the contents and get Nothing as model value" $ do
    modelBasic [evtKG keyA, evtK keyBackspace] ^. mTimeValue `shouldBe` Nothing
    modelBasic [evtKG keyA, evtK keyBackspace] ^. mTimeValid `shouldBe` True

  it "should input '01:30'" $ do
    modelBasic [evtKG keyA, evtT "01", evtT ":30"] ^. mTimeValue `shouldBe` Just (TimeOfDay 1 30 0)
    modelBasic [evtKG keyA, evtT "01", evtT ":30"] ^. mTimeValid `shouldBe` True

  it "should input '1' (invalid time)" $ do
    model [evtKG keyA, evtT "1"] ^. mTimeValue `shouldBe` Just midTime
    model [evtKG keyA, evtT "1"] ^. mTimeValid `shouldBe` False
    events [evtKG keyA, evtT "1"] `shouldBe` Seq.fromList [TimeValid False]

  it "should input '02:35' but fail because of minValue" $ do
    model [evtT "02:35"] ^. mTimeValue `shouldBe` Just midTime
    model [evtT "02:35"] ^. mTimeValid `shouldBe` False
    events [evtT "02:35"] `shouldBe` Seq.fromList [TimeValid False]

  it "should input '23:50' but fail because of maxValue" $ do
    model [evtT "23:50"] ^. mTimeValue `shouldBe` Just midTime
    model [evtT "23:50"] ^. mTimeValid `shouldBe` False
    events [evtT "23:50"] `shouldBe` Seq.fromList [TimeValid False]

  it "should remove one character and input '4'" $ do
    model [moveCharR, delCharL, evtT "4"] ^. mTimeValue `shouldBe` Just (TimeOfDay 14 24 0)
    model [moveCharR, delCharL, evtT "4"] ^. mTimeValid `shouldBe` True
    events [moveCharR, delCharL, evtT "4"] `shouldBe` Seq.fromList [TimeValid True, TimeValid True]

  it "should input '22:30', remove one word and input '15'" $ do
    model [evtT "22:30", delWordL, evtT "15"] ^. mTimeValue `shouldBe` Just (TimeOfDay 22 15 0)
    model [evtT "22:30", delWordL, evtT "15"] ^. mTimeValid `shouldBe` True
    events [evtT "22:30", delWordL, evtT "15"] `shouldBe` Seq.fromList [TimeValid True, TimeValid False, TimeValid True]

  it "should update the model when using the wheel" $ do
    let p = Point 100 10
    let steps1 = [WheelScroll p (Point 0 (-2000)) WheelNormal]
    let steps2 = [WheelScroll p (Point 0 (-64)) WheelNormal]
    let steps3 = [WheelScroll p (Point 0 64) WheelNormal]
    let steps4 = [WheelScroll p (Point 0 (-2000)) WheelFlipped]
    model steps1 ^. mTimeValue `shouldBe` Just minTime
    model steps2 ^. mTimeValue `shouldBe` Just (TimeOfDay 13 16 0)
    model steps3 ^. mTimeValue `shouldBe` Just (TimeOfDay 15 24 0)
    model steps4 ^. mTimeValue `shouldBe` Just maxTime

  it "should generate an event when focus is received" $
    events [evtFocus] `shouldBe` Seq.singleton (GotFocus emptyPath)

  it "should generate an event when focus is lost" $
    events [evtBlur] `shouldBe` Seq.singleton (LostFocus emptyPath)

  where
    minTime = TimeOfDay 3 15 0
    midTime = TimeOfDay 14 20 0
    maxTime = TimeOfDay 23 45 0
    wenv = mockWenv (MTimeModel (Just midTime) True)
    basicTimeNode :: WidgetNode MTimeModel TestEvt
    basicTimeNode = timeField_ mTimeValue [validInput mTimeValid, selectOnFocus_ False]
    timeCfg = [minValue (Just minTime), maxValue (Just maxTime), validInput mTimeValid, validInputV TimeValid, onFocus GotFocus, onBlur LostFocus]
    timeNode = timeField_ mTimeValue timeCfg
    model es = nodeHandleEventModel wenv (evtFocus : es) timeNode
    modelBasic es = nodeHandleEventModel wenv es basicTimeNode
    events es = nodeHandleEventEvts wenv es timeNode

handleEventValueTime :: Spec
handleEventValueTime = describe "handleEventTime" $ do
  it "should input an '12:07:48'" $
    evts [evtT "12:07:48"] `shouldBe` Seq.fromList [TimeChanged (TimeOfDay 12 07 48)]

  it "should move right, delete one character and input '5'" $ do
    let steps = [moveCharR, delCharL, evtT "5"]
    lastEvt steps `shouldBe` TimeChanged (TimeOfDay 13 35 25)
    model steps ^. timeValid `shouldBe` True

  it "should move right and delete one word" $ do
    let steps = [moveCharR, delWordL]
    evts steps `shouldBe` Seq.empty
    model steps ^. timeValid `shouldBe` False

  it "should input '04:20:', input '.', 'a', then input '10'" $ do
    let steps = [evtT "04:20:", evtT ".", evtT "a", evtT "10"]
    lastEvt steps `shouldBe` TimeChanged lowTime
    model steps ^. timeValid `shouldBe` True

  it "should input '22:5644:4555'" $ do
    let steps = [evtT "22", evtT ":56", evtT "44", evtT ":45", evtT "55"]
    lastEvt steps `shouldBe` TimeChanged maxTime

  where
    minTime = TimeOfDay 1 35 30
    lowTime = TimeOfDay 4 20 10
    midTime = TimeOfDay 13 35 20
    maxTime = TimeOfDay 22 56 45
    wenv = mockWenv (TimeModel minTime True)
    timeNode = timeFieldV_ midTime TimeChanged [minValue minTime, maxValue maxTime, selectOnFocus, validInput timeValid, timeFormatHHMMSS]
    evts es = nodeHandleEventEvts wenv (evtFocus : es) timeNode
    model es = nodeHandleEventModel wenv (evtFocus : es) timeNode
    lastIdx es = Seq.index es (Seq.length es - 1)
    lastEvt es = lastIdx (evts es)

handleEventMouseDragTime :: Spec
handleEventMouseDragTime = describe "handleEventMouseDragTime" $ do
  it "should drag upwards 100 pixels, setting the value to 18:50:15" $ do
    let selStart = Point 50 50
    let selEnd = Point 50 (-70)
    let steps = [evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. timeValue `shouldBe` TimeOfDay 18 50 15

  it "should drag downwards 100 pixels, setting the value to 11:30:15 (dragRate = 2)" $ do
    let selStart = Point 50 50
    let selEnd = Point 50 150
    let steps = [evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. timeValue `shouldBe` TimeOfDay 11 30 15

  it "should drag downwards 10000 pixels, staying at minTime (the minimum)" $ do
    let selStart = Point 50 50
    let selEnd = Point 50 10050
    let steps = [evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. timeValue `shouldBe` minTime

  it "should drag upwnwards 10000 pixels, staying at maxTime (the maximum)" $ do
    let selStart = Point 50 50
    let selEnd = Point 50 (-1950)
    let steps = [evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. timeValue `shouldBe` maxTime

  it "should drag downwards 30 and 20 pixels, setting the value to 14:30:15" $ do
    let selStart = Point 50 50
    let selMid = Point 50 60
    let selEnd = Point 50 50
    let steps = [
          evtPress selStart, evtMove selMid, evtRelease selMid,
          evtPress selStart, evtMove selEnd, evtRelease selEnd
          ]
    model steps ^. timeValue `shouldBe` TimeOfDay 14 30 15

  it "should set focus and drag upwards 100 pixels, but value stay at midTime since shift is not pressed" $ do
    let selStart = Point 50 50
    let selEnd = Point 50 (-70)
    let steps = [evtK keyTab, evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. timeValue `shouldBe` midTime

  it "should set focus and drag upwards 100 pixels, setting the value to 18:50:15 since shift is pressed" $ do
    let selStart = Point 50 50
    let selEnd = Point 50 (-70)
    let steps = [evtKS keyTab, evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. timeValue `shouldBe` TimeOfDay 18 50 15

  it "should drag upwards 100 pixels, setting the value to 18:50:15 even if it was double clicked on" $ do
    let selStart = Point 50 50
    let selEnd = Point 50 (-70)
    let steps = [evtDblClick selStart, evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. timeValue `shouldBe` TimeOfDay 18 50 15

  it "should generate a focus event when clicked" $ do
    let p = Point 50 50
    -- Presses a key to reset the shift = true state
    evts [evtK keyA, evtPress p] `shouldBe` Seq.singleton (GotFocus (Seq.fromList [0, 0]))

  it "should not generate a focus event when clicked if shift is pressed" $ do
    let p = Point 50 50
    evts [evtKS keyA, evtPress p] `shouldBe` Seq.empty

  where
    minTime = TimeOfDay 01 20 30
    midTime = TimeOfDay 14 50 15
    maxTime = TimeOfDay 23 40 50
    wenv = mockWenv (TimeModel midTime True)
      & L.inputStatus . L.keyMod . L.leftShift .~ True
    timeNode = vstack [
        button "Test" (TimeChanged midTime), -- Used only to have focus
        timeField timeValue,
        timeField_ timeValue [dragRate 2, minValue minTime, maxValue maxTime, timeFormatHHMMSS, onFocus GotFocus]
      ]
    evts es = nodeHandleEventEvts wenv es timeNode
    model es = nodeHandleEventModel wenv es timeNode
    lastIdx es = Seq.index es (Seq.length es - 1)
    lastEvt es = lastIdx (evts es)

getSizeReqTime :: Spec
getSizeReqTime = describe "getSizeReqTime" $ do
  it "should return width = Flex 160 1" $
    sizeReqW `shouldBe` expandSize 160 1

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` fixedSize 20

  it "should return width = Flex 100 1 when resizeOnChange = True" $
    sizeReqW2 `shouldBe` expandSize 100 1

  it "should return height = Fixed 20 when resizeOnChange = True" $
    sizeReqH2 `shouldBe` fixedSize 20

  where
    wenv = mockWenvEvtUnit (def :: TimeModel)
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv (timeField timeValue)
    timeResize = timeField_ timeValue [resizeOnChange]
    (sizeReqW2, sizeReqH2) = nodeGetSizeReq wenv timeResize
