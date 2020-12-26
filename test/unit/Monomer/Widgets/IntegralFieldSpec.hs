{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.IntegralFieldSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestUtil
import Monomer.TestEventUtil
import Monomer.Widgets.Button
import Monomer.Widgets.IntegralField
import Monomer.Widgets.Stack

import qualified Monomer.Lens as L

data TestEvt
  = NumberChanged Int
  | GotFocus
  | LostFocus
  deriving (Eq, Show)

data TestModel = TestModel {
  _tmIntegralValue :: Int,
  _tmIntegralValid :: Bool
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''TestModel

spec :: Spec
spec = describe "IntegralField" $ do
  handleEvent
  handleEventValue
  handleEventMouseDrag
  updateSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should input '123' without select on focus" $ do
    modelBasic [evtT "1", evtT "2", evtT "3"] ^. integralValue `shouldBe` 1230
    modelBasic [evtT "1", evtT "2", evtT "3"] ^. integralValid `shouldBe` True

  it "should input '1'" $ do
    model [evtT "1"] ^. integralValue `shouldBe` 1
    model [evtT "1"] ^. integralValid `shouldBe` True

  it "should input '-1'" $ do
    model [evtT "-1"] ^. integralValue `shouldBe` -1
    model [evtT "-1"] ^. integralValid `shouldBe` True

  it "should input '1501'" $ do
    model [evtT "1", evtT "5", evtT "0", evtT "1"] ^. integralValue `shouldBe` 1501
    model [evtT "1", evtT "5", evtT "0", evtT "1"] ^. integralValid `shouldBe` True

  it "should input '1502', but fail because of maxValue" $ do
    model [evtT "1", evtT "5", evtT "0", evtT "2"] ^. integralValue `shouldBe` 150
    model [evtT "1", evtT "5", evtT "0", evtT "2"] ^. integralValid `shouldBe` False

  it "should input '123', remove one character and input '4'" $ do
    model [evtT "123", delCharL, evtT "4"] ^. integralValue `shouldBe` 124
    model [evtT "123", delCharL, evtT "4"] ^. integralValid `shouldBe` True

  it "should input '123', remove one word and input '456'" $ do
    model [evtT "123", delWordL, evtT "456"] ^. integralValue `shouldBe` 456
    model [evtT "123", delWordL, evtT "456"] ^. integralValid `shouldBe` True

  it "should generate an event when focus is received" $
    events Focus `shouldBe` Seq.singleton GotFocus

  it "should generate an event when focus is lost" $
    events Blur `shouldBe` Seq.singleton LostFocus

  where
    wenv = mockWenv (TestModel 0 True)
    basicIntNode = integralField_ integralValue [selectOnFocus False]
    intCfg = [maxValue 1501, validInput integralValid, onFocus GotFocus, onBlur LostFocus]
    intNode = integralField_ integralValue intCfg
    model es = nodeHandleEventModel wenv (Focus : es) intNode
    modelBasic es = nodeHandleEventModel wenv es basicIntNode
    events evt = nodeHandleEventEvts wenv [evt] intNode

handleEventValue :: Spec
handleEventValue = describe "handleEvent" $ do
  it "should input an '10'" $
    evts [evtT "1", evtT "0"] `shouldBe` Seq.fromList [NumberChanged 1, NumberChanged 10]

  it "should input '1', move to beginning and input '5'" $ do
    let steps = [evtT "1", moveLineL, evtT "5"]
    lastEvt steps `shouldBe` NumberChanged 51

  it "should input '1', input '.' then input '5'" $ do
    let steps = [evtT "1", evtT ".", evtT "5"]
    lastEvt steps `shouldBe` NumberChanged 15
    model steps ^. integralValid `shouldBe` True

  it "should input '3', input 'a' then input '6'" $ do
    let steps = [evtT "3", evtT "a", evtT "6"]
    lastEvt steps `shouldBe` NumberChanged 36
    model steps ^. integralValid `shouldBe` True

  it "should input '1234', delete line then input '777'" $ do
    let steps = [evtT "1234", selLineL, evtT "777"]
    lastEvt steps `shouldBe` NumberChanged 777
    model steps ^. integralValid `shouldBe` True

  where
    wenv = mockWenv (TestModel 0 False)
    intNode = integralFieldV_ 0 NumberChanged [maxValue 2345, selectOnFocus True, validInput integralValid]
    evts es = nodeHandleEventEvts wenv (Focus : es) intNode
    model es = nodeHandleEventModel wenv (Focus : es) intNode
    lastIdx es = Seq.index es (Seq.length es - 1)
    lastEvt es = lastIdx (evts es)

handleEventMouseDrag :: Spec
handleEventMouseDrag = describe "handleEventMouseDrag" $ do
  it "should drag upwards 100 pixels, setting the value to 100" $ do
    let str = "This is text"
    let selStart = Point 50 30
    let selEnd = Point 50 (-70)
    let steps = [evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. integralValue `shouldBe` 100

  it "should drag downwards 100 pixels, setting the value to -200 (dragRate = 2)" $ do
    let str = "This is text"
    let selStart = Point 50 50
    let selEnd = Point 50 150
    let steps = [evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. integralValue `shouldBe` -200

  it "should drag downwards 30 and 20 pixels, setting the value to -5" $ do
    let str = "This is text"
    let selStart = Point 50 30
    let selMid = Point 50 60
    let selEnd = Point 50 50
    let steps = [
          evtPress selStart, evtMove selMid, evtRelease selMid,
          evtPress selStart, evtMove selEnd, evtRelease selEnd
          ]
    model steps ^. integralValue `shouldBe` -50

  it "should drag upwards 100 pixels, but value stay at 0 since it has focus" $ do
    let str = "This is text"
    let selStart = Point 50 30
    let selEnd = Point 50 (-70)
    let steps = [evtK keyTab, evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. integralValue `shouldBe` 0

  it "should drag upwards 100 pixels, but value stay at 0 since it was double clicked on" $ do
    let str = "This is text"
    let selStart = Point 50 30
    let selEnd = Point 50 (-70)
    let steps = [evtDblClick selStart, evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. integralValue `shouldBe` 0

  where
    wenv = mockWenv (TestModel 0 False)
    integralNode = vstack [
        button "Test" (NumberChanged 0), -- Used only to have focus
        integralField integralValue,
        integralField_ integralValue [dragRate 2]
      ]
    evts es = nodeHandleEventEvts wenv es integralNode
    model es = nodeHandleEventModel wenv es integralNode
    lastIdx es = Seq.index es (Seq.length es - 1)
    lastEvt es = lastIdx (evts es)

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  it "should return width = Flex 110 1" $
    sizeReqW `shouldBe` FlexSize 110 1

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` FixedSize 20

  where
    wenv = mockWenvEvtUnit (TestModel 10000000000 True)
    (sizeReqW, sizeReqH) = nodeUpdateSizeReq wenv (integralField integralValue)
