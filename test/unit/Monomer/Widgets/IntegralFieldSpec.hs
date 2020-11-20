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
import Monomer.Event
import Monomer.TestUtil
import Monomer.TestKeyboardUtil
import Monomer.Widgets.IntegralField

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
    basicIntInst = integralField integralValue
    intCfg = [maxValue 1501, selectOnFocus True, validInput integralValid, onFocus GotFocus, onBlur LostFocus]
    intInst = integralField_ integralValue intCfg
    model es = instHandleEventModel wenv (Focus : es) intInst
    modelBasic es = instHandleEventModel wenv es basicIntInst
    events evt = instHandleEventEvts wenv [evt] intInst

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
    intInst = integralFieldV_ 0 NumberChanged [maxValue 2345, selectOnFocus True, validInput integralValid]
    evts es = instHandleEventEvts wenv (Focus : es) intInst
    model es = instHandleEventModel wenv (Focus : es) intInst
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
    (sizeReqW, sizeReqH) = instUpdateSizeReq wenv (integralField integralValue)
