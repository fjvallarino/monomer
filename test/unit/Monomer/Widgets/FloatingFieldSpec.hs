{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.FloatingFieldSpec (spec) where

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
import Monomer.Widgets.FloatingField

import qualified Monomer.Lens as L

data TestEvt
  = NumberChanged Double
  | GotFocus
  | LostFocus
  deriving (Eq, Show)

data TestModel = TestModel {
  _tmFloatingValue :: Double,
  _tmFloatingValid :: Bool
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''TestModel

spec :: Spec
spec = describe "FloatingField" $ do
  handleEvent
  handleEventValue
  updateSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should input '123' without select on focus" $ do
    modelBasic [evtT "1", evtT "2", evtT "3"] ^. floatingValue `shouldBe` 1230
    modelBasic [evtT "1", evtT "2", evtT "3"] ^. floatingValid `shouldBe` True

  it "should input '1.23'" $ do
    model [evtT "1.23"] ^. floatingValue `shouldBe` 1.23
    model [evtT "1.23"] ^. floatingValid `shouldBe` True

  it "should input '-1'" $ do
    model [evtT "-1"] ^. floatingValue `shouldBe` -1
    model [evtT "-1"] ^. floatingValid `shouldBe` True

  it "should input '1501'" $ do
    model [evtT "1", evtT "5", evtT "0", evtT "1"] ^. floatingValue `shouldBe` 1501
    model [evtT "1", evtT "5", evtT "0", evtT "1"] ^. floatingValid `shouldBe` True

  it "should input '1502', but fail because of maxValue" $ do
    model [evtT "1", evtT "5", evtT "0", evtT "2"] ^. floatingValue `shouldBe` 150
    model [evtT "1", evtT "5", evtT "0", evtT "2"] ^. floatingValid `shouldBe` False

  it "should input '123', remove one character and input '4'" $ do
    model [evtT "123", delCharL, evtT "4"] ^. floatingValue `shouldBe` 124
    model [evtT "123", delCharL, evtT "4"] ^. floatingValid `shouldBe` True

  it "should input '123', remove one word and input '456'" $ do
    model [evtT "123", delWordL, evtT "456"] ^. floatingValue `shouldBe` 456
    model [evtT "123", delWordL, evtT "456"] ^. floatingValid `shouldBe` True

  it "should input '123.34', remove one word and input '56'" $ do
    model [evtT "123.34", delWordL, evtT "56"] ^. floatingValue `shouldBe` 123.56
    model [evtT "123.34", delWordL, evtT "56"] ^. floatingValid `shouldBe` True

  it "should input '123.34', remove two words and input '56'" $ do
    model [evtT "123.34", delWordL, delWordL, evtT "56"] ^. floatingValue `shouldBe` 56
    model [evtT "123.34", delWordL, delWordL, evtT "56"] ^. floatingValid `shouldBe` True

  it "should generate an event when focus is received" $
    events Focus `shouldBe` Seq.singleton GotFocus

  it "should generate an event when focus is lost" $
    events Blur `shouldBe` Seq.singleton LostFocus

  where
    wenv = mockWenv (TestModel 0 True)
    basicFloatingNode = floatingField floatingValue
    floatCfg = [maxValue 1501, selectOnFocus True, validInput floatingValid, onFocus GotFocus, onBlur LostFocus]
    floatNode = floatingField_ floatingValue floatCfg
    model es = nodeHandleEventModel wenv (Focus : es) floatNode
    modelBasic es = nodeHandleEventModel wenv es basicFloatingNode
    events evt = nodeHandleEventEvts wenv [evt] floatNode

handleEventValue :: Spec
handleEventValue = describe "handleEvent" $ do
  it "should input an '100'" $
    evts [evtT "1", evtT "0", evtT "0"] `shouldBe` Seq.fromList [NumberChanged 10, NumberChanged 100]

  it "should input a '1' and be considered invalid" $ do
    evts [evtT "1"] `shouldBe` Seq.fromList []
    model [evtT "1"] ^. floatingValid `shouldBe` False

  it "should input '1', move to beginning and input '5'" $ do
    let steps = [evtT "1", moveLineL, evtT "5"]
    lastEvt steps `shouldBe` NumberChanged 51

  it "should input '1', input '.' then input '5'" $ do
    let steps = [evtT "10", evtT ".", evtT "5"]
    lastEvt steps `shouldBe` NumberChanged 10.5
    model steps ^. floatingValid `shouldBe` True

  it "should input '20', input '.' twice then input '777'" $ do
    let steps = [evtT "20", evtT ".", evtT ".", evtT "7", evtT "7", evtT "7"]
    lastEvt steps `shouldBe` NumberChanged 20.77
    model steps ^. floatingValid `shouldBe` True

  it "should input '10', '.' then input '2345'" $ do
    let steps = [evtT "10", evtT ".", evtT "2", evtT "3", evtT "4", evtT "5"]
    lastEvtDecimals steps `shouldBe` NumberChanged 10.234

  it "should input '3', input 'a' then input '6'" $ do
    let steps = [evtT "3", evtT "a", evtT "6"]
    lastEvt steps `shouldBe` NumberChanged 36
    model steps ^. floatingValid `shouldBe` True

  it "should input '1234', delete line then input '777'" $ do
    let steps = [evtT "1234", selLineL, evtT "777"]
    lastEvt steps `shouldBe` NumberChanged 777
    model steps ^. floatingValid `shouldBe` True

  where
    wenv = mockWenv (TestModel 0 False)
    floatNode = floatingFieldV_ 0 NumberChanged [minValue 10, maxValue 2345, selectOnFocus True, validInput floatingValid]
    floatDecimalsNode = floatingFieldV_ 0 NumberChanged [selectOnFocus True, decimals 3]
    evts es = nodeHandleEventEvts wenv (Focus : es) floatNode
    evtsAlt es = nodeHandleEventEvts wenv (Focus : es) floatDecimalsNode
    model es = nodeHandleEventModel wenv (Focus : es) floatNode
    lastIdx es = Seq.index es (Seq.length es - 1)
    lastEvt es = lastIdx (evts es)
    lastEvtDecimals es = lastIdx (evtsAlt es)

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  it "should return width = Flex 110 1" $
    sizeReqW `shouldBe` FlexSize 110 1

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` FixedSize 20

  where
    wenv = mockWenvEvtUnit (TestModel 10000000 True)
    (sizeReqW, sizeReqH) = nodeUpdateSizeReq wenv (floatingField floatingValue)
