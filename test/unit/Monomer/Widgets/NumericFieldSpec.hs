{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.NumericFieldSpec (spec) where

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
import Monomer.Widgets.NumericField
import Monomer.Widgets.Stack

import qualified Monomer.Lens as L

data TestEvt
  = IntegralChanged Int
  | FractionalChanged Double
  | GotFocus
  | LostFocus
  deriving (Eq, Show)

data IntegralModel = IntegralModel {
  _imIntegralValue :: Int,
  _imIntegralValid :: Bool
} deriving (Eq, Show)

data FractionalModel = FractionalModel {
  _fmFractionalValue :: Double,
  _fmFractionalValid :: Bool
} deriving (Eq, Show)

instance WidgetModel IntegralModel
instance WidgetModel FractionalModel

makeLensesWith abbreviatedFields ''IntegralModel
makeLensesWith abbreviatedFields ''FractionalModel

spec :: Spec
spec = describe "NumericField" $ do
  specIntegral
  specFractional

specIntegral :: Spec
specIntegral = describe "IntegralField" $ do
  handleEventIntegral
  handleEventValueIntegral
  handleEventMouseDragIntegral
  getSizeReqIntegral

handleEventIntegral :: Spec
handleEventIntegral = describe "handleEventIntegral" $ do
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
    wenv = mockWenv (IntegralModel 0 True)
    basicIntNode = numericField_ integralValue [selectOnFocus False]
    intCfg = [maxValue 1501, validInput integralValid, onFocus GotFocus, onBlur LostFocus]
    intNode = numericField_ integralValue intCfg
    model es = nodeHandleEventModel wenv (Focus : es) intNode
    modelBasic es = nodeHandleEventModel wenv es basicIntNode
    events evt = nodeHandleEventEvts wenv [evt] intNode

handleEventValueIntegral :: Spec
handleEventValueIntegral = describe "handleEventIntegral" $ do
  it "should input an '10'" $
    evts [evtT "1", evtT "0"] `shouldBe` Seq.fromList [IntegralChanged 1, IntegralChanged 10]

  it "should input '1', move to beginning and input '5'" $ do
    let steps = [evtT "1", moveLineL, evtT "5"]
    lastEvt steps `shouldBe` IntegralChanged 51

  it "should input '1', input '.' then input '5'" $ do
    let steps = [evtT "1", evtT ".", evtT "5"]
    lastEvt steps `shouldBe` IntegralChanged 15
    model steps ^. integralValid `shouldBe` True

  it "should input '3', input 'a' then input '6'" $ do
    let steps = [evtT "3", evtT "a", evtT "6"]
    lastEvt steps `shouldBe` IntegralChanged 36
    model steps ^. integralValid `shouldBe` True

  it "should input '1234', delete line then input '777'" $ do
    let steps = [evtT "1234", selLineL, evtT "777"]
    lastEvt steps `shouldBe` IntegralChanged 777
    model steps ^. integralValid `shouldBe` True

  where
    wenv = mockWenv (IntegralModel 0 False)
    intNode = numericFieldV_ 0 IntegralChanged [maxValue 2345, selectOnFocus True, validInput integralValid]
    evts es = nodeHandleEventEvts wenv (Focus : es) intNode
    model es = nodeHandleEventModel wenv (Focus : es) intNode
    lastIdx es = Seq.index es (Seq.length es - 1)
    lastEvt es = lastIdx (evts es)

handleEventMouseDragIntegral :: Spec
handleEventMouseDragIntegral = describe "handleEventMouseDragIntegral" $ do
  it "should drag upwards 100 pixels, setting the value to 100" $ do
    let str = "This is text"
    let selStart = Point 50 30
    let selEnd = Point 50 (-70)
    let steps = [evtPress selStart, evtMove selEnd, evtReleaseDrag selEnd]
    model steps ^. integralValue `shouldBe` 100

  it "should drag downwards 100 pixels, setting the value to -200 (dragRate = 2)" $ do
    let str = "This is text"
    let selStart = Point 50 50
    let selEnd = Point 50 150
    let steps = [evtPress selStart, evtMove selEnd, evtReleaseDrag selEnd]
    model steps ^. integralValue `shouldBe` -200

  it "should drag downwards 1000 pixels, staying at -500 (the minimum)" $ do
    let str = "This is text"
    let selStart = Point 50 50
    let selEnd = Point 50 1050
    let steps = [evtPress selStart, evtMove selEnd, evtReleaseDrag selEnd]
    model steps ^. integralValue `shouldBe` -500

  it "should drag upwnwards 1000 pixels, staying at 500 (the maximum)" $ do
    let str = "This is text"
    let selStart = Point 50 50
    let selEnd = Point 50 (-950)
    let steps = [evtPress selStart, evtMove selEnd, evtReleaseDrag selEnd]
    model steps ^. integralValue `shouldBe` 500

  it "should drag downwards 30 and 20 pixels, setting the value to -5" $ do
    let str = "This is text"
    let selStart = Point 50 30
    let selMid = Point 50 60
    let selEnd = Point 50 50
    let steps = [
          evtPress selStart, evtMove selMid, evtReleaseDrag selMid,
          evtPress selStart, evtMove selEnd, evtReleaseDrag selEnd
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
    wenv = mockWenv (IntegralModel 0 False)
    integralNode = vstack [
        button "Test" (IntegralChanged 0), -- Used only to have focus
        numericField integralValue,
        numericField_ integralValue [dragRate 2, minValue (-500), maxValue 500]
      ]
    evts es = nodeHandleEventEvts wenv es integralNode
    model es = nodeHandleEventModel wenv es integralNode
    lastIdx es = Seq.index es (Seq.length es - 1)
    lastEvt es = lastIdx (evts es)

getSizeReqIntegral :: Spec
getSizeReqIntegral = describe "getSizeReqIntegral" $ do
  it "should return width = Flex 110 1" $
    sizeReqW `shouldBe` FlexSize 110 1

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` FixedSize 20

  where
    wenv = mockWenvEvtUnit (IntegralModel 10000000000 True)
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv (numericField integralValue)

-- ------------------------------
-- Fractional field
-- ------------------------------
specFractional :: Spec
specFractional = describe "FractionalField" $ do
  handleEventFractional
  handleEventValueFractional
  handleEventMouseDragFractional
  getSizeReqFractional

handleEventFractional :: Spec
handleEventFractional = describe "handleEventFractional" $ do
  it "should input '123' without select on focus" $ do
    modelBasic [evtT "1", evtT "2", evtT "3"] ^. fractionalValue `shouldBe` 1230
    modelBasic [evtT "1", evtT "2", evtT "3"] ^. fractionalValid `shouldBe` True

  it "should input '1.23'" $ do
    model [evtT "1.23"] ^. fractionalValue `shouldBe` 1.23
    model [evtT "1.23"] ^. fractionalValid `shouldBe` True

  it "should input '-1'" $ do
    model [evtT "-1"] ^. fractionalValue `shouldBe` -1
    model [evtT "-1"] ^. fractionalValid `shouldBe` True

  it "should input '1501'" $ do
    model [evtT "1", evtT "5", evtT "0", evtT "1"] ^. fractionalValue `shouldBe` 1501
    model [evtT "1", evtT "5", evtT "0", evtT "1"] ^. fractionalValid `shouldBe` True

  it "should input '1502', but fail because of maxValue" $ do
    model [evtT "1", evtT "5", evtT "0", evtT "2"] ^. fractionalValue `shouldBe` 150
    model [evtT "1", evtT "5", evtT "0", evtT "2"] ^. fractionalValid `shouldBe` False

  it "should input '123', remove one character and input '4'" $ do
    model [evtT "123", delCharL, evtT "4"] ^. fractionalValue `shouldBe` 124
    model [evtT "123", delCharL, evtT "4"] ^. fractionalValid `shouldBe` True

  it "should input '123', remove one word and input '456'" $ do
    model [evtT "123", delWordL, evtT "456"] ^. fractionalValue `shouldBe` 456
    model [evtT "123", delWordL, evtT "456"] ^. fractionalValid `shouldBe` True

  it "should input '123.34', remove one word and input '56'" $ do
    model [evtT "123.34", delWordL, evtT "56"] ^. fractionalValue `shouldBe` 123.56
    model [evtT "123.34", delWordL, evtT "56"] ^. fractionalValid `shouldBe` True

  it "should input '123.34', remove two words and input '56'" $ do
    model [evtT "123.34", delWordL, delWordL, evtT "56"] ^. fractionalValue `shouldBe` 56
    model [evtT "123.34", delWordL, delWordL, evtT "56"] ^. fractionalValid `shouldBe` True

  it "should generate an event when focus is received" $
    events Focus `shouldBe` Seq.singleton GotFocus

  it "should generate an event when focus is lost" $
    events Blur `shouldBe` Seq.singleton LostFocus

  where
    wenv = mockWenv (FractionalModel 0 True)
    basicFractionalNode = numericField_ fractionalValue [selectOnFocus False]
    floatCfg = [maxValue 1501, validInput fractionalValid, onFocus GotFocus, onBlur LostFocus]
    floatNode = numericField_ fractionalValue floatCfg
    model es = nodeHandleEventModel wenv es floatNode
    modelBasic es = nodeHandleEventModel wenv es basicFractionalNode
    events evt = nodeHandleEventEvts wenv [evt] floatNode

handleEventValueFractional :: Spec
handleEventValueFractional = describe "handleEventValueFractional" $ do
  it "should input an '100'" $
    evts [evtT "1", evtT "0", evtT "0"] `shouldBe` Seq.fromList [FractionalChanged 10, FractionalChanged 100]

  it "should input a '1' and be considered invalid" $ do
    evts [evtT "1"] `shouldBe` Seq.fromList []
    model [evtT "1"] ^. fractionalValid `shouldBe` False

  it "should input '1', move to beginning and input '5'" $ do
    let steps = [evtT "1", moveLineL, evtT "5"]
    lastEvt steps `shouldBe` FractionalChanged 51

  it "should input '1', input '.' then input '5'" $ do
    let steps = [evtT "10", evtT ".", evtT "5"]
    lastEvt steps `shouldBe` FractionalChanged 10.5
    model steps ^. fractionalValid `shouldBe` True

  it "should input '20', input '.' twice then input '777'" $ do
    let steps = [evtT "20", evtT ".", evtT ".", evtT "7", evtT "7", evtT "7"]
    lastEvt steps `shouldBe` FractionalChanged 20.77
    model steps ^. fractionalValid `shouldBe` True

  it "should input '10', '.' then input '2345'" $ do
    let steps = [evtT "10", evtT ".", evtT "2", evtT "3", evtT "4", evtT "5"]
    lastEvtDecimals steps `shouldBe` FractionalChanged 10.234

  it "should input '3', input 'a' then input '6'" $ do
    let steps = [evtT "3", evtT "a", evtT "6"]
    lastEvt steps `shouldBe` FractionalChanged 36
    model steps ^. fractionalValid `shouldBe` True

  it "should input '1234', delete line then input '777'" $ do
    let steps = [evtT "1234", selLineL, evtT "777"]
    lastEvt steps `shouldBe` FractionalChanged 777
    model steps ^. fractionalValid `shouldBe` True

  where
    wenv = mockWenv (FractionalModel 0 False)
    floatNode = numericFieldV_ 0 FractionalChanged [minValue 10, maxValue 2345, selectOnFocus True, validInput fractionalValid]
    floatDecimalsNode = numericFieldV_ 0 FractionalChanged [selectOnFocus True, decimals 3]
    evts es = nodeHandleEventEvts wenv (Focus : es) floatNode
    evtsAlt es = nodeHandleEventEvts wenv (Focus : es) floatDecimalsNode
    model es = nodeHandleEventModel wenv (Focus : es) floatNode
    lastIdx es = Seq.index es (Seq.length es - 1)
    lastEvt es = lastIdx (evts es)
    lastEvtDecimals es = lastIdx (evtsAlt es)

handleEventMouseDragFractional :: Spec
handleEventMouseDragFractional = describe "handleEventMouseDragFractional" $ do
  it "should drag upwards 100 pixels, setting the value to 10" $ do
    let str = "This is text"
    let selStart = Point 50 30
    let selEnd = Point 50 (-70)
    let steps = [evtPress selStart, evtMove selEnd, evtReleaseDrag selEnd]
    model steps ^. fractionalValue `shouldBe` 10

  it "should drag downwards 100 pixels, setting the value to -20 (dragRate = 0.2)" $ do
    let str = "This is text"
    let selStart = Point 50 50
    let selEnd = Point 50 150
    let steps = [evtPress selStart, evtMove selEnd, evtReleaseDrag selEnd]
    model steps ^. fractionalValue `shouldBe` -20

  it "should drag downwards 30 and 20 pixels, setting the value to -5" $ do
    let str = "This is text"
    let selStart = Point 50 30
    let selMid = Point 50 60
    let selEnd = Point 50 50
    let steps = [
          evtPress selStart, evtMove selMid, evtReleaseDrag selMid,
          evtPress selStart, evtMove selEnd, evtReleaseDrag selEnd
          ]
    model steps ^. fractionalValue `shouldBe` -5

  it "should drag upwards 100 pixels, but value stay at 0 since it has focus" $ do
    let str = "This is text"
    let selStart = Point 50 30
    let selEnd = Point 50 (-70)
    let steps = [evtK keyTab, evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. fractionalValue `shouldBe` 0

  it "should drag upwards 100 pixels, but value stay at 0 since it was double clicked on" $ do
    let str = "This is text"
    let selStart = Point 50 30
    let selEnd = Point 50 (-70)
    let steps = [evtDblClick selStart, evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. fractionalValue `shouldBe` 0

  where
    wenv = mockWenv (FractionalModel 0 False)
    floatNode = vstack [
        button "Test" (FractionalChanged 0), -- Used only to have focus
        numericField fractionalValue,
        numericField_ fractionalValue [dragRate 0.2]
      ]
    evts es = nodeHandleEventEvts wenv es floatNode
    model es = nodeHandleEventModel wenv es floatNode
    lastIdx es = Seq.index es (Seq.length es - 1)
    lastEvt es = lastIdx (evts es)

getSizeReqFractional :: Spec
getSizeReqFractional = describe "getSizeReqFractional" $ do
  it "should return width = Flex 110 1" $
    sizeReqW `shouldBe` FlexSize 110 1

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` FixedSize 20

  where
    wenv = mockWenvEvtUnit (FractionalModel 10000000 True)
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv (numericField fractionalValue)
