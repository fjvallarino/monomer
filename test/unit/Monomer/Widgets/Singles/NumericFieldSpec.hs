{-|
Module      : Monomer.Widgets.Singles.NumericFieldSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for NumericField widget.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Singles.NumericFieldSpec (spec) where

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
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.Button
import Monomer.Widgets.Singles.NumericField

import qualified Monomer.Lens as L

data TestEvt
  = IntegralChanged Int
  | FractionalChanged (Maybe Double)
  | ValidNumber Bool
  | GotFocus Path
  | LostFocus Path
  deriving (Eq, Show)

data IntegralModel = IntegralModel {
  _imIntegralValue :: Int,
  _imIntegralValid :: Bool
} deriving (Eq, Show)

data FractionalModel = FractionalModel {
  _fmFractionalValue :: Maybe Double,
  _fmFractionalValid :: Bool
} deriving (Eq, Show)

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
  it "should remove the contents and get Nothing as model value" $ do
    modelBasic [evtKG keyA, evtK keyBackspace] ^. integralValue `shouldBe` 0
    modelBasic [evtKG keyA, evtK keyBackspace] ^. integralValid `shouldBe` False

  it "should input '123' without select on focus" $ do
    modelBasic [evtT "1", evtT "2", evtT "3"] ^. integralValue `shouldBe` 1230
    modelBasic [evtT "1", evtT "2", evtT "3"] ^. integralValid `shouldBe` True

  it "should input '1'" $ do
    model [evtT "1"] ^. integralValue `shouldBe` 1
    model [evtT "1"] ^. integralValid `shouldBe` True
    events [evtT "1"] `shouldBe` Seq.fromList [ValidNumber True]

  it "should input '-1'" $ do
    model [evtT "-1"] ^. integralValue `shouldBe` -1
    model [evtT "-1"] ^. integralValid `shouldBe` True
    events [evtT "-1"] `shouldBe` Seq.fromList [ValidNumber True]

  it "should input '1501'" $ do
    model [evtT "1", evtT "5", evtT "0", evtT "1"] ^. integralValue `shouldBe` 1501
    model [evtT "1", evtT "5", evtT "0", evtT "1"] ^. integralValid `shouldBe` True
    events [evtT "1", evtT "5", evtT "0", evtT "1"] `shouldBe` Seq.fromList (replicate 4 (ValidNumber True))

  it "should input '1502', but fail because of maxValue" $ do
    model [evtT "1", evtT "5", evtT "0", evtT "2"] ^. integralValue `shouldBe` 150
    model [evtT "1", evtT "5", evtT "0", evtT "2"] ^. integralValid `shouldBe` False
    events [evtT "1", evtT "5", evtT "0", evtT "2"] `shouldBe` Seq.fromList (replicate 3 (ValidNumber True) ++ [ValidNumber False])

  it "should input '123', remove one character and input '4'" $ do
    model [evtT "123", delCharL, evtT "4"] ^. integralValue `shouldBe` 124
    model [evtT "123", delCharL, evtT "4"] ^. integralValid `shouldBe` True
    events [evtT "123", delCharL, evtT "4"] `shouldBe` Seq.fromList (replicate 3 (ValidNumber True))

  it "should input '123', remove one word and input '456'" $ do
    model [evtT "123", delWordL, evtT "456"] ^. integralValue `shouldBe` 456
    model [evtT "123", delWordL, evtT "456"] ^. integralValid `shouldBe` True
    events [evtT "123", delWordL, evtT "456"] `shouldBe` Seq.fromList [ValidNumber True, ValidNumber False, ValidNumber True]

  it "should update the model when using the wheel" $ do
    let p = Point 100 10
    let steps1 = [WheelScroll p (Point 0 (-64)) WheelNormal]
    let steps2 = [WheelScroll p (Point 0 300) WheelFlipped]
    let steps3 = [WheelScroll p (Point 0 100) WheelNormal]
    let steps4 = [WheelScroll p (Point 0 2000) WheelNormal]
    model steps1 ^. integralValue `shouldBe` -64
    model steps2 ^. integralValue `shouldBe` -200
    model steps3 ^. integralValue `shouldBe` 100
    model steps4 ^. integralValue `shouldBe` 1501

  it "should generate an event when focus is received" $
    events [evtFocus] `shouldBe` Seq.singleton (GotFocus emptyPath)

  it "should generate an event when focus is lost" $
    events [evtBlur] `shouldBe` Seq.singleton (LostFocus emptyPath)

  where
    wenv = mockWenv (IntegralModel 0 True)
    basicIntNode :: WidgetNode IntegralModel TestEvt
    basicIntNode = numericField_ integralValue [validInput integralValid, selectOnFocus_ False]
    intCfg = [minValue (-200), maxValue 1501, validInput integralValid, validInputV ValidNumber, onFocus GotFocus, onBlur LostFocus]
    intNode = numericField_ integralValue intCfg
    model es = nodeHandleEventModel wenv (evtFocus : es) intNode
    modelBasic es = nodeHandleEventModel wenv es basicIntNode
    events es = nodeHandleEventEvts wenv es intNode

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
    intNode = numericFieldV_ 0 IntegralChanged [maxValue 2345, selectOnFocus, validInput integralValid]
    evts es = nodeHandleEventEvts wenv (evtFocus : es) intNode
    model es = nodeHandleEventModel wenv (evtFocus : es) intNode
    lastIdx es = Seq.index es (Seq.length es - 1)
    lastEvt es = lastIdx (evts es)

handleEventMouseDragIntegral :: Spec
handleEventMouseDragIntegral = describe "handleEventMouseDragIntegral" $ do
  it "should drag upwards 100 pixels, setting the value to 100" $ do
    let selStart = Point 50 30
    let selEnd = Point 50 (-70)
    let steps = [evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. integralValue `shouldBe` 100

  it "should drag downwards 100 pixels, setting the value to -200 (dragRate = 2)" $ do
    let selStart = Point 50 50
    let selEnd = Point 50 150
    let steps = [evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. integralValue `shouldBe` -200

  it "should drag downwards 1000 pixels, staying at -500 (the minimum)" $ do
    let selStart = Point 50 50
    let selEnd = Point 50 1050
    let steps = [evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. integralValue `shouldBe` -500

  it "should drag upwnwards 1000 pixels, staying at 500 (the maximum)" $ do
    let selStart = Point 50 50
    let selEnd = Point 50 (-950)
    let steps = [evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. integralValue `shouldBe` 500

  it "should drag downwards 30 and 20 pixels, setting the value to -5" $ do
    let selStart = Point 50 30
    let selMid = Point 50 60
    let selEnd = Point 50 50
    let steps = [
          evtPress selStart, evtMove selMid, evtRelease selMid,
          evtPress selStart, evtMove selEnd, evtRelease selEnd
          ]
    model steps ^. integralValue `shouldBe` -50

  it "should set focus and drag upwards 100 pixels, but value stay at 0 since shift is not pressed" $ do
    let selStart = Point 50 30
    let selEnd = Point 50 (-70)
    let steps = [evtK keyTab, evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. integralValue `shouldBe` 0

  it "should set focus and drag upwards 100 pixels, setting the value to 100 since shift is pressed" $ do
    let selStart = Point 50 30
    let selEnd = Point 50 (-70)
    let steps = [evtKS keyTab, evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. integralValue `shouldBe` 100

  it "should drag upwards 100 pixels, setting the value to 100 even if it was double clicked on" $ do
    let selStart = Point 50 30
    let selEnd = Point 50 (-70)
    let steps = [evtDblClick selStart, evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. integralValue `shouldBe` 100

  where
    wenv = mockWenv (IntegralModel 0 False)
      & L.inputStatus . L.keyMod . L.leftShift .~ True
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
  it "should return width = Flex 50 1" $
    sizeReqW `shouldBe` expandSize 50 1

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` fixedSize 20

  it "should return width = Flex 110 1 when resizeOnChange = True" $
    sizeReqW2 `shouldBe` expandSize 110 1

  it "should return height = Fixed 20 when resizeOnChange = True" $
    sizeReqH2 `shouldBe` fixedSize 20

  where
    wenv = mockWenvEvtUnit (IntegralModel 10000000000 True)
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv (numericField integralValue)
    numericResize = numericField_ integralValue [resizeOnChange]
    (sizeReqW2, sizeReqH2) = nodeGetSizeReq wenv numericResize

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
  it "should remove the contents and get Nothing as model value" $ do
    modelBasic [evtKG keyA, evtK keyBackspace] ^. fractionalValue `shouldBe` Nothing
    modelBasic [evtKG keyA, evtK keyBackspace] ^. fractionalValid `shouldBe` True

  it "should input '123' without select on focus" $ do
    modelBasic [evtT "1", evtT "2", evtT "3"] ^. fractionalValue `shouldBe` Just 1230
    modelBasic [evtT "1", evtT "2", evtT "3"] ^. fractionalValid `shouldBe` True

  it "should input '1.23'" $ do
    model [evtT "1.23"] ^. fractionalValue `shouldBe` Just 1.23
    model [evtT "1.23"] ^. fractionalValid `shouldBe` True

  it "should input '-1'" $ do
    model [evtT "-1"] ^. fractionalValue `shouldBe` Just (-1)
    model [evtT "-1"] ^. fractionalValid `shouldBe` True

  it "should input '1501'" $ do
    model [evtT "1", evtT "5", evtT "0", evtT "1"] ^. fractionalValue `shouldBe` Just 1501
    model [evtT "1", evtT "5", evtT "0", evtT "1"] ^. fractionalValid `shouldBe` True

  it "should input '1502', but fail because of maxValue" $ do
    model [evtT "1", evtT "5", evtT "0", evtT "2"] ^. fractionalValue `shouldBe` Just 150
    model [evtT "1", evtT "5", evtT "0", evtT "2"] ^. fractionalValid `shouldBe` False

  it "should input '123', remove one character and input '4'" $ do
    model [evtT "123", delCharL, evtT "4"] ^. fractionalValue `shouldBe` Just 124
    model [evtT "123", delCharL, evtT "4"] ^. fractionalValid `shouldBe` True

  it "should input '123', remove one word and input '456'" $ do
    model [evtT "123", delWordL, evtT "456"] ^. fractionalValue `shouldBe` Just 456
    model [evtT "123", delWordL, evtT "456"] ^. fractionalValid `shouldBe` True

  it "should input '123.34', remove one word and input '56'" $ do
    model [evtT "123.34", delWordL, evtT "56"] ^. fractionalValue `shouldBe` Just 123.56
    model [evtT "123.34", delWordL, evtT "56"] ^. fractionalValid `shouldBe` True

  it "should input '123.34', remove two words and input '56'" $ do
    model [evtT "123.34", delWordL, delWordL, evtT "56"] ^. fractionalValue `shouldBe` Just 56
    model [evtT "123.34", delWordL, delWordL, evtT "56"] ^. fractionalValid `shouldBe` True

  it "should update the model when using the wheel" $ do
    let p = Point 100 10
    let steps1 = [WheelScroll p (Point 0 (-8000)) WheelNormal]
    let steps2 = [WheelScroll p (Point 0 360) WheelFlipped]
    let steps3 = [WheelScroll p (Point 0 8700) WheelNormal]
    let steps4 = [WheelScroll p (Point 0 16000) WheelNormal]
    model steps1 ^. fractionalValue `shouldBe` Just (-500)
    model steps2 ^. fractionalValue `shouldBe` Just (-36)
    model steps3 ^. fractionalValue `shouldBe` Just 870
    model steps4 ^. fractionalValue `shouldBe` Just 1501

  it "should generate an event when focus is received" $
    events evtFocus `shouldBe` Seq.singleton (GotFocus emptyPath)

  it "should generate an event when focus is lost" $
    events evtBlur `shouldBe` Seq.singleton (LostFocus emptyPath)

  where
    wenv = mockWenv (FractionalModel (Just 0) True)
    basicFractionalNode :: WidgetNode FractionalModel TestEvt
    basicFractionalNode = numericField_ fractionalValue [selectOnFocus_ False, validInput fractionalValid]
    floatCfg = [minValue (Just (-500)), maxValue (Just 1501), validInput fractionalValid, onFocus GotFocus, onBlur LostFocus]
    floatNode = numericField_ fractionalValue floatCfg
    model es = nodeHandleEventModel wenv es floatNode
    modelBasic es = nodeHandleEventModel wenv es basicFractionalNode
    events evt = nodeHandleEventEvts wenv [evt] floatNode

handleEventValueFractional :: Spec
handleEventValueFractional = describe "handleEventValueFractional" $ do
  it "should input an '100'" $
    evts [evtT "1", evtT "0", evtT "0"] `shouldBe` Seq.fromList [FractionalChanged (Just 10), FractionalChanged (Just 100)]

  it "should input a '1' and be considered invalid" $ do
    evts [evtT "1"] `shouldBe` Seq.fromList []
    model [evtT "1"] ^. fractionalValid `shouldBe` False

  it "should input '1', move to beginning and input '5'" $ do
    let steps = [evtT "1", moveLineL, evtT "5"]
    lastEvt steps `shouldBe` FractionalChanged (Just 51)

  it "should input '1', input '.' then input '5'" $ do
    let steps = [evtT "10", evtT ".", evtT "5"]
    lastEvt steps `shouldBe` FractionalChanged (Just 10.5)
    model steps ^. fractionalValid `shouldBe` True

  it "should input '20', input '.' twice then input '777'" $ do
    let steps = [evtT "20", evtT ".", evtT ".", evtT "7", evtT "7", evtT "7"]
    lastEvt steps `shouldBe` FractionalChanged (Just 20.77)
    model steps ^. fractionalValid `shouldBe` True

  it "should input '10', '.' then input '2345'" $ do
    let steps = [evtT "10", evtT ".", evtT "2", evtT "3", evtT "4", evtT "5"]
    lastEvtDecimals steps `shouldBe` FractionalChanged (Just 10.234)

  it "should input '3', input 'a' then input '6'" $ do
    let steps = [evtT "3", evtT "a", evtT "6"]
    lastEvt steps `shouldBe` FractionalChanged (Just 36)
    model steps ^. fractionalValid `shouldBe` True

  it "should input '1234', delete line then input '777'" $ do
    let steps = [evtT "1234", selLineL, evtT "777"]
    lastEvt steps `shouldBe` FractionalChanged (Just 777)
    model steps ^. fractionalValid `shouldBe` True

  where
    wenv = mockWenv (FractionalModel (Just 0) False)
    floatNode = numericFieldV_ (Just 0) FractionalChanged [minValue (Just 10), maxValue (Just 2345), selectOnFocus, validInput fractionalValid]
    floatDecimalsNode = numericFieldV_ (Just 0) FractionalChanged [selectOnFocus, decimals 3]
    evts es = nodeHandleEventEvts wenv (evtFocus : es) floatNode
    evtsAlt es = nodeHandleEventEvts wenv (evtFocus : es) floatDecimalsNode
    model es = nodeHandleEventModel wenv (evtFocus : es) floatNode
    lastIdx es = Seq.index es (Seq.length es - 1)
    lastEvt es = lastIdx (evts es)
    lastEvtDecimals es = lastIdx (evtsAlt es)

handleEventMouseDragFractional :: Spec
handleEventMouseDragFractional = describe "handleEventMouseDragFractional" $ do
  it "should drag upwards 100 pixels, setting the value to 10" $ do
    let selStart = Point 50 30
    let selEnd = Point 50 (-70)
    let steps = [evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. fractionalValue `shouldBe` Just 10

  it "should drag downwards 100 pixels, setting the value to -20 (dragRate = 0.2)" $ do
    let selStart = Point 50 50
    let selEnd = Point 50 150
    let steps = [evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. fractionalValue `shouldBe` Just (-20)

  it "should drag downwards 30 and 20 pixels, setting the value to -5" $ do
    let selStart = Point 50 30
    let selMid = Point 50 60
    let selEnd = Point 50 50
    let steps = [
          evtPress selStart, evtMove selMid, evtRelease selMid,
          evtPress selStart, evtMove selEnd, evtRelease selEnd
          ]
    model steps ^. fractionalValue `shouldBe` Just (-5)

  it "should drag upwards 100 pixels, but value stay at 0 since shift is not pressed" $ do
    let selStart = Point 50 30
    let selEnd = Point 50 (-70)
    let steps = [evtK keyTab, evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. fractionalValue `shouldBe` Just 0

  it "should drag upwards 100 pixels, setting the value to 10 since shift not pressed" $ do
    let selStart = Point 50 30
    let selEnd = Point 50 (-70)
    let steps = [evtKS keyTab, evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. fractionalValue `shouldBe` Just 10

  it "should drag upwards 100 pixels, setting the value to 10 even if it was double clicked on" $ do
    let selStart = Point 50 30
    let selEnd = Point 50 (-70)
    let steps = [evtDblClick selStart, evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. fractionalValue `shouldBe` Just 10

  where
    wenv = mockWenv (FractionalModel (Just 0) False)
      & L.inputStatus . L.keyMod . L.leftShift .~ True
    floatNode = vstack [
        button "Test" (FractionalChanged (Just 0)), -- Used only to have focus
        numericField fractionalValue,
        numericField_ fractionalValue [dragRate 0.2]
      ]
    evts es = nodeHandleEventEvts wenv es floatNode
    model es = nodeHandleEventModel wenv es floatNode
    lastIdx es = Seq.index es (Seq.length es - 1)
    lastEvt es = lastIdx (evts es)

getSizeReqFractional :: Spec
getSizeReqFractional = describe "getSizeReqFractional" $ do
  it "should return width = Flex 70 1" $
    sizeReqW `shouldBe` expandSize 70 1

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` fixedSize 20

  it "should return width = Flex 110 1 when resizeOnChange = True" $
    sizeReqW2 `shouldBe` expandSize 110 1

  it "should return height = Fixed 20 when resizeOnChange = True" $
    sizeReqH2 `shouldBe` fixedSize 20

  where
    wenv = mockWenvEvtUnit (FractionalModel (Just 10000000) True)
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv (numericField fractionalValue)
    numericResize = numericField_ fractionalValue [resizeOnChange]
    (sizeReqW2, sizeReqH2) = nodeGetSizeReq wenv numericResize
