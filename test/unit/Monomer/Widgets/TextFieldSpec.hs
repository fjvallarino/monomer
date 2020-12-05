{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.TextFieldSpec (spec) where

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
import Monomer.Widgets.TextField

import qualified Monomer.Lens as L

data TestEvt
  = TextChanged Text
  | GotFocus
  | LostFocus
  deriving (Eq, Show)

newtype TestModel = TestModel {
  _tmTextValue :: Text
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''TestModel

spec :: Spec
spec = describe "TextField" $ do
  handleEvent
  handleEventValue
  updateSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should input an 'a'" $ do
    model [evtT "a"] ^. textValue `shouldBe` "a"
    ctx [evtT "a"] ^. L.renderSchedule `shouldSatisfy` null
    ctx [Focus, evtT "a"] ^. L.renderSchedule `shouldSatisfy` (==1) . length

  it "should input 'ababa', remove the middle 'a' and input 'c'" $ do
    let steps = [evtT "ababa", moveCharL, moveCharL, evtK keyBackspace, evtT "c"]
    model steps ^. textValue `shouldBe` "abcba"

  it "should input 'ababa', select last two and input 'c'" $ do
    let steps = [evtT "ababa", selCharL, selCharL, selCharL, evtT "c"]
    model steps ^. textValue `shouldBe` "abc"

  it "should input 'This is a dog', move to beginning, select first word and input 'that'" $ do
    let str = "This is a dog"
    let steps = [evtT str, moveWordL, moveWordL, moveWordL, moveWordL, selWordR, evtT "that"]
    model steps ^. textValue `shouldBe` "that is a dog"

  it "should input 'This is a dog', select one word, deselect and input 'big '" $ do
    let str = "This is a dog"
    let steps = [evtT str, selWordL, moveCharL, evtT "big "]
    model steps ^. textValue `shouldBe` "This is a big dog"

  it "should input 'This string is', and reject ' invalid' since maxLength == 20" $ do
    let str = "This string is"
    let steps = [evtT str, evtT " invalid"]
    model steps ^. textValue `shouldBe` "This string is"

  it "should input 'This is text', select all and input 'No'" $ do
    let str = "This is text"
    let steps = [evtT str, evtKG keyA, evtT "No"]
    model steps ^. textValue `shouldBe` "No"

  it "should input 'This is text', receive focus (with select on Focus) and input 'No'" $ do
    let str = "This is text"
    let steps = [evtT str, Focus, evtT "No"]
    model steps ^. textValue `shouldBe` "No"

  it "should generate an event when focus is received" $ do
    events Focus `shouldBe` [GotFocus]
    ctx [Focus] ^. L.renderSchedule `shouldSatisfy` (==1) . length

  it "should generate an event when focus is lost" $ do
    events Blur `shouldBe` [LostFocus]
    ctx [Focus, Blur] ^. L.renderSchedule `shouldSatisfy` null

  where
    wenv = mockWenv (TestModel "")
    txtCfg = [maxLength 20, selectOnFocus True, onFocus GotFocus, onBlur LostFocus]
    txtNode = textField_ textValue txtCfg
    model es = nodeHandleEventModel wenv es txtNode
    events evt = nodeHandleEventEvts wenv [evt] txtNode
    ctx evts = nodeHandleEventCtx wenv evts txtNode

handleEventValue :: Spec
handleEventValue = describe "handleEvent" $ do
  it "should input an 'ab'" $
    evts [evtT "a", evtT "b"] `shouldBe` [TextChanged "a", TextChanged "ab"]

  it "should input 'this is a dog', input '?', move to beginning and input 'Is '" $ do
    let str = "this is a dog"
    let steps = [evtT str, evtT "?", moveLineL, evtT "Is "]
    lastEvt steps `shouldBe` TextChanged "Is this is a dog?"

  it "should input 'This is a dog', move before 'is', select 'is', deselect it and input 'nt'" $ do
    let str = "This is a dog"
    let steps = [evtT str, moveWordL, moveWordL, moveWordL, selWordR, moveCharR, evtT "n't"]
    lastEvt steps `shouldBe` TextChanged "This isn't a dog"

  it "should input 'This is a dog', remove one word and input 'bird'" $ do
    let str = "This is a dog"
    let steps = [evtT str, delWordL, evtT "cat"]
    lastEvt steps `shouldBe` TextChanged "This is a cat"

  it "should input 'This is a dog', select to beginning and input 'No'" $ do
    let str = "This is a dog"
    let steps = [evtT str, selLineL, evtT "No"]
    lastEvt steps `shouldBe` TextChanged "No"

  it "should input 'This is a dog', move to beginning, select until end and input 'No'" $ do
    let str = "This is a dog"
    let steps = [evtT str, moveLineL, selLineR, evtT "No"]
    lastEvt steps `shouldBe` TextChanged "No"

  it "should input 'a', move to beginning, input 'H', move to end and input 't'" $ do
    let steps = [evtT "a", evtK keyHome, evtT "H", evtK keyEnd, evtT "t"]
    lastEvt steps `shouldBe` TextChanged "Hat"

  it "should input 'abc', select to beginning, input 'def', move back twice, select to end and input 'dd'" $ do
    let steps = [evtT "abc", evtKAS keyHome, evtT "def", moveCharL, moveCharL, evtKAS keyEnd, evtT "dd"]
    lastEvt steps `shouldBe` TextChanged "ddd"

  -- Copy/paste is not currently tested because SDL requires video initialized and mocking is not in place
  where
    wenv = mockWenv (TestModel "")
    txtNode = textFieldV "" TextChanged
    evts es = nodeHandleEventEvts wenv es txtNode
    lastEvt es = last (evts es)

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  it "should return width = Flex 100 1" $
    sizeReqW `shouldBe` FlexSize 100 1

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` FixedSize 20

  where
    wenv = mockWenvEvtUnit (TestModel "Test value")
    (sizeReqW, sizeReqH) = nodeUpdateSizeReq wenv (textField textValue)
