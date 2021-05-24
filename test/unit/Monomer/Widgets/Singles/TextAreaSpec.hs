{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Singles.TextAreaSpec (spec) where

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
import Monomer.Widgets.Singles.TextArea

import qualified Monomer.Lens as L

data TestEvt
  = TextChanged Text
  | GotFocus Path
  | LostFocus Path
  deriving (Eq, Show)

newtype TestModel = TestModel {
  _tmTextValue :: Text
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''TestModel

spec :: Spec
spec = fdescribe "TextArea" $ do
  handleEvent
  handleEventValue
--  handleEventMouseSelect
--  handleEventHistory
  getSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should input an 'a'" $ do
    model [evtT "a"] ^. textValue `shouldBe` "a"
    ctx [evtT "a"] ^. L.renderSchedule `shouldSatisfy` (==1) . length

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

  it "should input 'This string very long text is', and reject ' invalid' since maxLength == 40" $ do
    let str = "This string very very long text is"
    let steps = [evtT str, evtT " invalid"]
    model steps ^. textValue `shouldBe` "This string very very long text is"

  it "should input 5 empty lines, and reject ' invalid' since maxLines == 5" $ do
    let str = "\n\n\n\n\n"
    let steps = [evtT str, evtT " invalid"]
    model steps ^. textValue `shouldBe` str

  it "should input 'This is text\ntest', select all and input 'No'" $ do
    let str = "This is text\ntest"
    let steps = [evtT str, evtKG keyA, evtT "No"]
    model steps ^. textValue `shouldBe` "No"

  it "should input 'This is text', receive focus (with select on Focus) and input 'No'" $ do
    let str = "This is text"
    let steps = [evtT str, evtFocus, evtT "No"]
    model steps ^. textValue `shouldBe` "No"

  it "should copy and paste text around" $ do
    let str = "This is some long text"
    let steps = [evtT str, selWordL, selWordL, selCharL, evtKG keyC, moveWordL, moveWordL, moveCharL, evtKG keyV]
    model steps ^. textValue `shouldBe` "This long text is some long text"

  it "should cut and paste text around" $ do
    let str = "This is long text"
    let steps = [evtT str, selWordL, selCharL, evtKG keyX, moveWordL, moveWordL, moveCharL, evtKG keyV]
    model steps ^. textValue `shouldBe` "This text is long"

  it "should generate an event when focus is received" $ do
    events [evtFocus] `shouldBe` Seq.singleton (GotFocus emptyPath)
    ctx [evtFocus] ^. L.renderSchedule `shouldSatisfy` (==1) . length

  it "should generate an event when focus is lost" $ do
    events [evtBlur] `shouldBe` Seq.singleton (LostFocus emptyPath)
    ctx [evtFocus, evtBlur] ^. L.renderSchedule `shouldSatisfy` null

  where
    wenv = mockWenv (TestModel "")
    txtCfg = [maxLength 40, maxLines 5, selectOnFocus, onFocus GotFocus, onBlur LostFocus]
    txtNode = textArea_ textValue txtCfg
    model es = nodeHandleEventModel wenv es txtNode
    events es = nodeHandleEventEvts wenv es txtNode
    ctx evts = nodeHandleEventCtx wenv evts txtNode

handleEventValue :: Spec
handleEventValue = describe "handleEventValue" $ do
  it "should input an 'ab'" $
    evts [evtT "a", evtT "b"] `shouldBe` Seq.fromList [TextChanged "a", TextChanged "ab"]

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
    let steps = [evtT "abc", evtKCS keyHome, evtT "def", moveCharL, moveCharL, evtKCS keyEnd, evtT "dd"]
    lastEvt steps `shouldBe` TextChanged "ddd"

  it "should input 'abc123', move to beginning, select three letters, copy, move to end, paste" $ do
    let steps = [evtT "abc123", evtKC keyHome, selCharR, selCharR, selCharR, evtKG keyC, evtK keyEnd, evtKG keyV]
    lastEvt steps `shouldBe` TextChanged "abc123abc"

  it "should input a-b-c-d on separate lines, move to beginning, move down, select two lines, input 'e'" $ do
    let steps = [evtT "a\nb\nc\nd", evtKC keyLeft, evtKC keyUp, evtK keyDown, evtKS keyDown, evtKS keyDown, evtT "e", evtK keyReturn]
    lastEvt steps `shouldBe` TextChanged "a\ne\nd"

  where
    wenv = mockWenv (TestModel "")
    txtNode = textAreaV "" TextChanged
    evts es = nodeHandleEventEvts wenv es txtNode
    lastIdx es = Seq.index es (Seq.length es - 1)
    lastEvt es = lastIdx (evts es)

handleEventMouseSelect :: Spec
handleEventMouseSelect = describe "handleEventMouseSelect" $ do
  it "should add text at the end, since click + drag started outside of viewport" $ do
    let str = "This is text"
    let selStart = Point 50 100
    let selEnd = Point 120 10
    let steps = [evtT str, evtPress selStart, evtMove selEnd, evtRelease selEnd, evtT "!"]
    model steps ^. textValue `shouldBe` "This is text!"

  it "should drag around and input 'Text'" $ do
    let str = ""
    let selStart = Point 50 10
    let selMid1 = Point 0 10
    let selMid2 = Point 200 10
    let selMid3 = Point (-200) 10
    let selEnd = Point 120 10
    let moves = [evtMove selMid1, evtMove selMid2, evtMove selMid3, evtMove selEnd]
    let steps = [evtT str, evtPress selStart] ++ moves ++ [evtRelease selEnd, evtT "Text"]
    model steps ^. textValue `shouldBe` "Text"

  it "should input 'This is text', select 'is text' and input 'test'" $ do
    let str = "This is text"
    let selStart = Point 40 10
    let selEnd = Point 120 10
    let steps = [evtT str, evtPress selStart, evtMove selEnd, evtRelease selEnd, evtT "test"]
    model steps ^. textValue `shouldBe` "This test"

  it "should input 'This is text', select all from beginning and input 'New'" $ do
    let str = "This is new"
    let selStart = Point 0 10
    let selEnd = Point 200 10
    let steps = [evtT str, evtPress selStart, evtMove selEnd, evtRelease selEnd, evtT "New"]
    model steps ^. textValue `shouldBe` "New"

  it "should input 'This is text', select all from the end and input 'New'" $ do
    let str = "This is"
    let selStart = Point 70 10
    let selEnd = Point 0 10
    let steps = [evtT str, evtPress selStart, evtMove selEnd, evtRelease selEnd, evtT "New"]
    model steps ^. textValue `shouldBe` "New"

  where
    wenv = mockWenvEvtUnit (TestModel "")
    txtNode = vstack [
        hstack [
          textArea textValue `style` [width 105],
          hstack []
        ]
      ]
    model es = nodeHandleEventModel wenv es txtNode
    events es = nodeHandleEventEvts wenv es txtNode

handleEventHistory :: Spec
handleEventHistory = describe "handleEventHistory" $ do
  it "should input 'This is text', have the last word removed and then undo" $ do
    let str = "This is text"
    let steps1 = [evtT str, evtKA keyBackspace]
    let steps2 = steps1 ++ [evtKG keyZ]
    model steps1 ^. textValue `shouldBe` "This is "
    model steps2 ^. textValue `shouldBe` "This is text"
    lastEvt steps2 `shouldBe` TextChanged "This is text"

  it "should input 'This is text', have the last two words removed, undo and redo" $ do
    let str = "This is text"
    let steps1 = [evtT str, evtKA keyBackspace, evtKA keyBackspace]
    let steps2 = steps1 ++ [evtKG keyZ, evtKG keyZ, evtKGS keyZ]
    model steps1 ^. textValue `shouldBe` "This "
    model steps2 ^. textValue `shouldBe` "This is "
    lastEvt steps2 `shouldBe` TextChanged "This is "

  it "should input 'This is just a string', play around with history and come end up with 'This is just text" $ do
    let str = "This is just a string"
    let steps1 = [evtT str, evtKA keyBackspace, evtKA keyBackspace, evtKA keyBackspace, evtKA keyBackspace, evtKA keyBackspace]
    let steps2 = steps1 ++ [evtKG keyZ, evtKG keyZ, evtKG keyZ, evtKG keyZ, evtKG keyZ, evtKGS keyZ, evtKGS keyZ, evtT "text"]
    model steps1 ^. textValue `shouldBe` ""
    model steps2 ^. textValue `shouldBe` "This is just text"
    lastEvt steps2 `shouldBe` TextChanged "This is just text"

  it "should input 'This is text', remove two words, undo, input 'not' and fail to redo" $ do
    let str = "This is text"
    let steps1 = [evtT str, evtKA keyBackspace, evtKA keyBackspace]
    let steps2 = steps1 ++ [evtKG keyZ, evtT "not", evtKGS keyZ]
    model steps1 ^. textValue `shouldBe` "This "
    model steps2 ^. textValue `shouldBe` "This is not"
    lastEvt steps2 `shouldBe` TextChanged "This is not"

  where
    wenv = mockWenv (TestModel "")
    txtCfg = [onChange TextChanged, selectOnFocus, onFocus GotFocus, onBlur LostFocus]
    txtNode = textArea_ textValue txtCfg
    model es = nodeHandleEventModel wenv es txtNode
    evts es = nodeHandleEventEvts wenv es txtNode
    lastIdx es = Seq.index es (Seq.length es - 1)
    lastEvt es = lastIdx (evts es)

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return (Min 100 1, Min 20 1)" $ do
    sizeReqW1 `shouldBe` minSize 100 1
    sizeReqH1 `shouldBe` minSize 20 1

  it "should return (Min 150 1, Min 80 1)" $ do
    sizeReqW2 `shouldBe` minSize 150 1
    sizeReqH2 `shouldBe` minSize 80 1

  where
    wenv1 = mockWenvEvtUnit (TestModel "Test value")
    (sizeReqW1, sizeReqH1) = nodeGetSizeReq wenv1 (textArea textValue)
    wenv2 = mockWenvEvtUnit (TestModel "Test long value\n\n\n\n")
    (sizeReqW2, sizeReqH2) = nodeGetSizeReq wenv2 (textArea textValue)
