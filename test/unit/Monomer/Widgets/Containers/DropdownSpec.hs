{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Containers.DropdownSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Data.Functor ((<&>))
import Data.Text (Text)
import Test.Hspec
import TextShow

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestEventUtil
import Monomer.TestUtil
import Monomer.Widgets.Containers.Dropdown
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.Button
import Monomer.Widgets.Singles.Label

import qualified Monomer.Lens as L

data TestEvt
  = ItemSel Int TestItem
  | GotFocus Path
  | LostFocus Path
  deriving (Eq, Show)

newtype TestItem = TestItem {
  _tiCode :: Int
} deriving (Eq, Show)

instance TextShow TestItem where
  showb (TestItem c) = "TestItem: " <> showb c

newtype TestModel = TestModel {
  _tmSelectedItem :: TestItem
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''TestItem
makeLensesWith abbreviatedFields ''TestModel

testItems = [0..50] <&> TestItem
testItem0 = head testItems
testItem3 = testItems!!3
testItem7 = testItems!!7
testItem10 = testItems!!10

spec :: Spec
spec = describe "Dropdown" $ do
  handleEvent
  handleEventValue
  getSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  let mainP = Point 50 10

  it "should not update the model if not clicked" $
    model [evtClick (Point 3000 3000)] ^. selectedItem `shouldBe` testItem0

  it "should update the model when clicked" $ do
    let itemP = Point 50 90
    model [evtClick mainP, evtClick itemP] ^. selectedItem `shouldBe` testItem3

  it "should update the model when clicked, after wheel scrolled" $ do
    let itemP = Point 50 90
    let steps = [evtClick mainP, WheelScroll itemP (Point 0 (-14)) WheelNormal, evtClick itemP]
    model steps ^. selectedItem `shouldBe` testItem10

  it "should update the model when clicked, after list is displaced because of arrow press" $ do
    let p = Point 50 30
    let steps = [evtClick mainP] ++ replicate 12 (evtK keyDown) ++ [evtClick p]
    model steps ^. selectedItem `shouldBe` testItem3

  it "should update the model when Enter/Space is pressed, after navigating to an element" $ do
    let steps = [evtClick mainP] ++ replicate 11 (evtK keyDown) ++ [evtK keyUp, evtK keySpace]
    model steps ^. selectedItem `shouldBe` testItem10

  it "should generate an event when focus is received" $ do
    eventsCnt [evtK keyTab] `shouldBe` Seq.singleton (GotFocus $ Seq.fromList [0, 0])

  it "should generate an event when focus is lost and list is not open" $ do
    let path = Seq.fromList [0, 0]
    eventsCnt [evtK keyTab, evtK keyTab] `shouldBe` Seq.fromList [GotFocus path, LostFocus path]

  where
    wenv = mockWenv (TestModel testItem0)
    labelItem = label . showt
    lvNode :: WidgetNode TestModel TestEvt
    lvNode = vstack [
        dropdown_ selectedItem testItems labelItem labelItem [maxHeight 200]
      ]
    cntNode = vstack [
        button "Test" (ItemSel 0 testItem0),
        dropdown_ selectedItem testItems labelItem labelItem [onFocus GotFocus, onBlur LostFocus]
      ]
    model es = nodeHandleEventModel wenv es lvNode
    eventsCnt evts = nodeHandleEventEvts wenv evts cntNode

handleEventValue :: Spec
handleEventValue = describe "handleEventValue" $ do
  let outP = Point 3000 3000
  let mainP = Point 50 10

  it "should not generate an event if clicked outside" $
    clickEvts outP `shouldBe` Seq.empty

  it "should not generate an event when clicked outside, after being opened with keyboard" $ do
    let itemP = Point 50 90
    events [evtK keyDown, evtClick outP, evtClick itemP] `shouldBe` Seq.empty

  it "should generate an event when clicked, after being opened with keyboard" $ do
    let itemP = Point 50 90
    events [evtK keyDown, evtClick itemP] `shouldBe` Seq.singleton (ItemSel 3 testItem3)

  it "should generate an event when Enter/Space is pressed, after navigating to an element" $ do
    let steps = [evtK keyDown] ++ replicate 7 (evtK keyDown) ++ [evtK keySpace]
    events steps `shouldBe` Seq.singleton (ItemSel 7 testItem7)

  it "should generate a focus lost event when opened, canceled, and navigated away" $ do
    let steps = [evtK keyDown, evtK keyEscape, evtK keyTab]
    events steps `shouldBe` Seq.singleton (LostFocus $ Seq.fromList [0, 1])

  it "should generate an event when focus is lost and list is open. The navigation also generates a change event" $ do
    let steps = [evtK keyDown] ++ replicate 3 (evtK keyDown) ++ [evtK keyTab]
    events steps `shouldBe` Seq.fromList [ItemSel 3 testItem3, LostFocus $ Seq.fromList [0, 1]]
  where
    wenv = mockWenv (TestModel testItem0)
    labelItem = label . showt
    lvNode = vstack [
        dropdownV_ testItem0 ItemSel testItems labelItem labelItem [maxHeight 200, onBlur LostFocus],
        button "Test" (ItemSel 0 testItem0)
      ]
    clickEvts p = nodeHandleEventEvts wenv [Click p LeftBtn] lvNode
    events es = nodeHandleEventEvts wenv es lvNode

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return width = Flex 120" $
    sizeReqW `shouldBe` expandSize 120 1

  it "should return height = Flex 20 1" $
    sizeReqH `shouldBe` fixedSize 20

  where
    wenv = mockWenvEvtUnit (TestModel testItem0)
    labelItem l = label_ (showt l) [resizeFactorW 0.01]
    lvNode = dropdown selectedItem testItems labelItem labelItem
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv lvNode
