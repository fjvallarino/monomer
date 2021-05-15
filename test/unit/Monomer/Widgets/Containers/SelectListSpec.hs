{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Containers.SelectListSpec (spec) where

import Control.Lens ((&), (^.), (.~), _1)
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
import Monomer.Widgets.Containers.SelectList
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.Button
import Monomer.Widgets.Singles.Label

import qualified Monomer.Lens as L

data TestEvt
  = ItemSel Int TestItem
  | BtnClick
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

testItems = [0..99] <&> TestItem
testItem0 = head testItems
testItem3 = testItems!!3
testItem7 = testItems!!7
testItem10 = testItems!!10
testItem70 = testItems!!70

spec :: Spec
spec = describe "SelectList" $ do
  handleEvent
  handleEventValue
  getSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should not update the model if not clicked" $
    clickModel (Point 3000 3000) ^. selectedItem `shouldBe` testItem0

  it "should update the model when clicked" $
    clickModel (Point 100 70) ^. selectedItem `shouldBe` testItem3

  it "should update the model when clicked, after wheel scrolled" $ do
    let p = Point 100 10
    let steps = [evtK keyTab, WheelScroll p (Point 0 (-140)) WheelNormal, evtClick p]
    model steps ^. selectedItem `shouldBe` testItem70

  it "should update the model when clicked, after list is displaced because of arrow press" $ do
    let p = Point 100 10
    let steps = [evtK keyTab] ++ replicate 26 (evtK keyDown) ++ [evtClick p]
    model steps ^. selectedItem `shouldBe` testItem3

  it "should update the model when Enter/Space is pressed, after navigating to an element" $ do
    let steps = [evtK keyTab] ++ replicate 71 (evtK keyDown) ++ [evtK keyUp, evtK keySpace]
    model steps ^. selectedItem `shouldBe` testItem70

  it "should generate an event when focus is received" $ do
    let p = Point 100 30
    eventsCnt [evtClick p] `shouldBe` Seq.singleton (GotFocus $ Seq.fromList [0, 0])

  it "should generate an event when focus is lost" $ do
    let p = Point 100 30
    let path = Seq.fromList [0, 0]
    eventsCnt [evtClick p, evtBlur] `shouldBe` Seq.fromList [GotFocus path, LostFocus emptyPath]

  where
    wenv = mockWenv (TestModel testItem0)
    slNode = selectList_ selectedItem testItems (label . showt) [onFocus GotFocus, onBlur LostFocus]
    cntNode = vstack [
        button "Test" BtnClick,
        selectList_ selectedItem testItems (label . showt) [onFocus GotFocus, onBlur LostFocus]
      ]
    clickModel p = nodeHandleEventModel wenv [Click p LeftBtn] slNode
    model keys = nodeHandleEventModel wenv keys slNode
    events evts = nodeHandleEventEvts wenv evts slNode
    eventsCnt evts = nodeHandleEventEvts wenv evts cntNode

handleEventValue :: Spec
handleEventValue = describe "handleEventValue" $ do
  it "should not generate an event if clicked outside" $
    clickEvts (Point 3000 3000) `shouldBe` Seq.empty

  it "should generate an event when clicked" $
    events [evtK keyTab, evtClick (Point 100 70)] `shouldBe` Seq.singleton (ItemSel 3 testItem3)

  it "should generate an event when Enter/Space is pressed, after navigating to an element" $ do
    let steps = [evtK keyTab] ++ replicate 7 (evtK keyDown) ++ [evtK keySpace]
    events steps `shouldBe` Seq.singleton (ItemSel 7 testItem7)

  where
    wenv = mockWenv (TestModel testItem0)
    slNode = selectListV_ testItem0 ItemSel testItems (label . showt) [onFocus GotFocus, onBlur LostFocus]
    clickEvts p = nodeHandleEventEvts wenv [Click p LeftBtn] slNode
    events evts = Seq.drop (Seq.length res - 1) res where
      res = nodeHandleEventEvts wenv evts slNode

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return width = Flex 120" $
    sizeReqW `shouldBe` expandSize 120 1

  it "should return height = Flex 2000 1" $
    sizeReqH `shouldBe` expandSize 2000 1

  where
    wenv = mockWenvEvtUnit (TestModel testItem0)
    slNode = selectList selectedItem testItems (label . showt)
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv slNode
