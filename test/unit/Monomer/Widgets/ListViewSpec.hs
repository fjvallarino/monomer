{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.ListViewSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Data.Functor ((<&>))
import Data.Text (Text)
import Test.Hspec
import TextShow

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event
import Monomer.TestEventUtil
import Monomer.TestUtil
import Monomer.Widgets.Label
import Monomer.Widgets.ListView

import qualified Monomer.Lens as L

data TestEvt
  = ItemSel Int TestItem
  | GotFocus
  | LostFocus
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
testItem70 = testItems!!70

spec :: Spec
spec = describe "ListView" $ do
  handleEvent
  handleEventValue
  updateSizeReq

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

  -- Move back to 70 and fix performance issue
  it "should update the model when Enter/Space is pressed, after navigating to an element" $ do
    let steps = [evtK keyTab] ++ replicate 8 (evtK keyDown) ++ [evtK keyUp, evtK keySpace]
    model steps ^. selectedItem `shouldBe` testItem7

  it "should generate an event when focus is received" $ do
    let p = Point 100 10
    events [evtClick p] `shouldBe` Seq.singleton GotFocus

  it "should generate an event when focus is lost" $ do
    let p = Point 100 10
    events [evtClick p, Blur] `shouldBe` Seq.fromList [GotFocus, LostFocus]

  where
    wenv = mockWenv (TestModel testItem0)
    lvInst = listView_ selectedItem testItems (label . showt) [onFocus GotFocus, onBlur LostFocus]
    clickModel p = instHandleEventModel wenv [Click p LeftBtn] lvInst
    model keys = instHandleEventModel wenv keys lvInst
    events evts = instHandleEventEvts wenv evts lvInst

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
    lvInst = listViewV_ testItem0 ItemSel testItems (label . showt) [onFocus GotFocus, onBlur LostFocus]
    clickEvts p = instHandleEventEvts wenv [Click p LeftBtn] lvInst
    events evts = Seq.drop (Seq.length res - 1) res where
      res = instHandleEventEvts wenv evts lvInst

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  it "should return width = Flex 120" $
    sizeReqW `shouldBe` FlexSize 120 1

  it "should return height = Flex 2000 1" $
    sizeReqH `shouldBe` FlexSize 2000 1

  where
    wenv = mockWenvEvtUnit (TestModel testItem0)
    lvInst = listView selectedItem testItems (label . showt)
    (sizeReqW, sizeReqH) = instUpdateSizeReq wenv lvInst