{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.ZStackSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestEventUtil
import Monomer.TestUtil
import Monomer.Widgets.Button
import Monomer.Widgets.Confirm
import Monomer.Widgets.Composite
import Monomer.Widgets.Label
import Monomer.Widgets.Stack
import Monomer.Widgets.TextField
import Monomer.Widgets.ZStack

import qualified Monomer.Lens as L

newtype BtnEvent
  = BtnClick Int
  deriving (Eq, Show)

data TestModel = TestModel {
  _tmTextValue1 :: Text,
  _tmTextValue2 :: Text
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''TestModel

spec :: Spec
spec = describe "ZStack" $ do
  handleEvent
  getSizeReq
  resize

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  handleEventFirstVisible
  handleEventAllLayersActive
  handleEventFocusTop
  handleEventFocusAll
  handleEventFocusChange
  handleEventFocusKeep

handleEventFirstVisible :: Spec
handleEventFirstVisible = describe "handleEventFirstVisible" $ do
  it "should not generate an event if clicked outside" $
    clickEvts (Point 3000 3000) `shouldBe` Seq.empty

  it "should click the second layer, since top is not visible" $
    clickEvts (Point 100 100) `shouldBe` Seq.singleton (BtnClick 2)

  where
    wenv = mockWenv ()
    zstackNode = zstack [
        button "Click 1" (BtnClick 1),
        button "Click 2" (BtnClick 2),
        button "Click 3" (BtnClick 3) `visible` False
      ]
    clickEvts p = nodeHandleEventEvts wenv [Click p LeftBtn] zstackNode

handleEventAllLayersActive :: Spec
handleEventAllLayersActive = describe "handleEventAllLayersActive" $ do
  it "should not generate an event if clicked outside" $
    clickEvts (Point 3000 3000) `shouldBe` Seq.empty

  it "should click the first layer, since top is not visible and second does not have widgets in that location" $
    clickEvts (Point 200 15) `shouldBe` Seq.singleton (BtnClick 1)

  where
    wenv = mockWenv ()
    zstackNode = zstack_ [onlyTopActive False] [
        button "Click 1" (BtnClick 1),
        vstack [
          button "Click 2" (BtnClick 2) `style` [height 10]
        ],
        button "Click 3" (BtnClick 3) `visible` False
      ]
    clickEvts p = nodeHandleEventEvts wenv [Click p LeftBtn] zstackNode

handleEventFocusTop :: Spec
handleEventFocusTop = describe "handleEventFocusTop" $
  it "should not attempt to set focus on lower layers" $ do
    let steps = [evtK keyTab, evtK keyTab, evtT "abc"]
    model steps ^. textValue1 `shouldBe` ""
    model steps ^. textValue2 `shouldBe` "abc"

  where
    wenv = mockWenv (TestModel "" "")
    zstackNode = zstack [
        textField textValue1,
        textField textValue2
      ]
    model es = nodeHandleEventModel wenv es zstackNode

handleEventFocusAll :: Spec
handleEventFocusAll = describe "handleEventFocusAll" $
  it "should set focus on second layer, since it's enabled" $ do
    let steps = [evtK keyTab, evtT "abc"]
    model steps ^. textValue1 `shouldBe` "abc"
    model steps ^. textValue2 `shouldBe` ""

  where
    wenv = mockWenv (TestModel "" "")
    zstackNode = zstack_ [onlyTopActive False] [
        textField textValue1,
        textField textValue2
      ]
    model es = nodeHandleEventModel wenv es zstackNode

handleEventFocusChange :: Spec
handleEventFocusChange = describe "handleEventFocusChange" $
  it "should restore focus when switching between layers" $ do
    let steps = [ evtK keyTab, evtK keyReturn, evtK keyTab, evtK keyReturn, evtK keyReturn, evtK keyReturn ]
    evts steps `shouldBe` Seq.fromList [BtnClick 2, BtnClick 4, BtnClick 2, BtnClick 4]

  where
    wenv = mockWenv 10
    handleEvent
      :: WidgetEnv Int BtnEvent
      -> WidgetNode Int BtnEvent
      -> Int
      -> BtnEvent
      -> [EventResponse Int BtnEvent BtnEvent]
    handleEvent wenv _ model (BtnClick idx) = [Report (BtnClick idx), Model idx]
    buildUI wenv model = zstack [
        hstack [
          button "3" (BtnClick 3),
          button "4" (BtnClick 4)
        ],
        hstack [
          button "1" (BtnClick 1),
          button "2" (BtnClick 2)
        ] `visible` (model > 2)
      ]
    cmpNode = composite "main" id buildUI handleEvent
    evts es = nodeHandleEventEvts wenv es cmpNode

handleEventFocusKeep :: Spec
handleEventFocusKeep = describe "handleEventFocusKeep" $
  it "should not restore focus when switching between layers if a focus change request is detected" $ do
    let steps = [ evtK keyTab, evtK keyReturn, evtK keyTab, evtK keyReturn, evtK keyReturn, evtK keyReturn ]
    evts steps `shouldBe` Seq.fromList [BtnClick 2, BtnClick 4, BtnClick 2, BtnClick 3]

  where
    wenv = mockWenv 10
    handleEvent
      :: WidgetEnv Int BtnEvent
      -> WidgetNode Int BtnEvent
      -> Int
      -> BtnEvent
      -> [EventResponse Int BtnEvent BtnEvent]
    handleEvent wenv _ model (BtnClick idx) = [Report (BtnClick idx), Model idx]
    buildUI wenv model = zstack [
        hstack [
          confirm "Message" (BtnClick 3) (BtnClick 4)
        ] `visible` (model <= 2),
        hstack [
          button "1" (BtnClick 1),
          button "2" (BtnClick 2)
        ] `visible` (model > 2)
      ]
    cmpNode = composite "main" id buildUI handleEvent
    evts es = nodeHandleEventEvts wenv es cmpNode

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  getSizeReqEmpty
  getSizeReqItems
  getSizeReqItemsFixed

getSizeReqEmpty :: Spec
getSizeReqEmpty = describe "empty" $ do
  it "should return width = Fixed 0" $
    sizeReqW `shouldBe` FixedSize 0

  it "should return height = Fixed 0" $
    sizeReqH `shouldBe` FixedSize 0

  where
    wenv = mockWenv ()
    zstackNode = zstack []
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv zstackNode

getSizeReqItems :: Spec
getSizeReqItems = describe "several items, horizontal" $ do
  it "should return width = Flex 130 0.01" $
    sizeReqW `shouldBe` FlexSize 130 0.01

  it "should return height = Fixed 60" $
    sizeReqH `shouldBe` FixedSize 60

  where
    wenv = mockWenv ()
    zstackNode = zstack [
        vstack [
          label "Label a1"
        ],
        vstack [
          label "Long label b1",
          label "Long label b2"
        ],
        vstack [
          label "Label c1",
          label "Label c2",
          label "Label c3"
        ]
      ]
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv zstackNode

getSizeReqItemsFixed :: Spec
getSizeReqItemsFixed = describe "several items, horizontal" $ do
  it "should return width = Fixed 300" $
    sizeReqW `shouldBe` FixedSize 300

  it "should return height = Fixed 40" $
    sizeReqH `shouldBe` FixedSize 40

  where
    wenv = mockWenv ()
    zstackNode = zstack [
        vstack [
          label "Label a1",
          label "Label a2"
        ],
        vstack [
          label "Long b1",
          label "Long b2"
        ] `style` [width 300]
      ]
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv zstackNode

resize :: Spec
resize = describe "resize" $ do
  resizeEmpty
  resizeItems

resizeEmpty :: Spec
resizeEmpty = describe "empty" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should not have children" $
    children `shouldSatisfy` Seq.null

  where
    wenv = mockWenv ()
    vp = Rect 0 0 640 480
    zstackNode = zstack []
    newNode = nodeInit wenv zstackNode
    viewport = newNode ^. L.info . L.viewport
    children = newNode ^. L.children

resizeItems :: Spec
resizeItems = describe "several items, horizontal" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign the same viewport size to each children" $
    childrenRa `shouldBe` Seq.fromList [vp, vp, vp]

  where
    wenv = mockWenv ()
    vp   = Rect 0 0 640 480
    zstackNode = zstack [
        label "Label 1",
        label "Label Number Two",
        label "Label 3"
      ]
    newNode = nodeInit wenv zstackNode
    viewport = newNode ^. L.info . L.viewport
    childrenRa = (^. L.info . L.viewport) <$> newNode ^. L.children
