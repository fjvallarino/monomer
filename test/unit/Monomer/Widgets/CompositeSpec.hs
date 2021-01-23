{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.CompositeSpec (spec) where

import Codec.Serialise
import Control.Lens ((&), (^.), (^?), (.~), (%~), ix)
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Data.Typeable (cast)
import GHC.Generics
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestEventUtil
import Monomer.TestUtil
import Monomer.Widgets.Button
import Monomer.Widgets.Composite
import Monomer.Widgets.Label
import Monomer.Widgets.TextField
import Monomer.Widgets.Stack
import Monomer.Widgets.Util.Widget

import qualified Monomer.Lens as L
import qualified Monomer.Widgets.Single as SG

data MainEvt
  = MainBtnClicked
  | ChildClicked
  deriving (Eq, Show)

data ChildEvt
  = ChildBtnClicked
  | ChildMessage String
  deriving (Eq, Show)

data MainModel = MainModel {
  _tmClicks :: Int,
  _tmChild :: ChildModel
} deriving (Eq, Show, Generic, Serialise)

instance Default MainModel where
  def = MainModel {
    _tmClicks = 0,
    _tmChild = def
  }

data ChildModel = ChildModel {
  _cmClicks :: Int,
  _cmMessage :: String
} deriving (Eq, Show, Generic, Serialise)

instance Default ChildModel where
  def = ChildModel {
    _cmClicks = 0,
    _cmMessage = ""
  }

data TestModel = TestModel {
  _tmText1 :: Text,
  _tmText2 :: Text
} deriving (Eq, Show, Generic, Serialise)

msgWidget = defaultWidgetNode "msgWidget" $ SG.createSingle () def {
  SG.singleHandleMessage = msgWidgetHandleMessage
}

msgWidgetHandleMessage wenv ctx message node = Just (resultEvts node evts) where
  val = fromMaybe "" (cast message)
  evts = [ChildMessage val]

makeLensesWith abbreviatedFields ''MainModel
makeLensesWith abbreviatedFields ''ChildModel
makeLensesWith abbreviatedFields ''TestModel

baseLens idx = L.children . ix 0 . L.children . ix idx . L.children . ix 0
pathLens idx = baseLens idx . L.info . L.path
widLens idx = baseLens idx . L.info . L.widgetId

spec :: Spec
spec = describe "Composite" $ do
  handleEvent
  handleMessage
  getSizeReq
  resize

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  handleEventBasic
  handleEventChild
  handleEventLocalKey
  handleEventGlobalKey

handleEventBasic :: Spec
handleEventBasic = describe "handleEventBasic" $ do
  it "should not generate an event if clicked outside" $
    model [evtClick (Point 3000 3000)] ^. clicks `shouldBe` 0

  it "should generate a user provided event when clicked" $
    model [evtClick (Point 10 10)] ^. clicks `shouldBe` 1

  where
    wenv = mockWenv def
    handleEvent
      :: WidgetEnv MainModel MainEvt
      -> WidgetNode MainModel MainEvt
      -> MainModel
      -> MainEvt
      -> [EventResponse MainModel MainEvt ()]
    handleEvent wenv node model evt = [Model (model & clicks %~ (+1))]
    buildUI wenv model = button "Click" MainBtnClicked
    cmpNode = composite "main" id buildUI handleEvent
    model es = nodeHandleEventModel wenv es cmpNode

handleEventChild :: Spec
handleEventChild = describe "handleEventChild" $ do
  it "should not generate an event if clicked outside" $ do
    model [evtClick (Point 3000 3000)] ^. clicks `shouldBe` 0
    model [evtClick (Point 3000 3000)] ^. child . clicks `shouldBe` 0

  it "should generate a main event when clicked in main button" $ do
    model [evtClick (Point 10 10)] ^. clicks `shouldBe` 1
    model [evtClick (Point 10 10)] ^. child . clicks `shouldBe` 0

  it "should generate a child event when clicked in child button" $ do
    model [evtClick (Point 10 30)] ^. clicks `shouldBe` 0
    model [evtClick (Point 10 30)] ^. child . clicks `shouldBe` 1

  where
    wenv = mockWenv def
    handleChild
      :: WidgetEnv ChildModel ChildEvt
      -> WidgetNode ChildModel ChildEvt
      -> ChildModel
      -> ChildEvt
      -> [EventResponse ChildModel ChildEvt MainEvt]
    handleChild wenv node model evt = [Model (model & clicks %~ (+1))]
    buildChild wenv model = button "Click" ChildBtnClicked
    handleEvent
      :: WidgetEnv MainModel MainEvt
      -> WidgetNode MainModel MainEvt
      -> MainModel
      -> MainEvt
      -> [EventResponse MainModel MainEvt ()]
    handleEvent wenv node model evt = [Model (model & clicks %~ (+1))]
    buildUI wenv model = vstack [
        button "Click" MainBtnClicked,
        composite "child" child buildChild handleChild
      ]
    cmpNode = composite "main" id buildUI handleEvent
    model es = nodeHandleEventModel wenv es cmpNode

handleEventLocalKey :: Spec
handleEventLocalKey = describe "handleEventLocalKey" $
  it "should insert new text at the end, since its merged with a local key" $ do
    wenv1 ^. L.model . text1 `shouldBe` "aacc"
    wenv1 ^. L.model . text2 `shouldBe` ""
    wenv2 ^. L.model . text1 `shouldBe` "aaccbb"
    wenv2 ^. L.model . text2 `shouldBe` ""
    newInstRoot ^? pathLens 0 `shouldBe` Just (Seq.fromList [0, 0, 0, 0])
    newInstRoot ^? pathLens 1 `shouldBe` Just (Seq.fromList [0, 0, 1, 0])
    newInstRoot ^? widLens 0 `shouldBe` Just (WidgetId 0 (Seq.fromList [0, 0, 0, 0]))
    newInstRoot ^? widLens 1 `shouldBe` Just (WidgetId 0 (Seq.fromList [0, 0, 1, 0]))

  where
    wenv = mockWenv (TestModel "" "")
    handleEvent
      :: WidgetEnv TestModel ()
      -> WidgetNode TestModel ()
      -> TestModel
      -> ()
      -> [EventResponse TestModel () ()]
    handleEvent wenv node model evt = []
    buildUI1 wenv model = hstack [
        vstack [
          textField text1 `key` "localTxt1"
        ],
        vstack [
          textField text1 `key` "localTxt2"
        ]
      ]
    buildUI2 wenv model = hstack [
        vstack [
          textField text1 `key` "localTxt2"
        ],
        vstack [
          textField text1 `key` "localTxt1"
        ]
      ]
    cmpNode1 = composite "main" id buildUI1 handleEvent
    cmpNode2 = composite_ "main" id buildUI2 handleEvent [mergeRequired (\_ _ -> True)]
    evts1 = [evtK keyTab, evtT "aacc", moveCharL, moveCharL]
    ((wenv1, _, root1), ctx1) = nodeHandleEvents wenv evts1 cmpNode1
    cntNodeM = nodeMerge wenv1 root1 cmpNode2
    evts2 = [evtK keyTab, evtK keyTab, evtT "bb"]
    ((wenv2, _, root2), ctx2) = nodeHandleEventsNoInit wenv1 evts2 cntNodeM
    newInstRoot = widgetSave (root2 ^. L.widget) wenv1 root2

handleEventGlobalKey :: Spec
handleEventGlobalKey = describe "handleEventGlobalKey" $
  it "should insert new text at the correct location, since its merged with a global key" $ do
    wenv1 ^. L.model . text1 `shouldBe` "aacc"
    wenv1 ^. L.model . text2 `shouldBe` ""
    wenv2 ^. L.model . text1 `shouldBe` "aabbcc"
    wenv2 ^. L.model . text2 `shouldBe` ""
    newInstRoot ^? pathLens 0 `shouldBe` Just (Seq.fromList [0, 0, 0, 0])
    newInstRoot ^? pathLens 1 `shouldBe` Just (Seq.fromList [0, 0, 1, 0])
    newInstRoot ^? widLens 0 `shouldBe` Just (WidgetId 0 (Seq.fromList [0, 0, 1, 0]))
    newInstRoot ^? widLens 1 `shouldBe` Just (WidgetId 0 (Seq.fromList [0, 0, 0, 0]))

  where
    wenv = mockWenv (TestModel "" "")
    handleEvent
      :: WidgetEnv TestModel ()
      -> WidgetNode TestModel ()
      -> TestModel
      -> ()
      -> [EventResponse TestModel () ()]
    handleEvent wenv node model evt = []
    buildUI1 wenv model = hstack [
        vstack [
          textField text1 `globalKey` "globalTxt1"
        ],
        vstack [
          textField text2 `globalKey` "globalTxt2"
        ]
      ]
    buildUI2 wenv model = hstack [
        vstack [
          textField text2 `globalKey` "globalTxt2"
        ],
        vstack [
          textField text1 `globalKey` "globalTxt1"
        ]
      ]
    cmpNode1 = composite "main" id buildUI1 handleEvent
    cmpNode2 = composite_ "main" id buildUI2 handleEvent [mergeRequired (\_ _ -> True)]
    evts1 = [evtT "aacc", moveCharL, moveCharL]
    ((wenv1, _, root1), ctx1) = nodeHandleEvents wenv evts1 cmpNode1
    cntNodeM = nodeMerge wenv1 root1 cmpNode2
    evts2 = [evtK keyTab, evtK keyTab, evtT "bb"]
    ((wenv2, _, root2), ctx2) = nodeHandleEventsNoInit wenv1 evts2 cntNodeM
    newInstRoot = widgetSave (root2 ^. L.widget) wenv1 root2

handleMessage :: Spec
handleMessage = describe "handleMessage" $ do
  it "should not generate an event if clicked outside" $ do
    model [evtClick (Point 50 10)] ^. child . message `shouldBe` "Test"

  where
    wenv = mockWenv def
    msg :: String
    msg = "Test"
    path = Seq.fromList [0, 0, 1, 0, 0]
    handleChild
      :: WidgetEnv ChildModel ChildEvt
      -> WidgetNode ChildModel ChildEvt
      -> ChildModel
      -> ChildEvt
      -> [EventResponse ChildModel ChildEvt MainEvt]
    handleChild wenv node model evt = case evt of
      ChildMessage m -> [Model (model & message .~ m)]
      _ -> []
    buildChild wenv model = vstack [ msgWidget ]
    handleEvent
      :: WidgetEnv MainModel MainEvt
      -> WidgetNode MainModel MainEvt
      -> MainModel
      -> MainEvt
      -> [EventResponse MainModel MainEvt ()]
    handleEvent wenv node model evt = [Request (SendMessage path msg)]
    buildUI wenv model = vstack [
        button "Start" MainBtnClicked,
        composite "child" child buildChild handleChild
      ]
    cmpNode = composite "main" id buildUI handleEvent
    model es = nodeHandleEventModel wenv es cmpNode

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return width = Flex 70 0.01" $
    sizeReqW `shouldBe` FlexSize 70 0.01

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` FixedSize 40

  where
    wenv = mockWenv ()
    handleEvent wenv node model evt = []
    buildUI :: WidgetEnv () () -> () -> WidgetNode () ()
    buildUI wenv model = vstack [
        label "label 1",
        label "label 2"
      ]
    cmpNode = composite "main" id buildUI handleEvent
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv cmpNode

resize :: Spec
resize = describe "resize" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign the same viewport size to its child" $
    childrenVp `shouldBe` Seq.singleton cvp1

  it "should assign the same renderArea size to its child" $
    childrenRa `shouldBe` Seq.singleton cvp1

  where
    wenv = mockWenv () & L.windowSize .~ Size 640 480
    vp   = Rect 0 0 640 480
    cvp1 = Rect 0 0 640 480
    handleEvent wenv node model evt = []
    buildUI :: WidgetEnv () () -> () -> WidgetNode () ()
    buildUI wenv model = hstack []
    cmpNode = composite "main" id buildUI handleEvent
    tmpNode = nodeInit wenv cmpNode
    newNode = widgetSave (tmpNode ^. L.widget) wenv tmpNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = (^. L.info . L.viewport) <$> newNode ^. L.children
    childrenRa = (^. L.info . L.renderArea) <$> newNode ^. L.children
