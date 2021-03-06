{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.CompositeSpec (spec) where

import Control.Lens ((&), (^.), (^?), (^..), (.~), (%~), _Just, ix, traverse)
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Data.Typeable (cast)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestEventUtil
import Monomer.TestUtil
import Monomer.Widgets.Composite
import Monomer.Widgets.Containers.Box
import Monomer.Widgets.Containers.Grid
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Containers.ZStack
import Monomer.Widgets.Singles.Button
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.TextField
import Monomer.Widgets.Util.Widget

import qualified Monomer.Lens as L
import qualified Monomer.Widgets.Single as SG

data MainEvt
  = MainBtnClicked
  | ChildClicked
  | MainResize Rect
  deriving (Eq, Show)

data ChildEvt
  = ChildBtnClicked
  | ChildMessage String
  | ChildResize Rect
  deriving (Eq, Show)

data MainModel = MainModel {
  _tmClicks :: Int,
  _tmChild :: ChildModel
} deriving (Eq, Show)

instance Default MainModel where
  def = MainModel {
    _tmClicks = 0,
    _tmChild = def
  }

instance WidgetModel MainModel

data ChildModel = ChildModel {
  _cmClicks :: Int,
  _cmMessage :: String
} deriving (Eq, Show)

instance WidgetModel ChildModel

instance Default ChildModel where
  def = ChildModel {
    _cmClicks = 0,
    _cmMessage = ""
  }

data TestModel = TestModel {
  _tmText1 :: Text,
  _tmText2 :: Text
} deriving (Eq, Show)

instance WidgetModel TestModel

msgWidget = defaultWidgetNode "msgWidget" $ SG.createSingle () def {
  SG.singleHandleMessage = msgWidgetHandleMessage
}

msgWidgetHandleMessage wenv target message node = Just (resultEvts node evts) where
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
  findByPoint
  findByPath
  findNextFocus
  getSizeReq
  resize

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  handleEventBasic
  handleEventChild
  handleEventResize
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
      -> [EventResponse MainModel MainEvt MainEvt]
    handleEvent wenv node model evt = case evt of
      MainBtnClicked -> [Model (model & clicks %~ (+1))]
      _ -> []
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

handleEventResize :: Spec
handleEventResize = describe "handleEventResize" $ do
  it "should not generate a resize event on init" $
    events [] `shouldBe` Seq.fromList []

  it "should generate a resize event when size changes" $
    events [evtClick (Point 10 10)] `shouldBe` Seq.fromList [MainResize cvp]

  where
    wenv = mockWenv def
    handleChild
      :: WidgetEnv ChildModel ChildEvt
      -> WidgetNode ChildModel ChildEvt
      -> ChildModel
      -> ChildEvt
      -> [EventResponse ChildModel ChildEvt MainEvt]
    handleChild wenv node model evt = case evt of
      ChildResize rect -> [Report (MainResize rect)]
      _ -> [Model (model & clicks %~ (+1))]
    buildChild wenv model = vstack [
        button "Click" ChildBtnClicked,
        label "Test" `style` [height 3000] `visible` (model ^. clicks > 0)
      ]
    handleEvent
      :: WidgetEnv MainModel MainEvt
      -> WidgetNode MainModel MainEvt
      -> MainModel
      -> MainEvt
      -> [EventResponse MainModel MainEvt MainEvt]
    handleEvent wenv node model evt = case evt of
      MainResize{} -> [Report evt]
      _ -> [Model (model & clicks %~ (+1))]
    buildUI wenv model = vstack [
        composite_ "child" child buildChild handleChild [onResize ChildResize]
      ]
    cmpNode = composite_ "main" id buildUI handleEvent [onResize MainResize]
    model es = nodeHandleEventModel wenv es cmpNode
    events es = nodeHandleEventEvts wenv es cmpNode
    vp = Rect 0 0 640 480
    cvp = Rect 0 0 640 3020

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
          textField text1 `localKey` "localTxt1"
        ],
        vstack [
          textField text1 `localKey` "localTxt2"
        ]
      ]
    buildUI2 wenv model = hstack [
        vstack [
          textField text1 `localKey` "localTxt2"
        ],
        vstack [
          textField text1 `localKey` "localTxt1"
        ]
      ]
    cmpNode1 = composite "main" id buildUI1 handleEvent
    cmpNode2 = composite_ "main" id buildUI2 handleEvent [mergeRequired (\_ _ -> True)]
    evts1 = [evtK keyTab, evtT "aacc", moveCharL, moveCharL]
    (wenv1, root1, _, _) = fst $ nodeHandleEvents wenv WInit evts1 cmpNode1
    cntNodeM = nodeMerge wenv1 root1 cmpNode2
    evts2 = [evtK keyTab, evtK keyTab, evtT "bb"]
    (wenv2, root2, _, _) = fst $ nodeHandleEvents wenv1 WNoInit evts2 cntNodeM
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
          textField text1 `key` "globalTxt1"
        ],
        vstack [
          textField text2 `key` "globalTxt2"
        ]
      ]
    buildUI2 wenv model = hstack [
        vstack [
          textField text2 `key` "globalTxt2"
        ],
        vstack [
          textField text1 `key` "globalTxt1"
        ]
      ]
    cmpNode1 = composite "main" id buildUI1 handleEvent
    cmpNode2 = composite_ "main" id buildUI2 handleEvent [mergeRequired (\_ _ -> True)]
    evts1 = [evtT "aacc", moveCharL, moveCharL]
    (wenv1, root1, _, _) = fst $ nodeHandleEvents wenv WInit evts1 cmpNode1
    cntNodeM = nodeMerge wenv1 root1 cmpNode2
    evts2 = [evtK keyTab, evtK keyTab, evtT "bb"]
    (wenv2, root2, _, _) = fst $ nodeHandleEvents wenv1 WNoInit evts2 cntNodeM
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
    handleEvent wenv node model evt = [Request (SendMessage wid msg)] where
      wni = wenv ^. L.findByPath $ path
      wid = maybe def (^. L.widgetId) wni
    buildUI wenv model = vstack [
        button "Start" MainBtnClicked,
        composite "child" child buildChild handleChild
      ]
    cmpNode = composite "main" id buildUI handleEvent
    model es = nodeHandleEventModel wenv es cmpNode

findByPoint :: Spec
findByPoint = describe "findByPoint" $ do
  it "should return Nothing" $
    wni emptyPath (Point 3000 3000) `shouldBe` Nothing

  it "should return item number 5" $ do
    let res = wni emptyPath (Point 320 240)
    res ^? _Just . L.path ^.. traverse . traverse `shouldBe` [0, 0, 0, 0, 1, 1]

  it "should return item number 8" $ do
    let res = wni emptyPath (Point 600 240)
    res ^? _Just . L.path ^.. traverse . traverse `shouldBe` [0, 0, 0, 0, 1, 2]

  it "should return background item" $ do
    let res = wni emptyPath (Point 560 400)
    res ^? _Just . L.path ^.. traverse . traverse `shouldBe` [0, 0, 0, 1]

  it "should return item number 9 when starting from the second level" $ do
    let res = wni (Seq.fromList [0, 0]) (Point 560 340)
    res ^? _Just . L.path ^.. traverse . traverse `shouldBe` [0, 0, 0, 0, 2, 2, 0, 1]

  it "should return Nothing if start path is not valid" $ do
    let res = wni (Seq.fromList [0, 1]) (Point 600 400)
    res `shouldBe` Nothing

  where
    wenv = mockWenvEvtUnit (TestModel "" "")
    cmpNode = findByHelperUI
    wni start point = res where
      inode = nodeInit wenv cmpNode
      res = widgetFindByPoint (inode ^. L.widget) wenv start point inode

findByPath :: Spec
findByPath = describe "findByPath" $ do
  it "should return Nothing" $ do
    wni emptyPath `shouldBe` Nothing
    wni (Seq.fromList [1]) `shouldBe` Nothing
    wni (Seq.fromList [0, 100]) `shouldBe` Nothing

  it "should return item number 5" $ do
    let res = wni (Seq.fromList [0, 0, 0, 0, 1, 1])
    roundRectUnits <$> res ^? _Just . L.viewport `shouldBe` Just (Rect 213 160 213 160)

  it "should return item number 8" $ do
    let res = wni (Seq.fromList [0, 0, 0, 0, 1, 2])
    roundRectUnits <$> res ^? _Just . L.viewport `shouldBe` Just (Rect 427 160 213 160)

  it "should return background item" $ do
    let res = wni (Seq.fromList [0, 0, 0, 1])
    roundRectUnits <$> res ^? _Just . L.viewport `shouldBe` Just (Rect 0 0 640 480)

  it "should return item number 9 when starting from the second level" $ do
    let res = wni (Seq.fromList [0, 0, 0, 0, 2, 2, 0, 1])
    roundRectUnits <$> res ^? _Just . L.viewport `shouldBe` Just (Rect 427 340 213 20)

  where
    wenv = mockWenvEvtUnit (TestModel "" "")
    cmpNode = findByHelperUI
    wni path = res where
      inode = nodeInit wenv cmpNode
      res = widgetFindByPath (inode ^. L.widget) wenv path inode

findNextFocus :: Spec
findNextFocus = describe "findNextFocus" $ do
  it "should return the first textfield" $ do
    let res = wni FocusFwd emptyPath
    roundRectUnits <$> res ^? _Just . L.viewport `shouldBe` Just (Rect 0 0 213 160)
    res ^? _Just . L.path `shouldBe` Just (Seq.fromList [0, 0, 0, 0, 0, 0])

  it "should return the second textfield" $ do
    let res = wni FocusFwd (Seq.fromList [0, 0, 0, 0, 0, 0])
    roundRectUnits <$> res ^? _Just . L.viewport `shouldBe` Just (Rect 213 160 213 160)
    res ^? _Just . L.path `shouldBe` Just (Seq.fromList [0, 0, 0, 0, 1, 1])

  it "should return the third textfield" $ do
    let res = wni FocusFwd (Seq.fromList [0, 0, 0, 0, 1, 1])
    roundRectUnits <$> res ^? _Just . L.viewport `shouldBe` Just (Rect 427 360 213 20)
    res ^? _Just . L.path `shouldBe` Just (Seq.fromList [0, 0, 0, 0, 2, 2, 0, 2])

  it "should return the third textfield (starts backwards)" $ do
    let res = wni FocusBwd emptyPath
    roundRectUnits <$> res ^? _Just . L.viewport `shouldBe` Just (Rect 427 360 213 20)
    res ^? _Just . L.path `shouldBe` Just (Seq.fromList [0, 0, 0, 0, 2, 2, 0, 2])

  it "should return the second textfield (starts backwards)" $ do
    let res = wni FocusBwd (Seq.fromList [0, 0, 0, 0, 2, 2, 0, 2])
    roundRectUnits <$> res ^? _Just . L.viewport `shouldBe` Just (Rect 213 160 213 160)
    res ^? _Just . L.path `shouldBe` Just (Seq.fromList [0, 0, 0, 0, 1, 1])

  it "should return the first textfield (starts backwards)" $ do
    let res = wni FocusBwd (Seq.fromList [0, 0, 0, 0, 1, 1])
    roundRectUnits <$> res ^? _Just . L.viewport `shouldBe` Just (Rect 0 0 213 160)
    res ^? _Just . L.path `shouldBe` Just (Seq.fromList [0, 0, 0, 0, 0, 0])

  where
    wenv = mockWenvEvtUnit (TestModel "" "")
    cmpNode = findByHelperUI
    wni dir start = res where
      inode = nodeInit wenv cmpNode
      res = widgetFindNextFocus (inode ^. L.widget) wenv dir start inode

findByHelperUI :: WidgetNode TestModel ep
findByHelperUI = composite "main" id buildUI handleEvent where
  handleEvent wenv node model evt = []
  buildLabels :: WidgetEnv TestModel () -> TestModel -> WidgetNode TestModel ()
  buildLabels wenv model = vstack_ [ignoreEmptyArea] [
      label "a", label "b", textField text1
    ]
  cmpLabels = composite "main" id buildLabels handleEvent
  buildUI :: WidgetEnv TestModel () -> TestModel -> WidgetNode TestModel ()
  buildUI wenv model = box_ [ignoreEmptyArea, expandContent] $
    zstack_ [onlyTopActive False] [
      label "Background",
      vgrid [
          hgrid [ textField text1, label "2", label "3" ],
          hgrid [ label "4", textField text1, label "6" ],
          hgrid [ label "7", label "8", cmpLabels ]
        ]
    ]

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return width = Fixed 70" $
    sizeReqW `shouldBe` fixedSize 70

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` fixedSize 40

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
