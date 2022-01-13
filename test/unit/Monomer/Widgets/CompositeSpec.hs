{-|
Module      : Monomer.Widgets.CompositeSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for Composite widget.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.CompositeSpec (spec) where

import Control.Lens (
  (&), (^.), (^?), (^?!), (^..), (.~), (%~), _Just, ix, folded, traverse, dropping)
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Data.Foldable (toList)
import Data.Maybe
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import TextShow
import Test.Hspec

import qualified Data.Map as Map
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
import Monomer.Widgets.Singles.Checkbox
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.TextField
import Monomer.Widgets.Util.Widget

import qualified Monomer.Lens as L
import qualified Monomer.Widgets.Single as SG

data MainEvt
  = MainBtnClicked
  | ChildClicked
  | MainResize Rect
  | OnInit
  | OnDispose
  | OnChange MainModel
  deriving (Eq, Show)

data ChildEvt
  = ChildBtnClicked
  | ChildMessage String
  | ChildResize Rect
  deriving (Eq, Show)

data DeepEvt
  = DeepInit
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

data ChildModel = ChildModel {
  _cmClicks :: Int,
  _cmMessage :: String,
  _cmDeep :: DeepModel
} deriving (Eq, Show)

instance Default ChildModel where
  def = ChildModel {
    _cmClicks = 0,
    _cmMessage = "",
    _cmDeep = def
  }

newtype DeepModel = DeepModel {
  _dmClicked :: Bool
} deriving (Eq, Show)

instance Default DeepModel where
  def = DeepModel {
    _dmClicked = False
  }

data TestModel = TestModel {
  _tmItems :: [Int],
  _tmText1 :: Text,
  _tmText2 :: Text
} deriving (Eq, Show)

instance Default TestModel where
  def = TestModel {
    _tmItems = [],
    _tmText1 = "",
    _tmText2 = ""
  }

msgWidget = defaultWidgetNode "msgWidget" $ SG.createSingle () def {
  SG.singleHandleMessage = msgWidgetHandleMessage
}

msgWidgetHandleMessage wenv node target message = Just (resultEvts node evts) where
  val = fromMaybe "" (cast message)
  evts = [ChildMessage val]

makeLensesWith abbreviatedFields ''MainModel
makeLensesWith abbreviatedFields ''ChildModel
makeLensesWith abbreviatedFields ''DeepModel
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
  handleEventOnInit
  handleEventOnDispose
  handleEventOnChange
  handleEventNewRoot
  handleEventChild
  handleEventResize
  handleEventLocalKey

handleEventBasic :: Spec
handleEventBasic = describe "handleEventBasic" $ do
  it "should not generate an event if clicked outside" $ do
    model [evtClick (Point 3000 3000)] ^. clicks `shouldBe` 0
    reqs [evtClick (Point 3000 3000)] `shouldBe` Seq.Empty

  it "should generate a user provided event when clicked" $ do
    model [evtClick (Point 10 10)] ^. clicks `shouldBe` 1
    reqs [evtClick (Point 10 10)] `shouldSatisfy` (== 3) . length

  where
    wenv = mockWenv def
    handleEvent
      :: WidgetEnv MainModel MainEvt
      -> WidgetNode MainModel MainEvt
      -> MainModel
      -> MainEvt
      -> [EventResponse MainModel MainEvt MainModel MainEvt]
    handleEvent wenv node model evt = case evt of
      MainBtnClicked -> [Model (model & clicks %~ (+1))]
      _ -> []
    buildUI wenv model = button "Click" MainBtnClicked
    cmpNode = composite "main" id buildUI handleEvent
    model es = nodeHandleEventModel wenv es cmpNode
    reqs es = nodeHandleEventReqs wenv es cmpNode

handleEventOnInit :: Spec
handleEventOnInit = describe "handleEventOnInit" $ do
  it "should generate an init event" $ do
    evts [] `shouldBe` Seq.singleton OnInit

  where
    wenv = mockWenv def
    handleEvent
      :: WidgetEnv MainModel MainEvt
      -> WidgetNode MainModel MainEvt
      -> MainModel
      -> MainEvt
      -> [EventResponse MainModel MainEvt MainModel MainEvt]
    handleEvent wenv node model evt = case evt of
      OnInit{} -> [Report evt]
      _ -> []
    buildUI wenv model = vstack []
    cmpNode = composite_ "main" id buildUI handleEvent [onInit OnInit]
    evts es = nodeHandleEventEvts_ wenv WInitKeepFirst es cmpNode

handleEventOnDispose :: Spec
handleEventOnDispose = describe "handleEventOnDispose" $ do
  it "should generate an init event" $ do
    let val = case evts [] ^?! L.requests . ix 1 of
          SendMessage wid msg -> cast msg
          _ -> Nothing

    val `shouldBe` Just OnDispose

  where
    wenv = mockWenv def
    handleEvent
      :: WidgetEnv MainModel MainEvt
      -> WidgetNode MainModel MainEvt
      -> MainModel
      -> MainEvt
      -> [EventResponse MainModel MainEvt MainModel MainEvt]
    handleEvent wenv node model evt = case evt of
      OnInit{} -> [Report evt]
      _ -> []
    buildUI wenv model = vstack []
    cmpNode = nodeInit wenv
      $ composite_ "main" id buildUI handleEvent [onDispose OnDispose]
    evts es = widgetDispose (cmpNode ^. L.widget) wenv cmpNode

handleEventOnChange :: Spec
handleEventOnChange = describe "handleEventOnChange" $ do
  it "should not generate an event if model did not change" $ do
    evts [evtClick (Point 10 10)] `shouldBe` Seq.empty

  it "should generate an event if model changed" $ do
    let items = toList $ evts [evtClick (Point 10 30)]

    [i | i@OnChange{} <- items] `shouldSatisfy` not . null

  where
    wenv = mockWenv def
    handleEvent
      :: WidgetEnv MainModel MainEvt
      -> WidgetNode MainModel MainEvt
      -> MainModel
      -> MainEvt
      -> [EventResponse MainModel MainEvt MainModel MainEvt]
    handleEvent wenv node model evt = case evt of
      MainBtnClicked -> [Model (model & clicks %~ (+1))]
      ChildClicked -> [Model model]
      OnChange{} -> [Report evt]
      _ -> []
    buildUI wenv model = vstack [
        button "Click secondary" ChildClicked,
        button "Click main" MainBtnClicked
      ]
    cmpNode = composite_ "main" id buildUI handleEvent [onChange OnChange]
    evts es = nodeHandleEventEvts wenv es cmpNode

handleEventNewRoot :: Spec
handleEventNewRoot = describe "handleEventNewRoot" $ do
  it "should generate a resize request when the widgetType of the root widget changes" $ do
    reqs [evtClick (Point 10 10)] `shouldSatisfy` (== 4) . length
    reqs [evtClick (Point 10 10)] `shouldSatisfy` (== 1) . length . Seq.filter isResizeWidgets

  where
    wenv = mockWenv def
    handleEvent
      :: WidgetEnv MainModel MainEvt
      -> WidgetNode MainModel MainEvt
      -> MainModel
      -> MainEvt
      -> [EventResponse MainModel MainEvt MainModel MainEvt]
    handleEvent wenv node model evt = case evt of
      MainBtnClicked -> [Model (model & clicks %~ (+1))]
      _ -> []
    buildUI wenv model
      | even (model ^. clicks) = button "Click" MainBtnClicked
      | otherwise = label "Test"
    cmpNode = composite "main" id buildUI handleEvent
    model es = nodeHandleEventModel wenv es cmpNode
    reqs es = nodeHandleEventReqs wenv es cmpNode

handleEventChild :: Spec
handleEventChild = describe "handleEventChild" $ do
  it "should not generate an event if clicked outside" $ do
    model [evtClick (Point 3000 3000)] ^. clicks `shouldBe` 0
    model [evtClick (Point 3000 3000)] ^. child . clicks `shouldBe` 0
    model [evtClick (Point 3000 3000)] ^. child . deep . clicked `shouldBe` False

  it "should generate a main event when clicked in main button" $ do
    model [evtClick (Point 10 10)] ^. clicks `shouldBe` 1
    model [evtClick (Point 10 10)] ^. child . clicks `shouldBe` 0
    model [evtClick (Point 10 10)] ^. child . deep . clicked `shouldBe` False

  it "should generate a child event when clicked in child button" $ do
    model [evtClick (Point 10 30)] ^. clicks `shouldBe` 0
    model [evtClick (Point 10 30)] ^. child . clicks `shouldBe` 1
    model [evtClick (Point 10 30)] ^. child . deep . clicked `shouldBe` False

  it "should update the nested model when the checkbox is clicked" $ do
    model [evtClick (Point 10 50)] ^. clicks `shouldBe` 0
    model [evtClick (Point 10 50)] ^. child . clicks `shouldBe` 0
    model [evtClick (Point 10 50)] ^. child . deep . clicked `shouldBe` True

  where
    wenv = mockWenv def

    handleDeep
      :: WidgetEnv DeepModel DeepEvt
      -> WidgetNode DeepModel DeepEvt
      -> DeepModel
      -> DeepEvt
      -> [EventResponse DeepModel DeepEvt ChildModel ChildEvt]
    handleDeep wenv node model evt = []
    buildDeep wenv model = checkbox clicked

    handleChild
      :: WidgetEnv ChildModel ChildEvt
      -> WidgetNode ChildModel ChildEvt
      -> ChildModel
      -> ChildEvt
      -> [EventResponse ChildModel ChildEvt MainModel MainEvt]
    handleChild wenv node model evt = [Model (model & clicks %~ (+1))]
    buildChild wenv model = vstack [
        button "Click" ChildBtnClicked,
        composite "deep" deep buildDeep handleDeep
      ]

    handleEvent
      :: WidgetEnv MainModel MainEvt
      -> WidgetNode MainModel MainEvt
      -> MainModel
      -> MainEvt
      -> [EventResponse MainModel MainEvt MainModel ()]
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
      -> [EventResponse ChildModel ChildEvt MainModel MainEvt]
    handleChild wenv node model evt = case evt of
      ChildResize rect -> [Report (MainResize rect)]
      _ -> [Model (model & clicks %~ (+1))]
    buildChild wenv model = vstack [
        button "Click" ChildBtnClicked,
        label "Test" `styleBasic` [height 3000] `nodeVisible` (model ^. clicks > 0)
      ]
    handleEvent
      :: WidgetEnv MainModel MainEvt
      -> WidgetNode MainModel MainEvt
      -> MainModel
      -> MainEvt
      -> [EventResponse MainModel MainEvt MainModel MainEvt]
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
handleEventLocalKey = describe "handleEventLocalKey" $ do
  handleEventLocalKeySingleState
  handleEventLocalKeyRemoveItem

handleEventLocalKeySingleState :: Spec
handleEventLocalKeySingleState = describe "handleEventLocalKeySingleState" $
  it "should insert new text at the end, since its merged with a local key" $ do
    wenv1 ^. L.model . text1 `shouldBe` "aacc"
    wenv1 ^. L.model . text2 `shouldBe` ""
    wenv2 ^. L.model . text1 `shouldBe` "aaccbb"
    wenv2 ^. L.model . text2 `shouldBe` ""
    newInstRoot ^? pathLens 0 `shouldBe` Just (Seq.fromList [0, 0, 0, 0])
    newInstRoot ^? pathLens 1 `shouldBe` Just (Seq.fromList [0, 0, 1, 0])
    newInstRoot ^? widLens 0 `shouldBe` Just (WidgetId 0 (Seq.fromList [0, 0, 1, 0]))
    newInstRoot ^? widLens 1 `shouldBe` Just (WidgetId 0 (Seq.fromList [0, 0, 0, 0]))

  where
    wenv = mockWenv def
    handleEvent
      :: WidgetEnv TestModel ()
      -> WidgetNode TestModel ()
      -> TestModel
      -> ()
      -> [EventResponse TestModel () TestModel ()]
    handleEvent wenv node model evt = []
    buildUI1 wenv model = hstack [
        vstack [
          textField text1
        ] `nodeKey` "localTxt1",
        vstack [
          textField text1
        ] `nodeKey` "localTxt2"
      ]
    buildUI2 wenv model = hstack [
        vstack [
          textField text1
        ] `nodeKey` "localTxt2",
        vstack [
          textField text1
        ] `nodeKey` "localTxt1"
      ]
    cmpNode1 = composite "main" id buildUI1 handleEvent
    cmpNode2 = composite_ "main" id buildUI2 handleEvent [mergeRequired (\_ _ -> True)]
    evts1 = [evtK keyTab, evtT "aacc", moveCharL, moveCharL]
    (wenv1, root1, _) = fst $ nodeHandleEvents wenv WInit evts1 cmpNode1
    cntNodeM = nodeMerge wenv1 cmpNode2 root1
    evts2 = [evtK keyTab, evtK keyTab, evtT "bb"]
    (wenv2, root2, _) = fst $ nodeHandleEvents wenv1 WNoInit evts2 cntNodeM
    newInstRoot = widgetGetInstanceTree (root2 ^. L.widget) wenv1 root2

handleEventLocalKeyRemoveItem :: Spec
handleEventLocalKeyRemoveItem = describe "handleEventLocalKeyRemoveItem" $
  it "should remove an element and keep the correct keys" $ do
    getKeys oldInstRoot `shouldBe` ["key0", "key1", "key2", "key3"]
    getKeys newInstRoot `shouldBe` ["key0", "key2", "key3"]
    length (newCtx ^. L.widgetPaths) `shouldBe` 2

  where
    initModel = def & items .~ [0..3]
    wenv = mockWenv initModel
    handleEvent
      :: WidgetEnv TestModel MainEvt
      -> WidgetNode TestModel MainEvt
      -> TestModel
      -> MainEvt
      -> [EventResponse TestModel MainEvt TestModel MainEvt]
    handleEvent wenv node model evt = case evt of
      MainBtnClicked -> [Model $ model & items .~ [0, 2, 3]]
      _ -> []
    buildUI wenv model = vstack (button "Button" MainBtnClicked : (keyedLabel <$> model ^. items))
    keyedLabel idx = label "Test" `nodeKey` ("key" <> showt idx)
    node = composite "main" id buildUI handleEvent
    evts = [evtClick (Point 100 10)]
    oldNode = nodeInit wenv node
    ((_, newRoot, _), newCtx) = nodeHandleEvents wenv WInit evts node
    oldInstRoot = widgetGetInstanceTree (oldNode ^. L.widget) wenv oldNode
    newInstRoot = widgetGetInstanceTree (newRoot ^. L.widget) wenv newRoot
    getKeys inst = inst ^.. L.children . ix 0 . L.children . folded . L.info . L.key . _Just . L._WidgetKey

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
      -> [EventResponse ChildModel ChildEvt MainModel MainEvt]
    handleChild wenv node model evt = case evt of
      ChildMessage m -> [Model (model & message .~ m)]
      _ -> []
    buildChild wenv model = vstack [ msgWidget ]
    handleEvent
      :: WidgetEnv MainModel MainEvt
      -> WidgetNode MainModel MainEvt
      -> MainModel
      -> MainEvt
      -> [EventResponse MainModel MainEvt MainModel ()]
    handleEvent wenv node model evt = [Request (SendMessage wid msg)] where
      wnis = wenv ^. L.findBranchByPath $ path
      wid = maybe def (^. L.widgetId) (Seq.lookup (length wnis - 1) wnis)
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
    wenv = mockWenvEvtUnit def
    cmpNode = findByHelperUI
    wni start point = res where
      inode = nodeInit wenv cmpNode
      res = widgetFindByPoint (inode ^. L.widget) wenv inode start point

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
    wenv = mockWenvEvtUnit def
    cmpNode = findByHelperUI
    wni path = res where
      inode = nodeInit wenv cmpNode
      res = findWidgetByPath wenv inode path

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
    wenv = mockWenvEvtUnit def
    cmpNode = findByHelperUI
    wni dir start = res where
      inode = nodeInit wenv cmpNode
      res = widgetFindNextFocus (inode ^. L.widget) wenv inode dir start

findByHelperUI :: Typeable ep => WidgetNode TestModel ep
findByHelperUI = composite "main" id buildUI handleEvent where
  handleEvent wenv node model evt = []
  buildLabels :: WidgetEnv TestModel () -> TestModel -> WidgetNode TestModel ()
  buildLabels wenv model = vstack_ [ignoreEmptyArea] [
      label "a", label "b", textField text1
    ]
  cmpLabels = composite "main" id buildLabels handleEvent
  buildUI :: WidgetEnv TestModel () -> TestModel -> WidgetNode TestModel ()
  buildUI wenv model = box_ [ignoreEmptyArea, expandContent] $
    zstack_ [onlyTopActive_ False] [
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
    wenv = mockWenvEvtUnit ()
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
    wenv = mockWenvEvtUnit ()
      & L.windowSize .~ Size 640 480
    vp   = Rect 0 0 640 480
    cvp1 = Rect 0 0 640 480
    handleEvent wenv node model evt = []
    buildUI :: WidgetEnv () () -> () -> WidgetNode () ()
    buildUI wenv model = hstack []
    cmpNode = composite "main" id buildUI handleEvent
    tmpNode = nodeInit wenv cmpNode
    newNode = widgetGetInstanceTree (tmpNode ^. L.widget) wenv tmpNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = (^. L.info . L.viewport) <$> newNode ^. L.children
