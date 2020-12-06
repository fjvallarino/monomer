module Monomer.Widgets.BoxSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Box
import Monomer.Widgets.Button
import Monomer.Widgets.Label

import qualified Monomer.Lens as L

data BtnEvent
  = BtnClick
  deriving (Eq, Show)

spec :: Spec
spec = describe "Box" $ do
  handleEvent
  updateSizeReq
  resize

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should not generate an event if clicked outside" $
    events (Point 3000 3000) `shouldBe` Seq.empty

  it "should generate an event if the button (centered) is clicked" $
    events (Point 320 240) `shouldBe` Seq.singleton BtnClick

  where
    wenv = mockWenv ()
    btn = button "Click" BtnClick
    boxNode = nodeInit wenv (box btn)
    events p = nodeHandleEventEvts wenv [Click p LeftBtn] boxNode

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  it "should return width = Flex 50 0.01" $
    sizeReqW `shouldBe` FlexSize 50 0.01

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` FixedSize 20

  where
    wenv = mockWenv ()
    boxNode = box (label "Label")
    (sizeReqW, sizeReqH) = nodeUpdateSizeReq wenv boxNode

resize :: Spec
resize = describe "resize" $ do
  resizeDefault
  resizeExpand
  resizeAlign

resizeDefault :: Spec
resizeDefault = describe "default" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should have one child" $
    children `shouldSatisfy` (== 1) . Seq.length

  it "should have its children assigned a viewport" $
    cViewport `shouldBe` cvp

  where
    wenv = mockWenv ()
    vp  = Rect   0   0 640 480
    cvp = Rect 295 230  50  20
    boxNode = box (label "Label")
    newNode = nodeInit wenv boxNode
    children = newNode ^. L.children
    viewport = newNode ^. L.widgetInstance . L.viewport
    cViewport = getChildVp wenv []

resizeExpand :: Spec
resizeExpand = describe "expand" $
  it "should have its children assigned a viewport" $
    cViewport `shouldBe` vp

  where
    wenv = mockWenv ()
    vp  = Rect   0   0 640 480
    cViewport = getChildVp wenv [expandContent]

resizeAlign :: Spec
resizeAlign = describe "align" $ do
  it "should align its child left" $
    childVpL `shouldBe` cvpl

  it "should align its child right" $
    childVpR `shouldBe` cvpr

  it "should align its child top" $
    childVpT `shouldBe` cvpt

  it "should align its child bottom" $
    childVpB `shouldBe` cvpb

  it "should align its child top-left" $
    childVpTL `shouldBe` cvplt

  it "should align its child bottom-right" $
    childVpBR `shouldBe` cvpbr

  where
    wenv = mockWenv ()
    cvpl  = Rect   0 230 50 20
    cvpr  = Rect 590 230 50 20
    cvpt  = Rect 295   0 50 20
    cvpb  = Rect 295 460 50 20
    cvplt = Rect   0   0 50 20
    cvpbr = Rect 590 460 50 20
    childVpL = getChildVp wenv [alignLeft]
    childVpR = getChildVp wenv [alignRight]
    childVpT = getChildVp wenv [alignTop]
    childVpB = getChildVp wenv [alignBottom]
    childVpTL = getChildVp wenv [alignTop, alignLeft]
    childVpBR = getChildVp wenv [alignBottom, alignRight]

getChildVp :: WidgetEnv s e -> [BoxCfg s e] -> Rect
getChildVp wenv cfgs = childLC ^. L.widgetInstance . L.viewport where
  lblNode = label "Label"
  boxNodeLC = nodeInit wenv (box_ lblNode cfgs)
  childLC = Seq.index (boxNodeLC ^. L.children) 0
