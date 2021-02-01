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
import Monomer.Widgets.ZStack

import qualified Monomer.Lens as L

newtype BtnEvent
  = BtnClick Int
  deriving (Eq, Show)

spec :: Spec
spec = describe "Box" $ do
  handleEvent
  handleEventIgnoreEmpty
  handleEventSinkEmpty
  getSizeReq
  resize

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should not generate an event if clicked outside" $
    events (Point 3000 3000) `shouldBe` Seq.empty

  it "should generate an event if the button (centered) is clicked" $
    events (Point 320 240) `shouldBe` Seq.singleton (BtnClick 0)

  where
    wenv = mockWenv ()
    btn = button "Click" (BtnClick 0)
    boxNode = nodeInit wenv (box btn)
    events p = nodeHandleEventEvts wenv [Click p LeftBtn] boxNode

handleEventIgnoreEmpty :: Spec
handleEventIgnoreEmpty = describe "handleEventIgnoreEmpty" $ do
  it "should click the bottom layer, since nothing is handled on top" $
    clickIgnored (Point 200 15) `shouldBe` Seq.singleton (BtnClick 1)

  it "should click the top layer, since pointer is on the button" $
    clickIgnored (Point 320 240) `shouldBe` Seq.singleton (BtnClick 2)

  where
    wenv = mockWenv ()
    btn2 = button "Click 2" (BtnClick 2) `style` [height 10]
    ignoredNode = zstack_ [onlyTopActive False] [
        button "Click 1" (BtnClick 1),
        box_ [ignoreEmptyArea True] btn2
      ]
    clickIgnored p = nodeHandleEventEvts wenv [Click p LeftBtn] ignoredNode

handleEventSinkEmpty :: Spec
handleEventSinkEmpty = describe "handleEventSinkEmpty" $ do
  it "should do nothing, since event is not passed down" $
    clickSunk (Point 200 15) `shouldBe` Seq.empty

  it "should click the top layer, since pointer is on the button" $
    clickSunk (Point 320 240) `shouldBe` Seq.singleton (BtnClick 2)

  where
    wenv = mockWenv ()
    centeredBtn = button "Click 2" (BtnClick 2) `style` [height 10]
    sunkNode = zstack_ [onlyTopActive False] [
        button "Click 1" (BtnClick 1),
        box_ [ignoreEmptyArea False] centeredBtn
      ]
    clickSunk p = nodeHandleEventEvts wenv [Click p LeftBtn] sunkNode

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return width = Flex 50 0.01" $
    sizeReqW `shouldBe` FlexSize 50 0.01

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` FixedSize 20

  where
    wenv = mockWenv ()
    boxNode = box (label "Label")
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv boxNode

resize :: Spec
resize = describe "resize" $ do
  resizeDefault
  resizeExpand
  resizeAlign

resizeDefault :: Spec
resizeDefault = describe "default" $ do
  it "should have the provided renderArea size" $
    renderArea `shouldBe` vp

  it "should have one child" $
    children `shouldSatisfy` (== 1) . Seq.length

  it "should have its children assigned a renderArea" $
    cRenderArea `shouldBe` cvp

  where
    wenv = mockWenv ()
    vp  = Rect   0   0 640 480
    cvp = Rect 295 230  50  20
    boxNode = box (label "Label")
    newNode = nodeInit wenv boxNode
    children = newNode ^. L.children
    renderArea = newNode ^. L.info . L.renderArea
    cRenderArea = getChildRa wenv []

resizeExpand :: Spec
resizeExpand = describe "expand" $
  it "should have its children assigned a valid renderArea" $
    cRenderArea `shouldBe` vp

  where
    wenv = mockWenv ()
    vp  = Rect   0   0 640 480
    cRenderArea = getChildRa wenv [expandContent]

resizeAlign :: Spec
resizeAlign = describe "align" $ do
  it "should align its child left" $
    childRaL `shouldBe` cvpl

  it "should align its child right" $
    childRaR `shouldBe` cvpr

  it "should align its child top" $
    childRaT `shouldBe` cvpt

  it "should align its child bottom" $
    childRaB `shouldBe` cvpb

  it "should align its child top-left" $
    childRaTL `shouldBe` cvplt

  it "should align its child bottom-right" $
    childRaBR `shouldBe` cvpbr

  where
    wenv = mockWenv ()
    cvpl  = Rect   0 230 50 20
    cvpr  = Rect 590 230 50 20
    cvpt  = Rect 295   0 50 20
    cvpb  = Rect 295 460 50 20
    cvplt = Rect   0   0 50 20
    cvpbr = Rect 590 460 50 20
    childRaL = getChildRa wenv [alignLeft]
    childRaR = getChildRa wenv [alignRight]
    childRaT = getChildRa wenv [alignTop]
    childRaB = getChildRa wenv [alignBottom]
    childRaTL = getChildRa wenv [alignTop, alignLeft]
    childRaBR = getChildRa wenv [alignBottom, alignRight]

getChildRa :: Eq s => WidgetEnv s e -> [BoxCfg s e] -> Rect
getChildRa wenv cfgs = childLC ^. L.info . L.renderArea where
  lblNode = label "Label"
  boxNodeLC = nodeInit wenv (box_ cfgs lblNode)
  childLC = Seq.index (boxNodeLC ^. L.children) 0
