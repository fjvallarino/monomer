module Monomer.Widgets.StackSpec (spec) where

import Control.Lens ((&), (.~))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Grid
import Monomer.Widgets.Label
import Monomer.Widgets.Stack

import qualified Monomer.Lens as L

spec :: Spec
spec = describe "Stack" $ do
  updateSizeReq
  resize

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  updateSizeReqEmpty
  updateSizeReqItems

updateSizeReqEmpty :: Spec
updateSizeReqEmpty = describe "empty" $ do
  it "should return Fixed width = 0" $
    sizeReqW `shouldBe` FixedSize 0

  it "should return Fixed height = 0" $
    sizeReqH `shouldBe` FixedSize 0

  where
    wenv = mockWenv ()
    vstackInst = vstack []
    (sizeReqW, sizeReqH) = instUpdateSizeReq wenv vstackInst

updateSizeReqItems :: Spec
updateSizeReqItems = describe "several items" $ do
  it "should return width = Flex 80 1" $
    sizeReqW `shouldBe` FlexSize 80 1

  it "should return height = Fixed 60" $
    sizeReqH `shouldBe` FixedSize 60

  where
    wenv = mockWenv ()
    vstackInst = vstack [
        label "Hello",
        label "how",
        label "are you?"
      ]
    (sizeReqW, sizeReqH) = instUpdateSizeReq wenv vstackInst

resize :: Spec
resize = describe "resize" $ do
  resizeEmpty
  resizeFlexibleH
  resizeFlexibleV
  resizeStrictFlexH
  resizeStrictFlexV

resizeEmpty :: Spec
resizeEmpty = describe "empty" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should not have children" $
    children `shouldSatisfy` Seq.null

  where
    wenv = mockWenv ()
    vp = Rect 0 0 640 480
    vstackInst = vstack []
    newInst = instInit wenv vstackInst
    viewport = _wiViewport newInst
    children = _wiChildren newInst

resizeFlexibleH :: Spec
resizeFlexibleH = describe "flexible items, horizontal" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign size proportional to requested size to each children" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  it "should assign size proportional to requested size to each children" $
    childrenRa `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  where
    wenv = mockWenv () & L.appWindowSize .~ Size 480 640
    vp   = Rect   0 0 480 640
    cvp1 = Rect   0 0 112 640
    cvp2 = Rect 112 0 256 640
    cvp3 = Rect 368 0 112 640
    hstackInst = hstack [
        label "Label 1",
        label "Label Number Two",
        label "Label 3"
      ]
    newInst = instInit wenv hstackInst
    viewport = _wiViewport newInst
    childrenVp = _wiViewport <$> _wiChildren newInst
    childrenRa = _wiRenderArea <$> _wiChildren newInst

resizeFlexibleV :: Spec
resizeFlexibleV = describe "flexible items, vertical" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign size proportional to requested size to each children" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  it "should assign size proportional to requested size to each children" $
    childrenRa `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  where
    wenv = mockWenv ()
    vp   = Rect 0   0 640 480
    cvp1 = Rect 0   0 640 160
    cvp2 = Rect 0 160 640 160
    cvp3 = Rect 0 320 640 160
    vstackInst = vstack [
        vgrid [ label "Label 1" ],
        vgrid [ label "Label Number Two" ],
        vgrid [ label "Label 3" ]
      ]
    newInst = instInit wenv vstackInst
    viewport = _wiViewport newInst
    childrenVp = _wiViewport <$> _wiChildren newInst
    childrenRa = _wiRenderArea <$> _wiChildren newInst

resizeStrictFlexH :: Spec
resizeStrictFlexH = describe "strict/flexible items, horizontal" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign requested size to the main labels and the rest to grid" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  it "should assign requested size to the main labels and the rest to grid" $
    childrenRa `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  where
    wenv = mockWenv ()
    vp   = Rect   0 0 640 480
    cvp1 = Rect   0 0 100 480
    cvp2 = Rect 100 0 100 480
    cvp3 = Rect 200 0 440 480
    hstackInst = hstack [
        label "Label 1" `style` [width 100],
        label "Label 2" `style` [width 100],
        hgrid [
          label "Label 3"
        ]
      ]
    newInst = instInit wenv hstackInst
    viewport = _wiViewport newInst
    childrenVp = _wiViewport <$> _wiChildren newInst
    childrenRa = _wiRenderArea <$> _wiChildren newInst

resizeStrictFlexV :: Spec
resizeStrictFlexV = describe "strict/flexible items, vertical" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign requested size to the main labels and the rest to grid" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  it "should assign requested size to the main labels and the rest to grid" $
    childrenRa `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  where
    wenv = mockWenv ()
    vp   = Rect 0   0 640 480
    cvp1 = Rect 0   0 640 100
    cvp2 = Rect 0 100 640  20
    cvp3 = Rect 0 120 640 360
    vstackInst = vstack [
        label "Label 1" `style` [height 100],
        label "Label 2",
        vgrid [
          label "Label 3"
        ]
      ]
    newInst = instInit wenv vstackInst
    viewport = _wiViewport newInst
    childrenVp = _wiViewport <$> _wiChildren newInst
    childrenRa = _wiRenderArea <$> _wiChildren newInst
