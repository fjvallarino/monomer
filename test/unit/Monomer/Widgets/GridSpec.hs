module Monomer.Widgets.GridSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Grid
import Monomer.Widgets.Label

spec :: Spec
spec = describe "Grid" $ do
  updateSizeReq
  resize

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  updateSizeReqEmpty
  updateSizeReqItemsH
  updateSizeReqItemsV

updateSizeReqEmpty :: Spec
updateSizeReqEmpty = describe "empty" $ do
  it "should return Fixed width = 0" $
    sizeReqW `shouldBe` FixedSize 0

  it "should return Fixed height = 0" $
    sizeReqH `shouldBe` FixedSize 0

  where
    wenv = mockWenv ()
    gridInst = vgrid []
    (sizeReqW, sizeReqH) = instUpdateSizeReq wenv gridInst

updateSizeReqItemsH :: Spec
updateSizeReqItemsH = describe "several items, horizontal" $ do
  it "should return Flex width = 240 (largest width * 3)" $
    sizeReqW `shouldBe` FlexSize 240 1

  it "should return Flex height = 20" $
    sizeReqH `shouldBe` FixedSize 20

  where
    wenv = mockWenv ()
    gridInst = hgrid [
        label "Hello",
        label "how",
        label "are you?"
      ]
    (sizeReqW, sizeReqH) = instUpdateSizeReq wenv gridInst

updateSizeReqItemsV :: Spec
updateSizeReqItemsV = describe "several items, vertical, one not visible" $ do
  it "should return Flex width = 80" $
    sizeReqW `shouldBe` FlexSize 80 1

  it "should return Flex height = 60" $
    sizeReqH `shouldBe` FlexSize 60 1

  where
    wenv = mockWenv ()
    gridInst = vgrid [
        label "Hello",
        label "how",
        label "" `visible` False,
        label "are you?"
      ]
    (sizeReqW, sizeReqH) = instUpdateSizeReq wenv gridInst

resize :: Spec
resize = describe "resize" $ do
  resizeEmpty
  resizeItemsH
  resizeItemsV

resizeEmpty :: Spec
resizeEmpty = describe "empty" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should not have children" $
    children `shouldSatisfy` Seq.null

  where
    wenv = mockWenv ()
    vp = Rect 0 0 640 480
    gridInst = vgrid []
    newInst = instResize wenv vp gridInst
    viewport = _wiViewport newInst
    children = _wiChildren newInst

resizeItemsH :: Spec
resizeItemsH = describe "several items, horizontal" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign the same viewport size to each children" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  it "should assign the same renderArea size to each children" $
    childrenRa `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  where
    wenv = mockWenv ()
    vp   = Rect   0 0 480 640
    cvp1 = Rect   0 0 160 640
    cvp2 = Rect 160 0 160 640
    cvp3 = Rect 320 0 160 640
    gridInst = hgrid [
        label "Label 1",
        label "Label Number Two",
        label "Label 3"
      ]
    newInst = instResize wenv vp gridInst
    viewport = _wiViewport newInst
    childrenVp = _wiViewport <$> _wiChildren newInst
    childrenRa = _wiRenderArea <$> _wiChildren newInst

resizeItemsV :: Spec
resizeItemsV = describe "several items, vertical, one not visible" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign the same viewport size to each children" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3, cvp4]

  it "should assign the same renderArea size to each children" $
    childrenRa `shouldBe` Seq.fromList [cvp1, cvp2, cvp3, cvp4]

  where
    wenv = mockWenv ()
    vp   = Rect 0   0 640 480
    cvp1 = Rect 0   0 640 160
    cvp2 = Rect 0 160 640 160
    cvp3 = Rect 0   0   0   0
    cvp4 = Rect 0 320 640 160
    gridInst = vgrid [
        label "Label 1",
        label "Label Number Two",
        label "Label invisible" `visible` False,
        label "Label 3"
      ]
    newInst = instResize wenv vp gridInst
    viewport = _wiViewport newInst
    childrenVp = _wiViewport <$> _wiChildren newInst
    childrenRa = _wiRenderArea <$> _wiChildren newInst
