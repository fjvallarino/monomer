module Monomer.Widgets.GridSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Grid
import Monomer.Widgets.Label

import qualified Monomer.Lens as L

spec :: Spec
spec = describe "Grid" $ do
  updateSizeReq
  resize

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  updateSizeReqEmpty
  updateSizeReqItemsH
  updateSizeReqItemsV
  updateSizeReqMixedH
  updateSizeReqMixedV

updateSizeReqEmpty :: Spec
updateSizeReqEmpty = describe "empty" $ do
  it "should return width = Fixed 0" $
    sizeReqW `shouldBe` FixedSize 0

  it "should return height = Fixed 0" $
    sizeReqH `shouldBe` FixedSize 0

  where
    wenv = mockWenv ()
    gridNode = vgrid []
    (sizeReqW, sizeReqH) = nodeUpdateSizeReq wenv gridNode

updateSizeReqItemsH :: Spec
updateSizeReqItemsH = describe "several items, horizontal" $ do
  it "should return width = Flex 240 0.01 (largest width * 3)" $
    sizeReqW `shouldBe` FlexSize 240 0.01

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` FixedSize 20

  where
    wenv = mockWenv ()
    gridNode = hgrid [
        label "Hello",
        label "how",
        label "are you?"
      ]
    (sizeReqW, sizeReqH) = nodeUpdateSizeReq wenv gridNode

updateSizeReqItemsV :: Spec
updateSizeReqItemsV = describe "several items, vertical, one not visible" $ do
  it "should return width = Flex 80 0.01" $
    sizeReqW `shouldBe` FlexSize 80 0.01

  it "should return height = Fixed 60" $
    sizeReqH `shouldBe` FixedSize 60

  where
    wenv = mockWenv ()
    gridNode = vgrid [
        label "Hello",
        label "how",
        label "" `visible` False,
        label "are you?"
      ]
    (sizeReqW, sizeReqH) = nodeUpdateSizeReq wenv gridNode

updateSizeReqMixedH :: Spec
updateSizeReqMixedH = describe "several items, different reqSizes" $ do
  it "should return width = Range 300 900 1 (3 * Range 100 300)" $
    sizeReqW `shouldBe` RangeSize 300 900 1

  it "should return height = Range 100 300 1" $
    sizeReqH `shouldBe` RangeSize 100 300 1

  where
    wenv = mockWenv ()
    gridNode = hgrid [
        label "Label 1" `style` [width 100, height 100],
        label "Label 2" `style` [maxWidth 300, maxHeight 300],
        label "Label 3"
      ]
    (sizeReqW, sizeReqH) = nodeUpdateSizeReq wenv gridNode

updateSizeReqMixedV :: Spec
updateSizeReqMixedV = describe "several items, different reqSizes" $ do
  it "should return width = Min 100 1" $
    sizeReqW `shouldBe` MinSize 100 1

  it "should return height = Min 300 1 (3 * Min 100)" $
    sizeReqH `shouldBe` MinSize 300 1

  where
    wenv = mockWenv ()
    gridNode = vgrid [
        label "Label 1" `style` [minWidth 100, minHeight 100],
        label "Label 2" `style` [maxWidth 300, maxHeight 300],
        label "Label 3"
      ]
    (sizeReqW, sizeReqH) = nodeUpdateSizeReq wenv gridNode

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
    gridNode = vgrid []
    newNode = nodeInit wenv gridNode
    viewport = newNode ^. L.info . L.viewport
    children = newNode ^. L.children

resizeItemsH :: Spec
resizeItemsH = describe "several items, horizontal" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign the same viewport size to each children" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  it "should assign the same renderArea size to each children" $
    childrenRa `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  where
    wenv = mockWenv () & L.appWindowSize .~ Size 480 640
    vp   = Rect   0 0 480 640
    cvp1 = Rect   0 0 160 640
    cvp2 = Rect 160 0 160 640
    cvp3 = Rect 320 0 160 640
    gridNode = hgrid [
        label "Label 1",
        label "Label Number Two",
        label "Label 3"
      ]
    newNode = nodeInit wenv gridNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = (^. L.info . L.viewport) <$> newNode ^. L.children
    childrenRa = (^. L.info . L.renderArea) <$> newNode ^. L.children

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
    gridNode = vgrid [
        label "Label 1",
        label "Label Number Two",
        label "Label invisible" `visible` False,
        label "Label 3"
      ]
    newNode = nodeInit wenv gridNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = (^. L.info . L.viewport) <$> newNode ^. L.children
    childrenRa = (^. L.info . L.renderArea) <$> newNode ^. L.children
