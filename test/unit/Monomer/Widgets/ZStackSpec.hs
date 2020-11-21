module Monomer.Widgets.ZStackSpec (spec) where

import Control.Lens ((&), (.~))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Label
import Monomer.Widgets.Stack
import Monomer.Widgets.ZStack

import qualified Monomer.Lens as L

spec :: Spec
spec = describe "ZStack" $ do
  updateSizeReq
  resize

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  updateSizeReqEmpty
  updateSizeReqItems
  updateSizeReqItemsFixed

updateSizeReqEmpty :: Spec
updateSizeReqEmpty = describe "empty" $ do
  it "should return width = Fixed 0" $
    sizeReqW `shouldBe` FixedSize 0

  it "should return height = Fixed 0" $
    sizeReqH `shouldBe` FixedSize 0

  where
    wenv = mockWenv ()
    zstackInst = zstack []
    (sizeReqW, sizeReqH) = instUpdateSizeReq wenv zstackInst

updateSizeReqItems :: Spec
updateSizeReqItems = describe "several items, horizontal" $ do
  it "should return width = Flex 130 0.01" $
    sizeReqW `shouldBe` FlexSize 130 0.01

  it "should return height = Fixed 60" $
    sizeReqH `shouldBe` FixedSize 60

  where
    wenv = mockWenv ()
    zstackInst = zstack [
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
    (sizeReqW, sizeReqH) = instUpdateSizeReq wenv zstackInst

updateSizeReqItemsFixed :: Spec
updateSizeReqItemsFixed = describe "several items, horizontal" $ do
  it "should return width = Fixed 300" $
    sizeReqW `shouldBe` FixedSize 300

  it "should return height = Fixed 40" $
    sizeReqH `shouldBe` FixedSize 40

  where
    wenv = mockWenv ()
    zstackInst = zstack [
        vstack [
          label "Label a1",
          label "Label a2"
        ],
        vstack [
          label "Long b1",
          label "Long b2"
        ] `style` [width 300]
      ]
    (sizeReqW, sizeReqH) = instUpdateSizeReq wenv zstackInst

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
    zstackInst = zstack []
    newInst = instInit wenv zstackInst
    viewport = _wiViewport newInst
    children = _wiChildren newInst

resizeItems :: Spec
resizeItems = describe "several items, horizontal" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign the same viewport size to each children" $
    childrenVp `shouldBe` Seq.fromList [vp, vp, vp]

  it "should assign the same renderArea size to each children" $
    childrenRa `shouldBe` Seq.fromList [vp, vp, vp]

  where
    wenv = mockWenv ()
    vp   = Rect 0 0 640 480
    zstackInst = zstack [
        label "Label 1",
        label "Label Number Two",
        label "Label 3"
      ]
    newInst = instInit wenv zstackInst
    viewport = _wiViewport newInst
    childrenVp = _wiViewport <$> _wiChildren newInst
    childrenRa = _wiRenderArea <$> _wiChildren newInst
