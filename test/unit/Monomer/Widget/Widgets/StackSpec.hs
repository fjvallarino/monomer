{-# LANGUAGE RecordWildCards #-}
{- HLINT ignore "Reduce duplication" -}

module Monomer.Widget.Widgets.StackSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.StyleCombinators
import Monomer.Event.Types
import Monomer.Widget.Types
import Monomer.Widget.TestUtil
import Monomer.Widget.Util
import Monomer.Widget.Widgets.Grid
import Monomer.Widget.Widgets.Label
import Monomer.Widget.Widgets.Stack

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
  it "should return size (0, 0)" $
    _srSize `shouldBe` Size 0 0

  it "should return Flexible width policy" $
    _srPolicyW `shouldBe` FlexibleSize

  it "should return Strict height policy" $
    _srPolicyH `shouldBe` FlexibleSize

  where
    wenv = mockWenv ()
    vstackInst = vstack []
    SizeReq{..} = instanceUpdateSizeReq wenv vstackInst

updateSizeReqItems :: Spec
updateSizeReqItems = describe "several items" $ do
  it "should return size (80, 60)" $
    _srSize `shouldBe` Size 80 60

  it "should return Flexible width policy" $
    _srPolicyW `shouldBe` FlexibleSize

  it "should return Strict height policy" $
    _srPolicyH `shouldBe` FlexibleSize

  where
    wenv = mockWenv ()
    vstackInst = vstack [
        label "Hello",
        label "how",
        label "are you?"
      ]
    SizeReq{..} = instanceUpdateSizeReq wenv vstackInst

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
    newInst = instanceResize wenv vp vstackInst
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
    wenv = mockWenv ()
    vp = Rect 0 0 480 640
    cvp1 = Rect   0 0 112 640
    cvp2 = Rect 112 0 256 640
    cvp3 = Rect 368 0 112 640
    hstackInst = hstack [
        label "Label 1",
        label "Label Number Two",
        label "Label 3"
      ]
    newInst = instanceResize wenv vp hstackInst
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
    vp = Rect 0 0 640 480
    cvp1 = Rect 0   0 640 160
    cvp2 = Rect 0 160 640 160
    cvp3 = Rect 0 320 640 160
    vstackInst = vstack [
        vgrid [ label "Label 1" ],
        vgrid [ label "Label Number Two" ],
        vgrid [ label "Label 3" ]
      ]
    newInst = instanceResize wenv vp vstackInst
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
    vp = Rect 0 0 640 480
    cvp1 = Rect   0 0 100 480
    cvp2 = Rect 100 0 100 480
    cvp3 = Rect 200 0 440 480
    hstackInst = hstack [
        label "Label 1" `style` width 100,
        label "Label 2" `style` width 100,
        hgrid [
          label "Label 3"
        ]
      ]
    newInst = instanceResize wenv vp hstackInst
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
    vp = Rect 0 0 640 480
    cvp1 = Rect 0  0 640  20
    cvp2 = Rect 0 20 640  20
    cvp3 = Rect 0 40 640 440
    vstackInst = vstack [
        label "Label 1",
        label "Label 2",
        vgrid [
          label "Label 3"
        ]
      ]
    newInst = instanceResize wenv vp vstackInst
    viewport = _wiViewport newInst
    childrenVp = _wiViewport <$> _wiChildren newInst
    childrenRa = _wiRenderArea <$> _wiChildren newInst
