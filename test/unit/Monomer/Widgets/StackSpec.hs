module Monomer.Widgets.StackSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Label
import Monomer.Widgets.Spacer
import Monomer.Widgets.Stack

import qualified Monomer.Lens as L

-- Event handling (ignoreEmptyClick) is tested in zstack
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
    vstackNode = vstack []
    (sizeReqW, sizeReqH) = nodeUpdateSizeReq wenv vstackNode

updateSizeReqItems :: Spec
updateSizeReqItems = describe "several items" $ do
  it "should return width = Flex 80 0.01" $
    sizeReqW `shouldBe` FlexSize 80 0.01

  it "should return height = Fixed 60" $
    sizeReqH `shouldBe` FixedSize 60

  where
    wenv = mockWenv ()
    vstackNode = vstack [
        label "Hello",
        label "how",
        label "are you?"
      ]
    (sizeReqW, sizeReqH) = nodeUpdateSizeReq wenv vstackNode

resize :: Spec
resize = describe "resize" $ do
  resizeEmpty
  resizeFlexibleH
  resizeFlexibleV
  resizeStrictFlexH
  resizeStrictFlexV
  resizeMixedH
  resizeMixedV
  resizeAllV
  resizeNoSpaceV
  resizeSpacerFlexH
  resizeSpacerFixedH

resizeEmpty :: Spec
resizeEmpty = describe "empty" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should not have children" $
    children `shouldSatisfy` Seq.null

  where
    wenv = mockWenv ()
    vp = Rect 0 0 640 480
    vstackNode = vstack []
    newNode = nodeInit wenv vstackNode
    viewport = newNode ^. L.info . L.viewport
    children = newNode ^. L.children

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
    hstackNode = hstack [
        label "Label 1",
        label "Label Number Two",
        label "Label 3"
      ]
    newNode = nodeInit wenv hstackNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> newNode ^. L.children
    childrenRa = roundRectUnits . _wniRenderArea . _wnInfo <$> newNode ^. L.children

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
    vstackNode = vstack [
        label "Label 1" `style` [flexHeight 20],
        label "Label Number Two" `style` [flexHeight 20],
        label "Label 3" `style` [flexHeight 20]
      ]
    newNode = nodeInit wenv vstackNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = (^. L.info . L.viewport) <$> newNode ^. L.children
    childrenRa = (^. L.info . L.renderArea) <$> newNode ^. L.children

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
    hstackNode = hstack [
        label "Label 1" `style` [width 100],
        label "Label 2" `style` [width 100],
        label "Label 3"
      ]
    newNode = nodeInit wenv hstackNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = (^. L.info . L.viewport) <$> newNode ^. L.children
    childrenRa = (^. L.info . L.renderArea) <$> newNode ^. L.children

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
    vstackNode = vstack [
        label "Label 1" `style` [height 100],
        label "Label 2",
        label "Label 3" `style` [flexHeight 100]
      ]
    newNode = nodeInit wenv vstackNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = (^. L.info . L.viewport) <$> newNode ^. L.children
    childrenRa = (^. L.info . L.renderArea) <$> newNode ^. L.children

resizeMixedH :: Spec
resizeMixedH = describe "mixed items, horizontal" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign size proportional to requested size to each children" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2]

  it "should assign size proportional to requested size to each children" $
    childrenRa `shouldBe` Seq.fromList [cvp1, cvp2]

  where
    wenv = mockWenv ()
    vp   = Rect   0 0 640 480
    cvp1 = Rect   0 0 196  20
    cvp2 = Rect 196 0 444  20
    hstackNode = vstack [
        hstack [
          label "Short label",
          label "This label is much longer"
        ]
      ]
    newNode = nodeInit wenv hstackNode
    viewport = newNode ^. L.info . L.viewport
    firstChild = Seq.index (newNode ^. L.children) 0
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> firstChild ^. L.children
    childrenRa = roundRectUnits . _wniRenderArea . _wnInfo <$> firstChild ^. L.children

resizeMixedV :: Spec
resizeMixedV = describe "mixed items, vertical" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign size proportional to requested size to each children" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  it "should assign size proportional to requested size to each children" $
    childrenRa `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  where
    wenv = mockWenv ()
    vp   = Rect 0   0 640 480
    cvp1 = Rect 0   0 640  20
    cvp2 = Rect 0  20 640 426
    cvp3 = Rect 0 446 640  34
    vstackNode = hstack [
        vstack [
          label "Label 1",
          label "Label 2" `style` [minHeight 250],
          label "Label 3" `style` [flexHeight 20]
        ]
      ]
    newNode = nodeInit wenv vstackNode
    viewport = newNode ^. L.info . L.viewport
    firstChild = Seq.index (newNode ^. L.children) 0
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> firstChild ^. L.children
    childrenRa = roundRectUnits . _wniRenderArea . _wnInfo <$> firstChild ^. L.children

resizeAllV :: Spec
resizeAllV = describe "all kinds of sizeReq, vertical" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign size proportional to requested size to each children" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3, cvp4, cvp5]

  it "should assign size proportional to requested size to each children" $
    childrenRa `shouldBe` Seq.fromList [cvp1, cvp2, cvp3, cvp4, cvp5]

  where
    wenv = mockWenv ()
    vp   = Rect 0   0 640 480
    cvp1 = Rect 0   0 640  50
    cvp2 = Rect 0  50 640 115
    cvp3 = Rect 0 165 640 135
    cvp4 = Rect 0 300 640  80
    cvp5 = Rect 0 380 640 100
    vstackNode = vstack [
        label "Label 1" `style` [width 50, height 50],
        label "Label 2" `style` [flexWidth 60, flexHeight 60],
        label "Label 3" `style` [minWidth 70, minHeight 70],
        label "Label 4" `style` [maxWidth 80, maxHeight 80],
        label "Label 5" `style` [rangeWidth 90 100, rangeHeight 90 100]
      ]
    newNode = nodeInit wenv vstackNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> newNode ^. L.children
    childrenRa = roundRectUnits . _wniRenderArea . _wnInfo <$> newNode ^. L.children

resizeNoSpaceV :: Spec
resizeNoSpaceV = describe "vertical, without enough space" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign size proportional to requested size to each children" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3, cvp4, cvp5]

  it "should assign size proportional to requested size to each children" $
    childrenRa `shouldBe` Seq.fromList [cvp1, cvp2, cvp3, cvp4, cvp5]

  where
    wenv = mockWenv ()
    vp   = Rect 0   0 640 480
    cvp1 = Rect 0   0 640 200
    cvp2 = Rect 0 200 640 200
    cvp3 = Rect 0 400 640   0
    cvp4 = Rect 0 400 640 200
    cvp5 = Rect 0 600 640 200
    vstackNode = vstack [
        label "Label 1" `style` [height 200],
        label "Label 2" `style` [height 200],
        label "Label 3" `style` [flexHeight 200],
        label "Label 4" `style` [height 200],
        label "Label 5" `style` [height 200]
      ]
    newNode = nodeInit wenv vstackNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> newNode ^. L.children
    childrenRa = roundRectUnits . _wniRenderArea . _wnInfo <$> newNode ^. L.children

resizeSpacerFlexH :: Spec
resizeSpacerFlexH = describe "label flex and spacer, horizontal" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign size proportional to requested size to each children" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  it "should assign size proportional to requested size to each children" $
    childrenRa `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  where
    wenv = mockWenv ()
    vp   = Rect   0 0 640 480
    cvp1 = Rect   0 0 211 480
    cvp2 = Rect 211 0   8 480
    cvp3 = Rect 219 0 421 480
    hstackNode = hstack [
        label "Label" `style` [flexWidth 100],
        spacer,
        label "Label" `style` [flexWidth 200]
      ]
    newNode = nodeInit wenv hstackNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> newNode ^. L.children
    childrenRa = roundRectUnits . _wniRenderArea . _wnInfo <$> newNode ^. L.children

resizeSpacerFixedH :: Spec
resizeSpacerFixedH = describe "label fixed and spacer, horizontal" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign size proportional to requested size to each children" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  it "should assign size proportional to requested size to each children" $
    childrenRa `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  where
    wenv = mockWenv ()
    vp   = Rect   0 0 640 480
    cvp1 = Rect   0 0 100 480
    cvp2 = Rect 100 0 340 480
    cvp3 = Rect 440 0 200 480
    hstackNode = hstack [
        label "Label" `style` [width 100],
        spacer,
        label "Label" `style` [width 200]
      ]
    newNode = nodeInit wenv hstackNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> newNode ^. L.children
    childrenRa = roundRectUnits . _wniRenderArea . _wnInfo <$> newNode ^. L.children
