{-|
Module      : Monomer.Widgets.Containers.StackSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for Stack widget.
-}
{-# LANGUAGE FlexibleContexts #-}

module Monomer.Widgets.Containers.StackSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.Spacer

import qualified Monomer.Lens as L

-- Event handling (ignoreEmptyClick) is tested in zstack
spec :: Spec
spec = describe "Stack" $ do
  getSizeReq
  resize

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  getSizeReqEmpty
  getSizeReqItems
  getSizeReqUpdater

getSizeReqEmpty :: Spec
getSizeReqEmpty = describe "empty" $ do
  it "should return Fixed width = 0" $
    sizeReqW `shouldBe` fixedSize 0

  it "should return Fixed height = 0" $
    sizeReqH `shouldBe` fixedSize 0

  where
    wenv = mockWenv ()
    vstackNode = vstack []
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv vstackNode

getSizeReqItems :: Spec
getSizeReqItems = describe "several items" $ do
  it "should return width = Fixed 80" $
    sizeReqW `shouldBe` fixedSize 80

  it "should return height = Fixed 60" $
    sizeReqH `shouldBe` fixedSize 60

  where
    wenv = mockWenv ()
    vstackNode = vstack [
        label "Hello",
        label "how",
        label "are you?"
      ]
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv vstackNode

getSizeReqUpdater :: Spec
getSizeReqUpdater = describe "getSizeReqUpdater" $ do
  it "should return width = Min 50 2" $
    sizeReqW1 `shouldBe` minSize 50 2

  it "should return height = Max 20" $
    sizeReqH1 `shouldBe` maxSize 20 3

  it "should return width = Min 50 10" $
    sizeReqW2 `shouldBe` minSize 50 10

  it "should return height = Max 20 15" $
    sizeReqH2 `shouldBe` maxSize 20 15

  where
    wenv = mockWenv ()
    updater (rw, rh) = (minSize (rw ^. L.fixed) 2, maxSize (rh ^. L.fixed) 3)
    vstackNode1 = vstack_ [sizeReqUpdater updater] [label "Label"]
    vstackNode2 = vstack_ [sizeReqUpdater (fixedToMinW 10), sizeReqUpdater (fixedToMaxH 15)] [label "Label"]
    (sizeReqW1, sizeReqH1) = nodeGetSizeReq wenv vstackNode1
    (sizeReqW2, sizeReqH2) = nodeGetSizeReq wenv vstackNode2

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
    -- Main axis is adjusted to content
    vp = Rect 0 0 640 0
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

  where
    wenv = mockWenv () & L.windowSize .~ Size 480 640
    vp   = Rect   0 0 480 640
    cvp1 = Rect   0 0 112 640
    cvp2 = Rect 112 0 256 640
    cvp3 = Rect 368 0 112 640
    hstackNode = hstack [
        label "Label 1" `styleBasic` [expandWidth 70],
        label "Label 2" `styleBasic` [expandWidth 160],
        label "Label 3" `styleBasic` [expandWidth 70]
      ]
    newNode = nodeInit wenv hstackNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> newNode ^. L.children

resizeFlexibleV :: Spec
resizeFlexibleV = describe "flexible items, vertical" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign size proportional to requested size to each children" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  where
    wenv = mockWenv ()
    vp   = Rect 0   0 640 480
    cvp1 = Rect 0   0 640 160
    cvp2 = Rect 0 160 640 160
    cvp3 = Rect 0 320 640 160
    vstackNode = vstack [
        label "Label 1" `styleBasic` [flexHeight 20],
        label "Label Number Two" `styleBasic` [flexHeight 20],
        label "Label 3" `styleBasic` [flexHeight 20]
      ]
    newNode = nodeInit wenv vstackNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = (^. L.info . L.viewport) <$> newNode ^. L.children

resizeStrictFlexH :: Spec
resizeStrictFlexH = describe "strict/flexible items, horizontal" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign requested size to the main labels and the rest to grid" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  where
    wenv = mockWenv ()
    vp   = Rect   0 0 640 480
    cvp1 = Rect   0 0 100 480
    cvp2 = Rect 100 0 100 480
    cvp3 = Rect 200 0 440 480
    hstackNode = hstack [
        label "Label 1" `styleBasic` [width 100],
        label "Label 2" `styleBasic` [width 100],
        label "Label 3" `styleBasic` [expandWidth 70]
      ]
    newNode = nodeInit wenv hstackNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = (^. L.info . L.viewport) <$> newNode ^. L.children

resizeStrictFlexV :: Spec
resizeStrictFlexV = describe "strict/flexible items, vertical" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign requested size to the main labels and the rest to grid" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  where
    wenv = mockWenv ()
    vp   = Rect 0   0 640 480
    cvp1 = Rect 0   0 640 100
    cvp2 = Rect 0 100 640  20
    cvp3 = Rect 0 120 640 360
    vstackNode = vstack [
        label "Label 1" `styleBasic` [height 100],
        label "Label 2",
        label "Label 3" `styleBasic` [flexHeight 100]
      ]
    newNode = nodeInit wenv vstackNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = (^. L.info . L.viewport) <$> newNode ^. L.children

resizeMixedH :: Spec
resizeMixedH = describe "mixed items, horizontal" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign size proportional to requested size to each children" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2]

  where
    wenv = mockWenv ()
    vp   = Rect   0 0 640  20
    cvp1 = Rect   0 0 196  20
    cvp2 = Rect 196 0 444  20
    hstackNode = vstack [
        hstack [
          label "Label 1" `styleBasic` [expandWidth 110],
          label "Label 2" `styleBasic` [expandWidth 250]
        ]
      ]
    newNode = nodeInit wenv hstackNode
    viewport = newNode ^. L.info . L.viewport
    firstChild = Seq.index (newNode ^. L.children) 0
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> firstChild ^. L.children

resizeMixedV :: Spec
resizeMixedV = describe "mixed items, vertical" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign size proportional to requested size to each children" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  where
    wenv = mockWenv ()
    vp   = Rect 0   0 70 480
    cvp1 = Rect 0   0 70  20
    cvp2 = Rect 0  20 70 426
    cvp3 = Rect 0 446 70  34
    vstackNode = hstack [
        vstack [
          label "Label 1",
          label "Label 2" `styleBasic` [minHeight 250],
          label "Label 3" `styleBasic` [flexHeight 20]
        ]
      ]
    newNode = nodeInit wenv vstackNode
    viewport = newNode ^. L.info . L.viewport
    firstChild = Seq.index (newNode ^. L.children) 0
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> firstChild ^. L.children

resizeAllV :: Spec
resizeAllV = describe "all kinds of sizeReq, vertical" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign size proportional to requested size to each children" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3, cvp4, cvp5]

  where
    wenv = mockWenv ()
    vp   = Rect 0   0 640 480
    cvp1 = Rect 0   0 640  50
    cvp2 = Rect 0  50 640 115
    cvp3 = Rect 0 165 640 135
    cvp4 = Rect 0 300 640  80
    cvp5 = Rect 0 380 640 100
    vstackNode = vstack [
        label "Label 1" `styleBasic` [width 50, height 50],
        label "Label 2" `styleBasic` [flexWidth 60, flexHeight 60],
        label "Label 3" `styleBasic` [minWidth 70, minHeight 70],
        label "Label 4" `styleBasic` [maxWidth 80, maxHeight 80],
        label "Label 5" `styleBasic` [rangeWidth 90 100, rangeHeight 90 100]
      ]
    newNode = nodeInit wenv vstackNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> newNode ^. L.children

resizeNoSpaceV :: Spec
resizeNoSpaceV = describe "vertical, without enough space" $ do
  it "should have a larger viewport size (parent should fix it)" $ do
    viewport `shouldBe` vp
    viewport `shouldBe` vp

  it "should assign size proportional to requested size to each children" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3, cvp4, cvp5]

  where
    wenv = mockWenv ()
    vp   = Rect 0   0 640 800
    cvp1 = Rect 0   0 640 200
    cvp2 = Rect 0 200 640 200
    cvp3 = Rect 0 400 640   0
    cvp4 = Rect 0 400 640 200
    cvp5 = Rect 0 600 640 200
    vstackNode = vstack [
        label "Label 1" `styleBasic` [height 200],
        label "Label 2" `styleBasic` [height 200],
        label "Label 3" `styleBasic` [flexHeight 200],
        label "Label 4" `styleBasic` [height 200],
        label "Label 5" `styleBasic` [height 200]
      ]
    newNode = nodeInit wenv vstackNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> newNode ^. L.children

resizeSpacerFlexH :: Spec
resizeSpacerFlexH = describe "label flex and spacer, horizontal" $ do
  it "should have the provided viewport size" $
    roundRectUnits viewport `shouldBe` vp

  it "should assign size proportional to requested size to each children" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  where
    wenv = mockWenv ()
    vp   = Rect   0 0 640 480
    cvp1 = Rect   0 0 211 480
    cvp2 = Rect 211 0   8 480
    cvp3 = Rect 219 0 421 480
    hstackNode = hstack [
        label "Label" `styleBasic` [flexWidth 100],
        filler,
        label "Label" `styleBasic` [flexWidth 200]
      ]
    newNode = nodeInit wenv hstackNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> newNode ^. L.children

resizeSpacerFixedH :: Spec
resizeSpacerFixedH = describe "label fixed and spacer, horizontal" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign size proportional to requested size to each children" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3]

  where
    wenv = mockWenv ()
    vp   = Rect   0 0 640 480
    cvp1 = Rect   0 0 100 480
    cvp2 = Rect 100 0 340 480
    cvp3 = Rect 440 0 200 480
    hstackNode = hstack [
        label "Label" `styleBasic` [width 100],
        filler,
        label "Label" `styleBasic` [width 200]
      ]
    newNode = nodeInit wenv hstackNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> newNode ^. L.children
