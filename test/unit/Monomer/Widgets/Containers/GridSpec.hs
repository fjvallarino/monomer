{-|
Module      : Monomer.Widgets.Containers.GridSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for Grid widget.
-}
{-# LANGUAGE FlexibleContexts #-}

module Monomer.Widgets.Containers.GridSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Containers.Grid
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.Spacer

import qualified Monomer.Lens as L

spec :: Spec
spec = describe "Grid" $ do
  getSizeReq
  resize

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  getSizeReqEmpty
  getSizeReqItemsH
  getSizeReqItemsV
  getSizeReqMixedH
  getSizeReqMixedV
  getSizeReqUpdater
  getSizeReqChildSpacing

getSizeReqEmpty :: Spec
getSizeReqEmpty = describe "empty" $ do
  it "should return width = Fixed 0" $
    sizeReqW `shouldBe` fixedSize 0

  it "should return height = Fixed 0" $
    sizeReqH `shouldBe` fixedSize 0

  where
    wenv = mockWenv ()
    gridNode = vgrid []
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv gridNode

getSizeReqItemsH :: Spec
getSizeReqItemsH = describe "several items, horizontal" $ do
  it "should return width = Fixed 240 (largest width * 3)" $
    sizeReqW `shouldBe` fixedSize 240

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` fixedSize 20

  where
    wenv = mockWenv ()
    gridNode = hgrid [
        label "Hello",
        label "how",
        label "are you?"
      ]
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv gridNode

getSizeReqItemsV :: Spec
getSizeReqItemsV = describe "several items, vertical, one not visible" $ do
  it "should return width = Fixed 80" $
    sizeReqW `shouldBe` fixedSize 80

  it "should return height = Fixed 60" $
    sizeReqH `shouldBe` fixedSize 60

  where
    wenv = mockWenv ()
    gridNode = vgrid [
        label "Hello",
        label "how",
        label "" `nodeVisible` False,
        label "are you?"
      ]
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv gridNode

getSizeReqMixedH :: Spec
getSizeReqMixedH = describe "several items, different reqSizes" $ do
  it "should return width = Range 300 900 1 (3 * Range 100 300)" $
    sizeReqW `shouldBe` rangeSize 300 900 1

  it "should return height = Range 100 300 1" $
    sizeReqH `shouldBe` rangeSize 100 300 1

  where
    wenv = mockWenv ()
    gridNode = hgrid [
        label "Label 1" `styleBasic` [width 100, height 100],
        label "Label 2" `styleBasic` [maxWidth 300, maxHeight 300],
        label "Label 3"
      ]
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv gridNode

getSizeReqMixedV :: Spec
getSizeReqMixedV = describe "several items, different reqSizes" $ do
  it "should return width = Min 100 1" $
    sizeReqW `shouldBe` SizeReq 100 200 100 1

  it "should return height = Min 300 1 (3 * Min 100)" $
    sizeReqH `shouldBe` SizeReq 300 600 300 1

  where
    wenv = mockWenv ()
    gridNode = vgrid [
        label "Label 1" `styleBasic` [minWidth 100, minHeight 100],
        label "Label 2" `styleBasic` [maxWidth 300, maxHeight 300],
        label "Label 3"
      ]
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv gridNode

getSizeReqUpdater :: Spec
getSizeReqUpdater = describe "getSizeReqUpdater" $ do
  it "should return width = Min 50 2" $
    sizeReqW `shouldBe` minSize 50 2

  it "should return height = Max 20" $
    sizeReqH `shouldBe` maxSize 20 3

  where
    wenv = mockWenv ()
    vgridNode = vgrid_ [sizeReqUpdater (fixedToMinW 2), sizeReqUpdater (fixedToMaxH 3)] [label "Label"]
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv vgridNode

getSizeReqChildSpacing :: Spec
getSizeReqChildSpacing = describe "with childSpacing" $ do
  it "should not add spacing when empty" $
    testGridSizeReq [] `shouldBe` fixedSize 0

  it "should not add spacing when singleton" $
    testGridSizeReq [spacer3] `shouldBe` fixedSize 3

  it "should add spacing between children" $
    testGridSizeReq [spacer3, spacer3] `shouldBe` fixedSize 8
  
  it "should not add spacing between invisible children" $ do
    testGridSizeReq [spacer3, spacer3 `nodeVisible` False] `shouldBe` fixedSize 3
  
  where
    wenv = mockWenv ()
    spacer3 = spacer_ [width 3, resizeFactor 0]
    testGridSizeReq children = sizeReqH where
      (_sizeReqW, sizeReqH) = nodeGetSizeReq wenv vgridNode
      vgridNode = vgrid_ [childSpacing_ 2] children

resize :: Spec
resize = describe "resize" $ do
  resizeEmpty
  resizeItemsH
  resizeItemsV
  resizeChildSpacing

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

  where
    wenv = mockWenv () & L.windowSize .~ Size 480 640
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

resizeItemsV :: Spec
resizeItemsV = describe "several items, vertical, one not visible" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign the same viewport size to each children" $
    childrenVp `shouldBe` Seq.fromList [cvp1, cvp2, cvp3, cvp4]

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
        label "Label invisible" `nodeVisible` False,
        label "Label 3"
      ]
    newNode = nodeInit wenv gridNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = (^. L.info . L.viewport) <$> newNode ^. L.children

resizeChildSpacing :: Spec
resizeChildSpacing = describe "with childSpacing" $ do

  it "should add spacing between children" $
    testGridChildLocations [spacer, spacer] `shouldBe`
      Seq.fromList [(0, 215), (215 + 50, 480)]
  
  it "should not add spacing between invisible children" $
    testGridChildLocations [spacer, spacer `nodeVisible` False, spacer]
      `shouldBe` Seq.fromList [(0, 215), (0, 0), (215 + 50, 480)]
  
  where
    wenv = mockWenv ()
    testGridChildLocations children = childLocations where
      vgridNode = vgrid_ [childSpacing_ 50] children
      newNode = nodeInit wenv vgridNode
      childLocations = rectRoundYPostions . _wniViewport . _wnInfo <$> newNode ^. L.children

rectRoundYPostions :: Rect -> (Double, Double)
rectRoundYPostions (Rect _x y _w h) =
  (fromIntegral (round y), fromIntegral (round (y + h)))