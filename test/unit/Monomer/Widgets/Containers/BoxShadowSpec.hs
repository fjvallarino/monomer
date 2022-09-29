{-|
Module      : Monomer.Widgets.Containers.BoxShadowSpec
Copyright   : (c) 2022 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for BoxShadow widget.
-}

module Monomer.Widgets.Containers.BoxShadowSpec (spec) where

import Control.Lens ((^.))
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.TestUtil
import Monomer.Widgets.Containers.BoxShadow
import Monomer.Widgets.Singles.Label

import qualified Monomer.Lens as L

spec :: Spec
spec = describe "BoxShadow" $ do
  getSizeReq
  resize

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return child width plus radius on each side" $ do
    sizeW `shouldBe` fixedSize (6 + 7 * 2)

  it "should return child height plus radius on each side" $ do
    sizeH `shouldBe` fixedSize (9 + 7 * 2)

  where
    wenv = mockWenvEvtUnit ()
    boxShadowNode = boxShadow_ [radius 7] $
      label "test" `styleBasic` [sizeReqW (fixedSize 6), sizeReqH (fixedSize 9)]
    (sizeW, sizeH) = nodeGetSizeReq wenv boxShadowNode

resize :: Spec
resize = describe "resize" $ do
  it "shadow in middle-center should put child in middle-center" $ do
    childVp [radius 8, alignMiddle, alignCenter] `shouldBe` Rect 8 8 624 464

  it "shadow in top-left should put child in bottom-right" $ do
    childVp [radius 8, alignTop, alignLeft] `shouldBe` Rect 10 10 624 464

  it "shadow in bottom-right should put child in top-left" $ do
    childVp [radius 8, alignBottom, alignRight] `shouldBe` Rect 6 6 624 464

  it "shadow in default location should put child in top-center" $ do
    childVp [radius 8] `shouldBe` Rect 8 6 624 464

  where
    wenv = mockWenvEvtUnit ()
    childVp config = childWidget ^. L.info . L.viewport where
      node = boxShadow_ config (label "test")
      initedNode = nodeInit wenv node
      childWidget = Seq.index (initedNode ^. L.children) 0
