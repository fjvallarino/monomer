{-|
Module      : Monomer.Core.SizeReqSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for SizeReq functions.
-}
module Monomer.Core.SizeReqSpec (spec) where

import Test.Hspec

import Monomer.Core.Combinators
import Monomer.Core.SizeReq

spec :: Spec
spec = describe "SizeReq" $ do
  sizeReqUpdaterSpec

sizeReqUpdaterSpec :: Spec
sizeReqUpdaterSpec = describe "SizeReqUpdater" $ do
  it "should update fixed size to minimum size" $ do
    fixedToMinW 1 (width 100, height 100) `shouldBe` (minWidth 100, height 100)
    fixedToMinH 1 (width 100, height 100) `shouldBe` (width 100, minHeight 100)

  it "should update fixed size to expand size" $ do
    fixedToExpandW 1 (width 100, height 100) `shouldBe` (expandWidth 100, height 100)
    fixedToExpandH 1 (width 100, height 100) `shouldBe` (width 100, expandHeight 100)
