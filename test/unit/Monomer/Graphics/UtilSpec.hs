{-|
Module      : Monomer.Graphics.UtilSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for graphics related utilities.
-}
module Monomer.Graphics.UtilSpec (spec) where

import Test.Hspec

import Monomer.Graphics

spec :: Spec
spec = describe "Util" $ do
  colorHex
  colorHSL

colorHex :: Spec
colorHex = describe "colorHex" $ do
  it "should generate the correct color" $ do
    rgbHex "#FFFFFF" `shouldBe` Color 255 255 255 1
    rgbHex "#FF0000" `shouldBe` Color 255 0 0 1
    rgbHex "#00FF00" `shouldBe` Color 0 255 0 1
    rgbHex "#0000FF" `shouldBe` Color 0 0 255 1
    rgbHex "#B06430" `shouldBe` Color 176 100 48 1

colorHSL :: Spec
colorHSL = describe "colorHSL" $ do
  it "should generate the correct color" $ do
    hsl   0 100 100 `shouldBe` rgbHex "#FFFFFF"
    hsl   0 100  50 `shouldBe` rgbHex "#FF0000"
    hsl 120 100  50 `shouldBe` rgbHex "#00FF00"
    hsl 240 100  50 `shouldBe` rgbHex "#0000FF"
