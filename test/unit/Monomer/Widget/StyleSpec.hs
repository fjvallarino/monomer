module Monomer.Widget.StyleSpec where

import Hedgehog
import Hedgehog.Classes
import Test.Hspec
import TestUtils

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Monomer.Common.Style
import Monomer.Graphics.Types

spec :: Spec
spec = describe "Style" $ do
  it "should do nothing, again" $
    1 `shouldBe` 1

  it "should have require function that checks hedgehog properties" $
    require $ property $ do
      x <- forAll (Gen.int Range.constantBounded)
      x === x

  it "should check BorderSide fulfills Semigroup laws" $
    1 `shouldBe` 1

  it "should check Padding fulfills Semigroup laws" $
    checkLaws genPadding [monoidLaws]

genInt :: Gen Int
genInt = Gen.int (Range.linear (-10000) 10000)

genDouble :: Gen Double
genDouble = Gen.double (Range.linearFrac (-10000) 10000)

genMDouble :: Gen (Maybe Double)
genMDouble = Gen.maybe genDouble

genRGB :: Gen Color
genRGB = Color <$> genInt <*> genInt <*> genInt <*> pure 1

genBorderSide :: Gen BorderSide
genBorderSide = BorderSide <$> genDouble <*> genRGB

genPadding :: Gen Padding
genPadding = Padding <$> genMDouble <*> genMDouble <*> genMDouble <*> genMDouble
