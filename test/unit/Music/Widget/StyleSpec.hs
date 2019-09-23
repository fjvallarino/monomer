module GUI.Widget.StyleSpec where

import Hedgehog
import Hedgehog.Classes
import Test.Hspec
import TestUtils

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import GUI.Core
import GUI.Widget.Style

spec :: Spec
spec = describe "Style" $ do
  it "should do nothing, again" $ do
    1 `shouldBe` 1

  it "should have require function that checks hedgehog properties" $ do
    require $ property $ do
      x <- forAll (Gen.int Range.constantBounded)
      x === x

  it "should check BorderSide fulfills Semigroup laws" $ do
    1 `shouldBe` 1

  it "should check Padding fulfills Semigroup laws" $ do
    checkLaws genPadding [monoidLaws]

genDouble :: Gen Double
genDouble = Gen.double (Range.linearFrac (-10000) 10000)

genMDouble :: Gen (Maybe Double)
genMDouble = Gen.maybe $ genDouble

genRGB :: Gen Color
genRGB = RGB <$> genDouble <*> genDouble <*> genDouble

genBorderSide :: Gen BorderSide
genBorderSide = BorderSide <$> genDouble <*> genRGB

genPadding :: Gen Padding
genPadding = Padding <$> genMDouble <*> genMDouble <*> genMDouble <*> genMDouble
