module GUI.Widget.CoreSpec where

import Test.Hspec

spec :: Spec
spec = describe "Widget.Core" $ do
  stubSpec

stubSpec :: Spec
stubSpec =
  describe "Stub" $ do
    it "does not really test anything" $ do
      2 `shouldBe` (1 + 1)
