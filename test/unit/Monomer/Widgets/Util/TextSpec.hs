module Monomer.Widgets.Util.TextSpec (spec) where

import Control.Lens ((^.), ix)
import Data.Default
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Graphics
import Monomer.Widgets.Util.Text
import Monomer.TestUtil

import qualified Monomer.Lens as L

spec :: Spec
spec = fdescribe "Text" $ do
  fitTextSingle
  fitTextMulti

fitTextSingle :: Spec
fitTextSingle = describe "fitTextToRect single line" $ do
  it "should return the same text, trimmed, if it fits" $
    elpsTrim "Text " ^. ix 0 . L.text `shouldBe` "Text"

  it "should return the same text, untrimmed, if it fits" $
    elpsKeep "Text " ^. ix 0 . L.text `shouldBe` "Text "

  it "should return text with ellipsis, trimmed, if it does not fit" $ do
    elpsTrim "This is longer" `shouldSatisfy` singleElement
    elpsTrim "This is longer" ^. ix 0 . L.text `shouldBe` "This is..."
    elpsTrim "This is a bit longer" `shouldSatisfy` singleElement
    elpsTrim "This is a bit longer" ^. ix 0 . L.text `shouldBe` "This is a..."

  it "should return text with ellipsis, untrimmed, if it does not fit" $ do
    elpsKeep "This is longer" `shouldSatisfy` singleElement
    elpsKeep "This is longer" ^. ix 0 . L.text `shouldBe` "This is ..."
    elpsKeep "This is a bit longer" `shouldSatisfy` singleElement
    elpsKeep "This is a bit longer" ^. ix 0 . L.text `shouldBe` "This is a..."

  it "should return text clipped, trimmed, if it does not fit" $ do
    clipTrim "This is longer" `shouldSatisfy` singleElement
    clipTrim "This is longer" ^. ix 0 . L.text `shouldBe` "This is"
    clipTrim "This is a bit longer" `shouldSatisfy` singleElement
    clipTrim "This is a bit longer" ^. ix 0 . L.text `shouldBe` "This is a"

  it "should return text clipped, untrimmed, if it does not fit" $ do
    clipKeep "This is longer" `shouldSatisfy` singleElement
    clipKeep "This is longer" ^. ix 0 . L.text `shouldBe` "This is "
    clipKeep "This is a bit longer" `shouldSatisfy` singleElement
    clipKeep "This is a bit longer" ^. ix 0 . L.text `shouldBe` "This is a "

  where
    wenv = mockWenv ()
    style = def
    rect = Rect 0 0 120 20
    fitText = fitTextToRect
    elpsTrim text = fitText wenv style Ellipsis SingleLine TrimSpaces rect text
    elpsKeep text = fitText wenv style Ellipsis SingleLine KeepSpaces rect text
    clipTrim text = fitText wenv style ClipText SingleLine TrimSpaces rect text
    clipKeep text = fitText wenv style ClipText SingleLine KeepSpaces rect text
    singleElement sq = Seq.length sq == 1

fitTextMulti :: Spec
fitTextMulti = describe "fitTextToRect single line" $ do
  it "should return the same text, trimmed, if it fits" $
    elpsTrim "Text " ^. ix 0 . L.text `shouldBe` "Text"

  it "should return the same text, untrimmed, if it fits" $
    elpsKeep "Text " ^. ix 0 . L.text `shouldBe` "Text "

  it "should return text with ellipsis, trimmed, if it does not fit" $ do
    elpsTrim "This is    really-long" `shouldSatisfy` elementCount 2
    elpsTrim "This is    really-long" ^. ix 0 . L.text `shouldBe` "This is"
    elpsTrim "This is    really-long" ^. ix 1 . L.text `shouldBe` "really-long"
    elpsTrim "This is    a tad bit longer" `shouldSatisfy` elementCount 2
    elpsTrim "This is    a tad bit longer" ^. ix 0 . L.text `shouldBe` "This is"
    elpsTrim "This is    a tad bit longer" ^. ix 1 . L.text `shouldBe` "a tad..."

  it "should return text with ellipsis, untrimmed, if it does not fit" $ do
    elpsKeep "This is    really-long" `shouldSatisfy` elementCount 2
    elpsKeep "This is    really-long" ^. ix 0 . L.text `shouldBe` "This is "
    elpsKeep "This is    really-long" ^. ix 1 . L.text `shouldBe` "   ..."
    elpsKeep "This is    a tad bit longer" `shouldSatisfy` elementCount 2
    elpsKeep "This is    a tad bit longer" ^. ix 0 . L.text `shouldBe` "This is "
    elpsKeep "This is    a tad bit longer" ^. ix 1 . L.text `shouldBe` "   a ..."

  it "should return text clipped, trimmed, if it does not fit" $ do
    clipTrim "This is    really-long" `shouldSatisfy` elementCount 2
    clipTrim "This is    really-long" ^. ix 0 . L.text `shouldBe` "This is"
    clipTrim "This is    really-long" ^. ix 1 . L.text `shouldBe` "really-long"
    clipTrim "This is    a tad bit longer" `shouldSatisfy` elementCount 2
    clipTrim "This is    a tad bit longer" ^. ix 0 . L.text `shouldBe` "This is"
    clipTrim "This is    a tad bit longer" ^. ix 1 . L.text `shouldBe` "a tad"

  it "should return text clipped, untrimmed, if it does not fit" $ do
    clipKeep "This is    really-long" `shouldSatisfy` elementCount 2
    clipKeep "This is    really-long" ^. ix 0 . L.text `shouldBe` "This is "
    clipKeep "This is    really-long" ^. ix 1 . L.text `shouldBe` "   "
    clipKeep "This is    a tad bit longer" `shouldSatisfy` elementCount 2
    clipKeep "This is    a tad bit longer" ^. ix 0 . L.text `shouldBe` "This is "
    clipKeep "This is    a tad bit longer" ^. ix 1 . L.text `shouldBe` "   a tad"

  where
    wenv = mockWenv ()
    style = def
    rect = Rect 0 0 80 40
    fitText = fitTextToRect
    elpsTrim text = fitText wenv style Ellipsis MultiLine TrimSpaces rect text
    elpsKeep text = fitText wenv style Ellipsis MultiLine KeepSpaces rect text
    clipTrim text = fitText wenv style ClipText MultiLine TrimSpaces rect text
    clipKeep text = fitText wenv style ClipText MultiLine KeepSpaces rect text
    elementCount count sq = Seq.length sq == count
