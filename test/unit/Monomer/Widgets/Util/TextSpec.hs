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
spec = describe "Text" $ do
  fitTextSingle
  fitTextMulti

fitTextSingle :: Spec
fitTextSingle = describe "fitTextToRect single line" $ do
  it "should return the same empty text, trimmed (ellipsis)" $
    elpsTrim "" ^. ix 0 . L.text `shouldBe` ""

  it "should return the same empty text, untrimmed (ellipsis)" $
    elpsKeep "" ^. ix 0 . L.text `shouldBe` ""

  it "should return the same empty text, trimmed (clip)" $
    clipTrim "" ^. ix 0 . L.text `shouldBe` ""

  it "should return the same empty text, untrimmed (clip)" $
    clipKeep "" ^. ix 0 . L.text `shouldBe` ""

  it "should return the same text, trimmed, if it fits (ellipsis)" $
    elpsTrim "Text " ^. ix 0 . L.text `shouldBe` "Text"

  it "should return the same text, untrimmed, if it fits (ellipsis)" $
    elpsKeep "Text " ^. ix 0 . L.text `shouldBe` "Text "

  it "should return the same text, trimmed, if it fits (clip)" $
    clipTrim "Text " ^. ix 0 . L.text `shouldBe` "Text"

  it "should return the same text, untrimmed, if it fits (clip)" $
    clipKeep "Text " ^. ix 0 . L.text `shouldBe` "Text "

  it "should return text with ellipsis, trimmed, if it does not fit" $ do
    elpsTrim "This is longer\nMore" `shouldSatisfy` singleElement
    elpsTrim "This is longer\nMore" ^. ix 0 . L.text `shouldBe` "This is..."
    elpsTrim "This is a bit longer\nMore" `shouldSatisfy` singleElement
    elpsTrim "This is a bit longer\nMore" ^. ix 0 . L.text `shouldBe` "This is a..."

  it "should return text with ellipsis, untrimmed, if it does not fit" $ do
    elpsKeep "This is longer\nMore" `shouldSatisfy` singleElement
    elpsKeep "This is longer\nMore" ^. ix 0 . L.text `shouldBe` "This is ..."
    elpsKeep "This is a bit longer\nMore" `shouldSatisfy` singleElement
    elpsKeep "This is a bit longer\nMore" ^. ix 0 . L.text `shouldBe` "This is a..."

  it "should return text clipped, trimmed, if it does not fit" $ do
    clipTrim "This is longer\nMore" `shouldSatisfy` singleElement
    clipTrim "This is longer\nMore" ^. ix 0 . L.text `shouldBe` "This is"
    clipTrim "This is a bit longer\nMore" `shouldSatisfy` singleElement
    clipTrim "This is a bit longer\nMore" ^. ix 0 . L.text `shouldBe` "This is a"

  it "should return text clipped, untrimmed, if it does not fit" $ do
    clipKeep "This is longer\nMore" `shouldSatisfy` singleElement
    clipKeep "This is longer\nMore" ^. ix 0 . L.text `shouldBe` "This is "
    clipKeep "This is a bit longer\nMore" `shouldSatisfy` singleElement
    clipKeep "This is a bit longer\nMore" ^. ix 0 . L.text `shouldBe` "This is a "

  where
    wenv = mockWenv ()
    renderer = wenv ^. L.renderer
    style = def
    rectE = Rect 0 0 120 20
    rectC = Rect 0 0 120 10
    elpsTrim text = fitTextToRect renderer style Ellipsis SingleLine TrimSpaces rectE text
    elpsKeep text = fitTextToRect renderer style Ellipsis SingleLine KeepSpaces rectE text
    clipTrim text = fitTextToRect renderer style ClipText SingleLine TrimSpaces rectC text
    clipKeep text = fitTextToRect renderer style ClipText SingleLine KeepSpaces rectC text
    singleElement sq = Seq.length sq == 1

fitTextMulti :: Spec
fitTextMulti = describe "fitTextToRect single line" $ do
  it "should return the same text, trimmed, if it fits" $ do
    elpsTrim "Text " `shouldSatisfy` elementCount 1
    elpsTrim "Text " ^. ix 0 . L.text `shouldBe` "Text"

  it "should return the same text, untrimmed, if it fits" $ do
    elpsKeep "Text " `shouldSatisfy` elementCount 1
    elpsKeep "Text " ^. ix 0 . L.text `shouldBe` "Text "

  -- Text.lines does not return an extra line if the last element is \n
  it "should return several empty lines" $ do
    elpsTrim_ rectTall "Text\n\n\n\n" `shouldSatisfy` elementCount 4
    elpsKeep_ rectTall "Text\n\n\n\n" `shouldSatisfy` elementCount 4
    clipTrim_ rectTall "Text\n\n\n\n" `shouldSatisfy` elementCount 4
    clipKeep_ rectTall "Text\n\n\n\n" `shouldSatisfy` elementCount 4

  it "should return several empty lines and one with text" $ do
    elpsTrim_ rectTall "Text\n\n\n\nend" `shouldSatisfy` elementCount 5
    elpsKeep_ rectTall "Text\n\n\n\nend" `shouldSatisfy` elementCount 5
    clipTrim_ rectTall "Text\n\n\n\nend" `shouldSatisfy` elementCount 5
    clipKeep_ rectTall "Text\n\n\n\nend" `shouldSatisfy` elementCount 5

  it "should return text with ellipsis, trimmed, if it does not fit" $ do
    elpsTrim "This is    really-long\nMore" `shouldSatisfy` elementCount 2
    elpsTrim "This is    really-long\nMore" ^. ix 0 . L.text `shouldBe` "This is"
    elpsTrim "This is    really-long\nMore" ^. ix 1 . L.text `shouldBe` "reall..."
    elpsTrim "This is    a tad bit longer\nMore" `shouldSatisfy` elementCount 2
    elpsTrim "This is    a tad bit longer\nMore" ^. ix 0 . L.text `shouldBe` "This is"
    elpsTrim "This is    a tad bit longer\nMore" ^. ix 1 . L.text `shouldBe` "a tad..."

  it "should return text with ellipsis, untrimmed, if it does not fit" $ do
    elpsKeep "This is    really-long\nMore" `shouldSatisfy` elementCount 2
    elpsKeep "This is    really-long\nMore" ^. ix 0 . L.text `shouldBe` "This is "
    elpsKeep "This is    really-long\nMore" ^. ix 1 . L.text `shouldBe` "   ..."
    elpsKeep "This is    a tad bit longer\nMore" `shouldSatisfy` elementCount 2
    elpsKeep "This is    a tad bit longer\nMore" ^. ix 0 . L.text `shouldBe` "This is "
    elpsKeep "This is    a tad bit longer\nMore" ^. ix 1 . L.text `shouldBe` "   a ..."

  it "should return text clipped, trimmed, if it does not fit" $ do
    clipTrim "This is    really-long\nMore" `shouldSatisfy` elementCount 3
    clipTrim "This is    really-long\nMore" ^. ix 0 . L.text `shouldBe` "This is"
    clipTrim "This is    really-long\nMore" ^. ix 1 . L.text `shouldBe` "really-long"
    clipTrim "This is    really-long\nMore" ^. ix 2 . L.text `shouldBe` "More"
    clipTrim "This is    a tad bit longer\nMore" `shouldSatisfy` elementCount 3
    clipTrim "This is    a tad bit longer\nMore" ^. ix 0 . L.text `shouldBe` "This is"
    clipTrim "This is    a tad bit longer\nMore" ^. ix 1 . L.text `shouldBe` "a tad"
    clipTrim "This is    a tad bit longer\nMore" ^. ix 2 . L.text `shouldBe` "bit"

  it "should return text clipped, untrimmed, if it does not fit" $ do
    clipKeep "This is    really-long\nMore" `shouldSatisfy` elementCount 3
    clipKeep "This is    really-long\nMore" ^. ix 0 . L.text `shouldBe` "This is "
    clipKeep "This is    really-long\nMore" ^. ix 1 . L.text `shouldBe` "   "
    clipKeep "This is    really-long\nMore" ^. ix 2 . L.text `shouldBe` "really-long"
    clipKeep "This is    a tad bit longer\nMore" `shouldSatisfy` elementCount 3
    clipKeep "This is    a tad bit longer\nMore" ^. ix 0 . L.text `shouldBe` "This is "
    clipKeep "This is    a tad bit longer\nMore" ^. ix 1 . L.text `shouldBe` "   a tad"
    clipKeep "This is    a tad bit longer\nMore" ^. ix 2 . L.text `shouldBe` " bit "

  where
    wenv = mockWenv ()
    renderer = wenv ^. L.renderer
    style = def
    rectE = Rect 0 0 80 40
    rectC = Rect 0 0 80 50
    rectTall = Rect 0 0 80 200
    elpsTrim text = elpsTrim_ rectE text
    elpsKeep text = elpsKeep_ rectE text
    clipTrim text = clipTrim_ rectC text
    clipKeep text = clipKeep_ rectC text
    elpsTrim_ rect text = fitTextToRect renderer style Ellipsis MultiLine TrimSpaces rect text
    elpsKeep_ rect text = fitTextToRect renderer style Ellipsis MultiLine KeepSpaces rect text
    clipTrim_ rect text = fitTextToRect renderer style ClipText MultiLine TrimSpaces rect text
    clipKeep_ rect text = fitTextToRect renderer style ClipText MultiLine KeepSpaces rect text
    elementCount count sq = Seq.length sq == count
