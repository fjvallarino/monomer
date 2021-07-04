{-|
Module      : Monomer.Widgets.Util.TextSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for Text handling.
-}
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
fitTextSingle = describe "fitTextToSize single line" $ do
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
    elpsTrim "This is longer\nMore" ^. ix 0 . L.text `shouldBe` "This is l..."
    elpsTrim "This is a bit longer\nMore" `shouldSatisfy` singleElement
    elpsTrim "This is a bit longer\nMore" ^. ix 0 . L.text `shouldBe` "This is a..."

  it "should return text with ellipsis, untrimmed, if it does not fit" $ do
    elpsKeep "This is longer\nMore" `shouldSatisfy` singleElement
    elpsKeep "This is longer\nMore" ^. ix 0 . L.text `shouldBe` "This is l..."
    elpsKeep "This is a bit longer\nMore" `shouldSatisfy` singleElement
    elpsKeep "This is a bit longer\nMore" ^. ix 0 . L.text `shouldBe` "This is a..."

  it "should return text clipped, trimmed, if it does not fit" $ do
    clipTrim "This is longer\nMore" `shouldSatisfy` singleElement
    clipTrim "This is longer\nMore" ^. ix 0 . L.text `shouldBe` "This is long"
    clipTrim "This is not a bit longer\nMore" `shouldSatisfy` singleElement
    clipTrim "This is not a bit longer\nMore" ^. ix 0 . L.text `shouldBe` "This is not"

  it "should return text clipped, untrimmed, if it does not fit" $ do
    clipKeep "This is longer\nMore" `shouldSatisfy` singleElement
    clipKeep "This is longer\nMore" ^. ix 0 . L.text `shouldBe` "This is long"
    clipKeep "This is not a bit longer\nMore" `shouldSatisfy` singleElement
    clipKeep "This is not a bit longer\nMore" ^. ix 0 . L.text `shouldBe` "This is not "

  where
    wenv = mockWenv ()
    fontMgr = wenv ^. L.fontManager
    style = def
    sizeE = Size 120 20
    sizeC = Size 120 10
    elpsTrim text = fitTextToSize fontMgr style Ellipsis SingleLine TrimSpaces Nothing sizeE text
    elpsKeep text = fitTextToSize fontMgr style Ellipsis SingleLine KeepSpaces Nothing sizeE text
    clipTrim text = fitTextToSize fontMgr style ClipText SingleLine TrimSpaces Nothing sizeC text
    clipKeep text = fitTextToSize fontMgr style ClipText SingleLine KeepSpaces Nothing sizeC text
    singleElement sq = Seq.length sq == 1

fitTextMulti :: Spec
fitTextMulti = describe "fitTextToSize single line" $ do
  it "should return the same text, trimmed, if it fits" $ do
    elpsTrim "Text " `shouldSatisfy` elementCount 1
    elpsTrim "Text " ^. ix 0 . L.text `shouldBe` "Text"

  it "should return the same text, untrimmed, if it fits" $ do
    elpsKeep "Text " `shouldSatisfy` elementCount 1
    elpsKeep "Text " ^. ix 0 . L.text `shouldBe` "Text "

  -- Text.lines does not return an extra line if the last element is \n
  it "should return several empty lines" $ do
    elpsTrim_ sizeTall "Text\n\n\n\n" `shouldSatisfy` elementCount 4
    elpsKeep_ sizeTall "Text\n\n\n\n" `shouldSatisfy` elementCount 4
    clipTrim_ sizeTall "Text\n\n\n\n" `shouldSatisfy` elementCount 4
    clipKeep_ sizeTall "Text\n\n\n\n" `shouldSatisfy` elementCount 4

  it "should return several empty lines and one with text" $ do
    elpsTrim_ sizeTall "Text\n\n\n\nend" `shouldSatisfy` elementCount 5
    elpsKeep_ sizeTall "Text\n\n\n\nend" `shouldSatisfy` elementCount 5
    clipTrim_ sizeTall "Text\n\n\n\nend" `shouldSatisfy` elementCount 5
    clipKeep_ sizeTall "Text\n\n\n\nend" `shouldSatisfy` elementCount 5

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
    fontMgr = wenv ^. L.fontManager
    style = def
    sizeE = Size 80 40
    sizeC = Size 80 50
    sizeTall = Size 80 200
    elpsTrim text = elpsTrim_ sizeE text
    elpsKeep text = elpsKeep_ sizeE text
    clipTrim text = clipTrim_ sizeC text
    clipKeep text = clipKeep_ sizeC text
    elpsTrim_ size text = fitTextToSize fontMgr style Ellipsis MultiLine TrimSpaces Nothing size text
    elpsKeep_ size text = fitTextToSize fontMgr style Ellipsis MultiLine KeepSpaces Nothing size text
    clipTrim_ size text = fitTextToSize fontMgr style ClipText MultiLine TrimSpaces Nothing size text
    clipKeep_ size text = fitTextToSize fontMgr style ClipText MultiLine KeepSpaces Nothing size text
    elementCount count sq = Seq.length sq == count
