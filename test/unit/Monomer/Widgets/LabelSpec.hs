module Monomer.Widgets.LabelSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Label

spec :: Spec
spec = describe "Label" $ do
  updateSizeReq
  updateSizeReqMulti
  updateSizeReqMultiKeepSpaces

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  it "should return width = Flex 100 0.01" $
    sizeReqW `shouldBe` FlexSize 100 0.01

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` FixedSize 20

  it "should return width = Flex 120 1" $
    sizeReq2W `shouldBe` FlexSize 120 1

  it "should return height = Flex 20 2" $
    sizeReq2H `shouldBe` FlexSize 20 2

  where
    wenv = mockWenv ()
    lblNode = label "Test label"
    lblNode2 = label_ "Test label 2" [resizeFactorW 1, resizeFactorH 2]
    (sizeReqW, sizeReqH) = nodeUpdateSizeReq wenv lblNode
    (sizeReq2W, sizeReq2H) = nodeUpdateSizeReq wenv lblNode2

updateSizeReqMulti :: Spec
updateSizeReqMulti = describe "updateSizeReq" $ do
  it "should return width = Fixed 50" $
    sizeReqW `shouldBe` FixedSize 50

  it "should return height = Fixed 60" $
    sizeReqH `shouldBe` FixedSize 60

  where
    wenv = mockWenv ()
    lblNode = label_ "Line    line    line" [textMultiLine] `style` [width 50]
    (sizeReqW, sizeReqH) = nodeUpdateSizeReq wenv lblNode

updateSizeReqMultiKeepSpaces :: Spec
updateSizeReqMultiKeepSpaces = describe "updateSizeReq" $ do
  it "should return width = Max 50 1" $
    sizeReqW `shouldBe` MaxSize 50 1

  it "should return height = Fixed 100" $
    sizeReqH `shouldBe` FixedSize 100

  where
    wenv = mockWenv ()
    caption = "Line    line    line"
    lblNode = label_ caption [textMultiLine, textKeepSpaces] `style` [maxWidth 50]
    (sizeReqW, sizeReqH) = nodeUpdateSizeReq wenv lblNode
