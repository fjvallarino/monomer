module Monomer.Widgets.LabelSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Label

spec :: Spec
spec = fdescribe "Label" $ do
  updateSizeReq
  updateSizeReqMulti
  --updateSizeReqMultiTrim

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  it "should return Flex width = 100" $
    sizeReqW `shouldBe` FlexSize 100 1

  it "should return Fixed height = 20" $
    sizeReqH `shouldBe` FixedSize 20

  where
    wenv = mockWenv ()
    lblInst = label "Test label"
    (sizeReqW, sizeReqH) = instUpdateSizeReq wenv lblInst

updateSizeReqMulti :: Spec
updateSizeReqMulti = describe "updateSizeReq" $ do
  it "should return Fixed width = 50" $
    sizeReqW `shouldBe` FixedSize 50

  it "should return Fixed height = 60" $
    sizeReqH `shouldBe` FixedSize 60

  where
    wenv = mockWenv ()
    lblInst = label_ "Line line line" [textMultiLine] `style` [width 50]
    (sizeReqW, sizeReqH) = instUpdateSizeReq wenv lblInst

updateSizeReqMultiTrim :: Spec
updateSizeReqMultiTrim = describe "updateSizeReq" $ do
  it "should return Fixed width = 50" $
    sizeReqW `shouldBe` FixedSize 50

  it "should return Fixed height = 60" $
    sizeReqH `shouldBe` FixedSize 60

  where
    wenv = mockWenv ()
    caption = "Line line line"
    lblInst = label_ caption [textMultiLine, textTrim] `style` [maxWidth 50]
    (sizeReqW, sizeReqH) = instUpdateSizeReq wenv lblInst
