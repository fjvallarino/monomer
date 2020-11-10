module Monomer.Widgets.LabelSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Label

spec :: Spec
spec = describe "Label"
  updateSizeReq

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
