{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.LabelSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Tree (rootPath)
import Monomer.Event.Types
import Monomer.Widget.Types
import Monomer.Widget.TestUtil
import Monomer.Widget.Util
import Monomer.Widget.Widgets.Label

spec :: Spec
spec = describe "Label"
  updateSizeReq

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  it "should return the expected size" $
    _srSize `shouldBe` Size 100 20

  it "should return Flexible width policy" $
    _srPolicyWidth `shouldBe` FlexibleSize

  it "should return Strict height policy" $
    _srPolicyHeight `shouldBe` StrictSize

  where
    wenv = mockWenv ()
    lblInst = label "Test label"
    SizeReq{..} = instanceUpdateSizeReq wenv lblInst
