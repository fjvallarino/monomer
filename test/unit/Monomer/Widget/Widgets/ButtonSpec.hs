{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.ButtonSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Tree (rootPath)
import Monomer.Event.Types
import Monomer.Widget.Types
import Monomer.Widget.TestUtil
import Monomer.Widget.Util
import Monomer.Widget.Widgets.Button

data BtnEvent
  = BtnClick
  deriving (Eq, Show)

spec :: Spec
spec = describe "Button" $ do
  handleEvent
  updateSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should not generate an event if clicked outside" $
    events (Point 3000 3000) `shouldBe` Seq.empty

  it "should generate a user provided event" $
    events (Point 100 100) `shouldBe` Seq.singleton BtnClick

  where
    wenv = mockWenv ()
    btn = initWidget wenv (button BtnClick "Click")
    events p = instanceGetEvents wenv (Click p LeftBtn) btn

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  it "should return the expected size" $
    _srSize `shouldBe` Size 50 20

  it "should return Flexible width policy" $
    _srPolicyW `shouldBe` FlexibleSize

  it "should return Strict height policy" $
    _srPolicyH `shouldBe` StrictSize

  where
    wenv = mockWenv ()
    btnInst = button Click "Click"
    SizeReq{..} = instanceUpdateSizeReq wenv btnInst
