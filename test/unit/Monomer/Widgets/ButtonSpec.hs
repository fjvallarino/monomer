module Monomer.Widgets.ButtonSpec (spec) where

import Data.Default
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Button

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
    clickEvts (Point 3000 3000) `shouldBe` Seq.empty

  it "should generate a user provided event when clicked" $
    clickEvts (Point 100 100) `shouldBe` Seq.singleton BtnClick

  it "should generate a user provided event when Enter/Space is pressed" $
    keyEvts keyReturn `shouldBe` Seq.singleton BtnClick

  where
    wenv = mockWenv ()
    btn = instInit wenv (button "Click" BtnClick)
    clickEvts p = instHandleEventEvts wenv [Click p LeftBtn] btn
    keyEvts key = instHandleEventEvts wenv [KeyAction def key KeyPressed] btn

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  it "should return width = Flex 50 1" $
    sizeReqW `shouldBe` FlexSize 50 1

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` FixedSize 20

  where
    wenv = mockWenv ()
    btnInst = button "Click" BtnClick
    (sizeReqW, sizeReqH) = instUpdateSizeReq wenv btnInst
