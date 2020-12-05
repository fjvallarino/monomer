module Monomer.Widgets.ButtonSpec (spec) where

import Data.Default
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Button

data BtnEvent
  = BtnClick
  | GotFocus
  | LostFocus
  deriving (Eq, Show)

spec :: Spec
spec = describe "Button" $ do
  handleEvent
  updateSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should not generate an event if clicked outside" $
    clickEvts (Point 3000 3000) `shouldBe` []

  it "should generate a user provided event when clicked" $
    clickEvts (Point 100 100) `shouldBe` [BtnClick]

  it "should generate a user provided event when Enter/Space is pressed" $
    keyEvts keyReturn `shouldBe` [BtnClick]

  it "should generate an event when focus is received" $
    events Focus `shouldBe` [GotFocus]

  it "should generate an event when focus is lost" $
    events Blur `shouldBe` [LostFocus]

  where
    wenv = mockWenv ()
    btnNode = button_ "Click" BtnClick [onFocus GotFocus, onBlur LostFocus]
    clickEvts p = nodeHandleEventEvts wenv [Click p LeftBtn] btnNode
    keyEvts key = nodeHandleEventEvts wenv [KeyAction def key KeyPressed] btnNode
    events evt = nodeHandleEventEvts wenv [evt] btnNode

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  it "should return width = Flex 50 0.01" $
    sizeReqW `shouldBe` FlexSize 50 0.01

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` FixedSize 20

  it "should return width = Flex 70 1" $
    sizeReq2W `shouldBe` FlexSize 70 1

  it "should return height = Flex 20 2" $
    sizeReq2H `shouldBe` FlexSize 20 2

  where
    wenv = mockWenv ()
    btnNode = button "Click" BtnClick
    btnNode2 = button_ "Click 2" BtnClick [resizeFactorW 1, resizeFactorH 2]
    (sizeReqW, sizeReqH) = nodeUpdateSizeReq wenv btnNode
    (sizeReq2W, sizeReq2H) = nodeUpdateSizeReq wenv btnNode2
