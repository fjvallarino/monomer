module Monomer.Widgets.Singles.ButtonSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Data.Default
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.Graphics
import Monomer.TestEventUtil
import Monomer.TestUtil
import Monomer.Widgets.Singles.Button

import qualified Monomer.Lens as L

data BtnEvent
  = BtnClick
  | GotFocus Path
  | LostFocus Path
  deriving (Eq, Show)

spec :: Spec
spec = describe "Button" $ do
  handleEvent
  getSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should not generate an event if clicked outside" $
    clickEvts (Point 3000 3000) `shouldBe` Seq.empty

  it "should generate a user provided event when clicked" $
    clickEvts (Point 100 100) `shouldBe` Seq.singleton BtnClick

  it "should generate a user provided event when Enter/Space is pressed" $
    keyEvts keyReturn `shouldBe` Seq.singleton BtnClick

  it "should generate an event when focus is received" $
    events evtFocus `shouldBe` Seq.singleton (GotFocus emptyPath)

  it "should generate an event when focus is lost" $
    events evtBlur `shouldBe` Seq.singleton (LostFocus emptyPath)

  where
    wenv = mockWenv ()
    btnNode = button_ "Click" BtnClick [onFocus GotFocus, onBlur LostFocus]
    clickEvts p = nodeHandleEventEvts wenv [Click p LeftBtn] btnNode
    keyEvts key = nodeHandleEventEvts wenv [KeyAction def key KeyPressed] btnNode
    events evt = nodeHandleEventEvts wenv [evt] btnNode

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  describe "fixed" $ do
    it "should return width = Fixed 50" $
      sizeReqW1 `shouldBe` fixedSize 50

    it "should return height = Fixed 20" $
      sizeReqH1 `shouldBe` fixedSize 20

  describe "flex" $ do
    it "should return width = Flex 70 1" $
      sizeReqW2 `shouldBe` flexSize 70 1

    it "should return height = Flex 20 2" $
      sizeReqH2 `shouldBe` flexSize 20 2

  describe "multi" $ do
    it "should return width = Fixed 50" $
      sizeReqW3 `shouldBe` fixedSize 50

    it "should return height = Flex 60 1" $
      sizeReqH3 `shouldBe` flexSize 60 1

  where
    wenv = mockWenv ()
    btnNode1 = button "Click" BtnClick
    btnNode2 = button_ "Click 2" BtnClick [resizeFactorW 1, resizeFactorH 2]
    btnNode3 = button_ "Line    line    line" BtnClick [multiLine, trimSpaces] `style` [width 50]
    (sizeReqW1, sizeReqH1) = nodeGetSizeReq wenv btnNode1
    (sizeReqW2, sizeReqH2) = nodeGetSizeReq wenv btnNode2
    (sizeReqW3, sizeReqH3) = nodeGetSizeReq wenv btnNode3
