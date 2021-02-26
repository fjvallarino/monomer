module Monomer.Widgets.ButtonSpec (spec) where

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
import Monomer.TestUtil
import Monomer.Widgets.Button

import qualified Monomer.Lens as L

data BtnEvent
  = BtnClick
  | GotFocus
  | LostFocus
  deriving (Eq, Show)

spec :: Spec
spec = describe "Button" $ do
  handleEvent
  getSizeReq
  getSizeReqMerge

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should not generate an event if clicked outside" $
    clickEvts (Point 3000 3000) `shouldBe` Seq.empty

  it "should generate a user provided event when clicked" $
    clickEvts (Point 100 100) `shouldBe` Seq.singleton BtnClick

  it "should generate a user provided event when Enter/Space is pressed" $
    keyEvts keyReturn `shouldBe` Seq.singleton BtnClick

  it "should generate an event when focus is received" $
    events Focus `shouldBe` Seq.singleton GotFocus

  it "should generate an event when focus is lost" $
    events Blur `shouldBe` Seq.singleton LostFocus

  where
    wenv = mockWenv ()
    btnNode = button_ "Click" BtnClick [onFocus GotFocus, onBlur LostFocus]
    clickEvts p = nodeHandleEventEvts wenv [Click p LeftBtn] btnNode
    keyEvts key = nodeHandleEventEvts wenv [KeyAction def key KeyPressed] btnNode
    events evt = nodeHandleEventEvts wenv [evt] btnNode

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return width = Flex 50 0.01" $
    sizeReqW `shouldBe` expandSize 50 0.01

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` fixedSize 20

  it "should return width = Flex 70 1" $
    sizeReq2W `shouldBe` expandSize 70 1

  it "should return height = Flex 20 2" $
    sizeReq2H `shouldBe` expandSize 20 2

  where
    wenv = mockWenv ()
    btnNode = button "Click" BtnClick
    btnNode2 = button_ "Click 2" BtnClick [resizeFactorW 1, resizeFactorH 2]
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv btnNode
    (sizeReq2W, sizeReq2H) = nodeGetSizeReq wenv btnNode2

getSizeReqMerge :: Spec
getSizeReqMerge = describe "getSizeReqMerge" $ do
  it "should return width = Flex 192 0.01" $
    sizeReqW `shouldBe` expandSize 192 0.01

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` fixedSize 20

  it "should return width = Flex 360 0.01" $
    sizeReq2W `shouldBe` expandSize 360 0.01

  it "should return height = Fixed 20" $
    sizeReq2H `shouldBe` fixedSize 20

  where
    renderer = mockRenderer {
      computeTextSize = mockTextSize Nothing,
      computeGlyphsPos = mockGlyphsPos Nothing
    }
    wenv = mockWenv ()
      & L.renderer .~ renderer
    btnNode = nodeInit wenv (button "Button" BtnClick)
    btnNode2 = button "Button" BtnClick `style` [textSize 60]
    btnRes = widgetMerge (btnNode ^. L.widget) wenv btnNode btnNode2
    WidgetResult btnMerged _ _ = btnRes
    btnInfo = btnNode ^. L.info
    mrgInfo = btnMerged ^. L.info
    (sizeReqW, sizeReqH) = (btnInfo ^. L.sizeReqW, btnInfo ^. L.sizeReqH)
    (sizeReq2W, sizeReq2H) = (mrgInfo ^. L.sizeReqW, mrgInfo ^. L.sizeReqH)
