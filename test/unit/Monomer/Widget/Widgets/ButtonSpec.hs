{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.ButtonSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
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
  preferredSize
  render

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should not generate an event if clicked outside" $
    events (Point 3000 3000) `shouldBe` Seq.empty

  it "should generate a user provided event" $
    events (Point 100 100) `shouldBe` Seq.singleton BtnClick

  where
    wenv = mockWenv ()
    btn = initWidget wenv (button BtnClick "Click")
    widget = _wiWidget btn
    click p = _widgetHandleEvent widget wenv rootPath (Click p LeftBtn) btn
    events p = maybe Seq.empty _wrEvents (click p)

preferredSize :: Spec
preferredSize = describe "preferredSize" $ do
  it "should return the expected size" $
    _srSize `shouldBe` Size 50 20

  it "should return Flexible width policy" $
    _srPolicyWidth `shouldBe` FlexibleSize

  it "should return Strict height policy" $
    _srPolicyHeight `shouldBe` StrictSize

  where
    wenv = mockWenv ()
    btnInst = button Click "Click"
    SizeReq{..} = instancePreferredSize wenv btnInst

render :: Spec
render = describe "render" $
  it "should render the button" $
    pendingWith "Need to mock Renderer"
