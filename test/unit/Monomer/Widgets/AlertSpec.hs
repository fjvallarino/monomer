module Monomer.Widgets.AlertSpec (spec) where

import Control.Lens ((&), (.~))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Alert

import qualified Monomer.Lens as L

data AlertEvent
  = CloseClick
  deriving (Eq, Show)

spec :: Spec
spec = describe "Alert"
  handleEvent

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should generate a close event if clicked outside the dialog" $
    events (Point 3000 3000) `shouldBe` Seq.singleton CloseClick

  it "should generate a close event when clicking the Accept button" $
    events (Point 150 380) `shouldBe` Seq.singleton CloseClick

  it "should not generate a close event when clicking the dialog" $
    events (Point 300 300) `shouldBe` Seq.empty

  where
    wenv = mockWenv () & L.theme .~ darkTheme
    alertInst = alert "Alert!" CloseClick
    events p = instHandleEventEvts wenv [Click p LeftBtn] alertInst
