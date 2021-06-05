{-|
Module      : Monomer.Widgets.Containers.AlertSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for Alert dialog widget.
-}
module Monomer.Widgets.Containers.AlertSpec (spec) where

import Control.Lens ((&), (.~))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Themes.SampleThemes
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Containers.Alert

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
    events (Point 50 450) `shouldBe` Seq.singleton CloseClick

  it "should not generate a close event when clicking the dialog" $
    events (Point 300 200) `shouldBe` Seq.empty

  where
    wenv = mockWenv () & L.theme .~ darkTheme
    alertNode = alertMsg "Alert!" CloseClick
    events p = nodeHandleEventEvts wenv [Click p BtnLeft] alertNode
