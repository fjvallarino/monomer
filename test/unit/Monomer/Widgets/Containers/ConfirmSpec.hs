{-|
Module      : Monomer.Widgets.Containers.ConfirmSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for Confirm dialog widget.
-}
module Monomer.Widgets.Containers.ConfirmSpec (spec) where

import Control.Lens ((&), (.~))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Themes.SampleThemes
import Monomer.Event
import Monomer.TestEventUtil
import Monomer.TestUtil
import Monomer.Widgets.Containers.Confirm

import qualified Monomer.Lens as L

data ConfirmEvent
  = AcceptClick
  | CancelClick
  deriving (Eq, Show)

spec :: Spec
spec = describe "Confirm"
  handleEvent

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should generate a close event if clicked outside the dialog" $
    events (Point 3000 3000) `shouldBe` Seq.singleton CancelClick

  it "should generate an Accept event when clicking the Accept button" $
    events (Point 500 280) `shouldBe` Seq.singleton AcceptClick

  it "should generate a Cancel event when clicking the Cancel button" $
    events (Point 600 280) `shouldBe` Seq.singleton CancelClick

  it "should not generate a close event when clicking the dialog" $
    events (Point 300 200) `shouldBe` Seq.empty

  where
    wenv = mockWenv () & L.theme .~ darkTheme
    confirmNode = confirmMsg "Confirm!" AcceptClick CancelClick
    events p = nodeHandleEventEvts wenv [evtClick p] confirmNode
