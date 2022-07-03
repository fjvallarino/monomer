{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Monomer.Widgets.Containers.PopupSpec where

import Control.Lens
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Core.Themes.SampleThemes
import Monomer.Event
import Monomer.TestEventUtil
import Monomer.TestUtil
import Monomer.Widgets.Composite
import Monomer.Widgets.Containers.Grid
import Monomer.Widgets.Containers.Popup
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.Spacer
import Monomer.Widgets.Singles.ToggleButton

import qualified Monomer.Lens as L

newtype TestEvent
  = OnPopupChange Bool
  deriving (Eq, Show)

data TestModel = TestModel {
  _tmOpen :: Bool,
  _tmPopupToggle :: Bool
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''TestModel

spec :: Spec
spec = fdescribe "Popup"
  handleEvent

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  describe "basics" $ do
    it "should update the model when the popup is open" $ do
      let evts = [evtClick (Point 50 10)]
      modelBase evts ^. open `shouldBe` True
      eventsBase evts `shouldBe` Seq.fromList [OnPopupChange True]

    it "should close the popup when clicked outside the content" $ do
      let evts = [evtClick (Point 50 10), evtClick (Point 50 100)]
      modelBase evts `shouldBe` TestModel False False
      eventsBase evts `shouldBe` Seq.fromList [OnPopupChange True, OnPopupChange False]

    it "should close the popup when Esc is pressed" $ do
      let evts = [evtClick (Point 50 10), evtK keyEscape]
      modelBase evts `shouldBe` TestModel False False
      eventsBase evts `shouldBe` Seq.fromList [OnPopupChange True, OnPopupChange False]

    it "should close the popup when clicked in the content's toggle button" $ do
      let evts = [evtClick (Point 50 10), evtClick (Point 40 60)]
      modelBase evts `shouldBe` TestModel False False
      eventsBase evts `shouldBe` Seq.fromList [OnPopupChange True, OnPopupChange False]

    it "should not close the popup when clicked in the content's test toggle button" $ do
      let evts = [evtClick (Point 50 10), evtClick (Point 80 60)]
      modelBase evts `shouldBe` TestModel True True
      eventsBase evts `shouldBe` Seq.fromList [OnPopupChange True]

  describe "popupDisabledClose" $ do
    it "should not close the popup when clicked outside the content if popupDisableClose is set" $ do
      let evts = [evtClick (Point 50 10), evtClick (Point 50 100)]
      modelDisable evts `shouldBe` TestModel True False
      eventsDisable evts `shouldBe` Seq.fromList [OnPopupChange True]

    it "should not close the popup when Esc is pressed if popupDisableClose is set" $ do
      let evts = [evtClick (Point 50 10), evtK keyEscape]
      modelDisable evts `shouldBe` TestModel True False
      eventsDisable evts `shouldBe` Seq.fromList [OnPopupChange True]

  describe "alignment" $ do
    it "should toggle the popupToggle button when aligned center to the widget" $ do
      let cfgs = [alignCenter]
      let evts = [evtClick (Point 50 10), evtClick (Point 340 60)]
      modelAlign cfgs evts `shouldBe` TestModel True True
      eventsAlign cfgs evts `shouldBe` Seq.fromList [OnPopupChange True]

    it "should toggle the popupToggle button when aligned top-center to the window" $ do
      let cfgs = [popupAlignToWindow, alignTop, alignCenter]
      let evts = [evtClick (Point 50 10), evtClick (Point 340 10)]
      modelAlign cfgs evts `shouldBe` TestModel True True
      eventsAlign cfgs evts `shouldBe` Seq.fromList [OnPopupChange True]

    it "should toggle the popupToggle button when aligned bottom-right to the window" $ do
      let cfgs = [popupAlignToWindow, alignBottom, alignRight]
      let evts = [evtClick (Point 50 10), evtClick (Point 620 460)]
      modelAlign cfgs evts `shouldBe` TestModel True True
      eventsAlign cfgs evts `shouldBe` Seq.fromList [OnPopupChange True]

  where
    wenv = mockWenv (TestModel False False)
      & L.theme .~ darkTheme
    handleEvent :: EventHandler TestModel TestEvent TestModel TestEvent
    handleEvent wenv node model evt = [ Report evt ]
    buildUI cfgs wenv model = vstack [
        toggleButton "Open" open,
        popup_ open cfgs $ hgrid [
            toggleButton "Open" open,
            toggleButton "Test" popupToggle
          ],
        filler
      ]
    popupNode cfgs = composite "popupNode" id (buildUI cfgs) handleEvent

    modelBase es = nodeHandleEventModel wenv es (popupNode [])
    modelDisable es = nodeHandleEventModel wenv es (popupNode [popupDisableClose])
    modelAlign cfgs es = nodeHandleEventModel wenv es (popupNode (popupDisableClose : cfgs))

    eventsBase es = nodeHandleEventEvts wenv es (popupNode [onChange OnPopupChange])
    eventsDisable es = nodeHandleEventEvts wenv es (popupNode [onChange OnPopupChange, popupDisableClose])
    eventsAlign cfgs es = nodeHandleEventEvts wenv es (popupNode (onChange OnPopupChange : popupDisableClose : cfgs))
