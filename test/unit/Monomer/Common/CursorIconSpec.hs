{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Common.CursorIconSpec (spec) where

import Control.Lens
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Data.Maybe
import Data.Sequence (Seq(..))
import Data.Text (Text)
import Safe
import Test.Hspec

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Core.Themes.SampleThemes
import Monomer.Event
import Monomer.Main
import Monomer.TestUtil
import Monomer.TestEventUtil
import Monomer.Widgets.Containers.Box
import Monomer.Widgets.Containers.Grid
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.Spacer
import Monomer.Widgets.Singles.TextDropdown

import qualified Monomer.Lens as L

newtype TestModel = TestModel {
  _tmSelectedItem :: Int
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''TestModel

spec :: Spec
spec = describe "Cursor Icon" $ do
  handleEventSimple
  handleEventNested
  handleEventOverlay

handleEventSimple :: Spec
handleEventSimple = describe "handleEventSimple" $ do
  it "should not change the cursor if not event happened" $ do
    icons [] `shouldBe` [CursorArrow]

  it "should not change the cursor if the widget does not have a cursor" $ do
    icons [[evtMove p1]] `shouldBe` [CursorArrow, CursorArrow]

  it "should change the cursor if the widget has a custom cursor" $ do
    icons [[evtMove p2]] `shouldBe` [CursorArrow, CursorHand]
    icons [[evtMove p3]] `shouldBe` [CursorArrow, CursorIBeam]
    icons [[evtMove p4]] `shouldBe` [CursorArrow, CursorInvalid]

  it "should generate the correct sequence of cursors from the events" $ do
    let evtsGroups = [[evtMove p2], [evtMove p3], [evtMove p4]]
    icons evtsGroups `shouldBe` [CursorArrow, CursorHand, CursorIBeam, CursorInvalid]

  where
    wenv = mockWenvEvtUnit ()
    node = vstack [
        label "Test",
        label "Test" `style` [cursorIcon CursorHand],
        label "Test" `style` [cursorIcon CursorIBeam],
        label "Test" `style` [cursorIcon CursorInvalid]
      ]
    icons egs = getIcons wenv node egs
    p1 = Point 100 10
    p2 = Point 100 30
    p3 = Point 100 50
    p4 = Point 100 70

handleEventNested :: Spec
handleEventNested = describe "handleEventNested" $ do
  it "should change the cursor if the widget has a custom cursor" $ do
    icons [[evtMove p11]] `shouldBe` [CursorArrow, CursorArrow]
    icons [[evtMove p21]] `shouldBe` [CursorArrow, CursorSizeH]
    icons [[evtMove p22]] `shouldBe` [CursorArrow, CursorHand]
    icons [[evtMove p31]] `shouldBe` [CursorArrow, CursorSizeV]
    icons [[evtMove p32]] `shouldBe` [CursorArrow, CursorHand]

  it "should generate the correct sequence of cursors from the events" $ do
    let evtsGroups = [[evtMove p11], [evtMove p21], [evtMove p22], [evtMove p31], [evtMove p32]]
    icons evtsGroups `shouldBe` [CursorArrow, CursorArrow, CursorSizeH, CursorHand, CursorSizeV, CursorHand]

  where
    wenv = mockWenvEvtUnit ()
    node = vstack [
        label "Test",
        hgrid [
          hgrid [
            label "Test" `style` [cursorIcon CursorSizeH],
            filler
          ]
        ] `style` [cursorIcon CursorHand],
        hgrid [
          hgrid [
            label "Test" `style` [cursorIcon CursorSizeV],
            filler
          ] `style` [cursorIcon CursorInvalid],
          spacer
        ] `style` [cursorIcon CursorHand]
      ]
    icons egs = getIcons wenv node egs
    p11 = Point 100 10
    p21 = Point 100 30
    p22 = Point 400 30
    p31 = Point 100 50
    p32 = Point 400 50

handleEventOverlay :: Spec
handleEventOverlay = describe "handleEventOverlay" $ do
  it "should not change the cursor if not event happened" $ do
    icons [] `shouldBe` [CursorArrow]

  it "should not show to arrow in overlay area if dropdown is not open" $ do
    let evtsGroups = [[evtMove p1], [evtMove p2], [evtMove p3]]
    icons evtsGroups `shouldBe` [CursorArrow, CursorHand, CursorInvalid, CursorInvalid]

  it "should show arrow in overlay area if dropdown is open" $ do
    let evtsGroups = [[evtMove p1], [evtClick p1], [evtMove p2], [evtMove p3]]
    icons evtsGroups `shouldBe` [CursorArrow, CursorHand, CursorHand, CursorHand, CursorArrow]

  it "should show arrow in overlay area when dropdown is open, invalid after it's closed" $ do
    let evtsGroups = [[evtMove p1], [evtClick p1], [evtMove p3], [evtClick p3]]
    icons evtsGroups `shouldBe` [CursorArrow, CursorHand, CursorHand, CursorArrow, CursorInvalid]

  where
    wenv = mockWenvEvtUnit (TestModel 0)
    node = vstack [
        textDropdown selectedItem [0..10::Int],
        filler
      ] `style` [cursorIcon CursorInvalid]
    icons egs = getIcons wenv node egs
    p1 = Point 100 10
    p2 = Point 100 30
    p3 = Point 100 400

getIcons
  :: Eq s
  => WidgetEnv s e
  -> WidgetNode s e
  -> [[SystemEvent]]
  -> [CursorIcon]
getIcons wenv root evtsGroups = iconsRes where
  firstIcon stack = fromMaybe CursorArrow (headMay stack)
  ctxs = snd <$> tail (nodeHandleEvents_ wenv WInit evtsGroups root)
  cursors = (^.. L.cursorStack . folded . _2) <$> ctxs
  iconsRes = firstIcon <$> cursors
