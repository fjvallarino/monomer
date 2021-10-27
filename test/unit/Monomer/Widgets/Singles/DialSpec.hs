{-|
Module      : Monomer.Widgets.Singles.DialSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for Dial widget.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Singles.DialSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Data.Sequence (Seq(..))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Core.Themes.SampleThemes
import Monomer.Event
import Monomer.TestUtil
import Monomer.TestEventUtil
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.Dial

import qualified Monomer.Lens as L

data TestEvt
  = DialChanged Double
  | GotFocus Path
  | LostFocus Path
  deriving (Eq, Show)

newtype TestModel = TestModel {
  _tmDialVal :: Double
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''TestModel

spec :: Spec
spec = describe "Dial" $ do
  handleKeyboard
  handleMouseDrag
  handleMouseDragVal
  handleWheel
  handleWheelVal
  handleShiftFocus
  getSizeReq

handleKeyboard :: Spec
handleKeyboard = describe "handleKeyboard" $ do
  it "should press arrow up ten times and set the dial value to 20" $ do
    let steps = replicate 10 (evtK keyUp)
    model steps ^. dialVal `shouldBe` 20

  it "should press arrow up + shift ten times and set the dial value to 2" $ do
    let steps = replicate 10 (evtKS keyUp)
    model steps ^. dialVal `shouldBe` 2

  it "should press arrow up + ctrl four times and set the dial value to 80" $ do
    let steps = replicate 4 (evtKG keyUp)
    model steps ^. dialVal `shouldBe` 80

  it "should press arrow down ten times and set the dial value to -20" $ do
    let steps = replicate 10 (evtK keyDown)
    model steps ^. dialVal `shouldBe` (-20)

  it "should press arrow down + shift five times and set the dial value to 1" $ do
    let steps = replicate 5 (evtKS keyDown)
    model steps ^. dialVal `shouldBe` -1

  it "should press arrow up + ctrl one time and set the dial value to -20" $ do
    let steps = [evtKG keyDown]
    model steps ^. dialVal `shouldBe` (-20)

  where
    wenv = mockWenvEvtUnit (TestModel 0)
      & L.theme .~ darkTheme
    dialNode = dial dialVal (-100) 100
    model es = nodeHandleEventModel wenv es dialNode

handleMouseDrag :: Spec
handleMouseDrag = describe "handleMouseDrag" $ do
  it "should not change the value when dragging off bounds" $ do
    let selStart = Point 0 0
    let selEnd = Point 0 100
    let steps = evtDrag selStart selEnd
    model steps ^. dialVal `shouldBe` 0

  it "should not change the value when dragging horizontally" $ do
    let selStart = Point 320 240
    let selEnd = Point 500 240
    let steps = evtDrag selStart selEnd
    model steps ^. dialVal `shouldBe` 0

  it "should drag 100 pixels up and set the dial value to 20" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 140
    let steps = evtDrag selStart selEnd
    model steps ^. dialVal `shouldBe` 20

  it "should drag 500 pixels up and set the dial value 100" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 (-260)
    let steps = evtDrag selStart selEnd
    model steps ^. dialVal `shouldBe` 100

  it "should drag 1000 pixels up, but stay on 100" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 (-760)
    let steps = evtDrag selStart selEnd
    model steps ^. dialVal `shouldBe` 100

  where
    wenv = mockWenvEvtUnit (TestModel 0)
      & L.theme .~ darkTheme
    dialNode = dial dialVal (-100) 100
    model es = nodeHandleEventModel wenv es dialNode

handleMouseDragVal :: Spec
handleMouseDragVal = describe "handleMouseDragVal" $ do
  it "should not change the value when dragging off bounds" $ do
    let selStart = Point 0 0
    let selEnd = Point 0 100
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList []

  it "should not change the value when dragging horizontally" $ do
    let selStart = Point 320 240
    let selEnd = Point 500 240
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList []

  it "should drag 100 pixels up and set the dial value to 110" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 140
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList [DialChanged 110]

  it "should drag 490 pixels up and set the dial value 500" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 (-250)
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList [DialChanged 500]

  it "should drag 1000 pixels up, but stay on 500" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 (-760)
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList [DialChanged 500]

  it "should generate an event when focus is received" $
    evts [evtFocus] `shouldBe` Seq.singleton (GotFocus emptyPath)

  it "should generate an event when focus is lost" $
    evts [evtBlur] `shouldBe` Seq.singleton (LostFocus emptyPath)

  where
    wenv = mockWenv (TestModel 0)
      & L.theme .~ darkTheme
    dialNode = dialV_ 10 DialChanged (-500) 500 [dragRate 1, onFocus GotFocus, onBlur LostFocus]
    evts es = nodeHandleEventEvts wenv es dialNode

handleWheel :: Spec
handleWheel = describe "handleWheel" $ do
  it "should update the model when using the wheel" $ do
    let p = Point 320 240
    let steps = [WheelScroll p (Point 0 100) WheelNormal]
    model steps ^. dialVal `shouldBe` 300

  where
    wenv = mockWenvEvtUnit (TestModel 200)
      & L.theme .~ darkTheme
    dialNode = dial_ dialVal (-500) 500 [wheelRate 1]
    model es = nodeHandleEventModel wenv es dialNode

handleWheelVal :: Spec
handleWheelVal = describe "handleWheelVal" $ do
  it "should update the model when using the wheel" $ do
    let p = Point 320 240
    let steps = [WheelScroll p (Point 0 (-200)) WheelNormal]
    evts steps `shouldBe` Seq.singleton (DialChanged (-200))

  it "should generate IgnoreParentEvents when using the wheel" $ do
    let p = Point 320 240
    let steps = [WheelScroll p (Point 0 (-64)) WheelNormal]
    reqs steps `shouldSatisfy` (IgnoreParentEvents `elem`)

  where
    wenv = mockWenv (TestModel 0)
      & L.theme .~ darkTheme
    dialNode = dialV_ 0 DialChanged (-500) 500 [wheelRate 1]
    evts es = nodeHandleEventEvts wenv es dialNode
    reqs es = nodeHandleEventReqs wenv es dialNode

handleShiftFocus :: Spec
handleShiftFocus = describe "handleShiftFocus" $ do
  it "should set focus when clicked" $ do
    evts [evtPress p] `shouldBe` Seq.fromList [GotFocus $ Seq.fromList [0, 0, 0]]

  it "should not set focus when clicked with shift pressed" $ do
    evts [evtKS keyA, evtPress p] `shouldBe` Seq.empty

  where
    wenv = mockWenv (TestModel 0)
      & L.theme .~ darkTheme
    p = Point 25 50
    dialNode = hstack [
        vstack [
          dial_ dialVal (-500) 500 [wheelRate 1],
          dial_ dialVal (-500) 500 [wheelRate 1, onFocus GotFocus]
        ]
      ]
    evts es = nodeHandleEventEvts wenv es dialNode

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return width = Fixed 50" $
    sizeReqW `shouldBe` fixedSize 50

  it "should return height = Fixed 50" $
    sizeReqH `shouldBe` fixedSize 50

  where
    wenv = mockWenvEvtUnit (TestModel 0)
      & L.theme .~ darkTheme
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv (dial dialVal 0 100)
