{-|
Module      : Monomer.Widgets.Singles.SliderSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for Slider widget.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Singles.SliderSpec (spec) where

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
import Monomer.Widgets.Singles.Slider

import qualified Monomer.Lens as L

data TestEvt
  = SliderChanged Double
  | GotFocus Path
  | LostFocus Path
  deriving (Eq, Show)

newtype TestModel = TestModel {
  _tmSliderVal :: Double
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''TestModel

spec :: Spec
spec = describe "Slider" $ do
  handleKeyboardH
  handleKeyboardV
  handleMouseDragH
  handleMouseDragV
  handleMouseDragThumb
  handleMouseDragValH
  handleMouseDragValV
  handleWheelH
  handleWheelValV
  handleShiftFocus
  getSizeReqH
  getSizeReqV
  getSizeReqThumb

handleKeyboardH :: Spec
handleKeyboardH = describe "handleKeyboardH" $ do
  it "should not change the value when using vertical arrows" $ do
    let steps = [evtK keyUp, evtK keyDown, evtK keyDown]
    model steps ^. sliderVal `shouldBe` 0

  it "should press arrow right ten times and set the slider value to 20" $ do
    let steps = replicate 10 (evtK keyRight)
    model steps ^. sliderVal `shouldBe` 20

  it "should press arrow right + shift ten times and set the slider value to 2" $ do
    let steps = replicate 10 (evtKS keyRight)
    model steps ^. sliderVal `shouldBe` 2

  it "should press arrow right + ctrl four times and set the slider value to 80" $ do
    let steps = replicate 4 (evtKG keyRight)
    model steps ^. sliderVal `shouldBe` 80

  it "should press arrow left ten times and set the slider value to -20" $ do
    let steps = replicate 10 (evtK keyLeft)
    model steps ^. sliderVal `shouldBe` (-20)

  it "should press arrow left + shift five times and set the slider value to 1" $ do
    let steps = replicate 5 (evtKS keyLeft)
    model steps ^. sliderVal `shouldBe` -1

  it "should press arrow right + ctrl one time and set the slider value to -20" $ do
    let steps = [evtKG keyLeft]
    model steps ^. sliderVal `shouldBe` (-20)

  where
    wenv = mockWenvEvtUnit (TestModel 0)
      & L.theme .~ darkTheme
    sliderNode = hslider sliderVal (-100) 100
    model es = nodeHandleEventModel wenv es sliderNode

handleKeyboardV :: Spec
handleKeyboardV = describe "handleKeyboardV" $ do
  it "should not change the value when using horizontal arrows" $ do
    let steps = [evtK keyLeft, evtK keyRight, evtK keyRight]
    model steps ^. sliderVal `shouldBe` 0

  it "should press arrow up ten times and set the slider value to 20" $ do
    let steps = replicate 10 (evtK keyUp)
    model steps ^. sliderVal `shouldBe` 20

  it "should press arrow up + shift ten times and set the slider value to 2" $ do
    let steps = replicate 10 (evtKS keyUp)
    model steps ^. sliderVal `shouldBe` 2

  it "should press arrow up + ctrl four times and set the slider value to 80" $ do
    let steps = replicate 4 (evtKG keyUp)
    model steps ^. sliderVal `shouldBe` 80

  it "should press arrow down ten times and set the slider value to -20" $ do
    let steps = replicate 10 (evtK keyDown)
    model steps ^. sliderVal `shouldBe` (-20)

  it "should press arrow down + shift five times and set the slider value to 1" $ do
    let steps = replicate 5 (evtKS keyDown)
    model steps ^. sliderVal `shouldBe` -1

  it "should press arrow up + ctrl one time and set the slider value to -20" $ do
    let steps = [evtKG keyDown]
    model steps ^. sliderVal `shouldBe` (-20)

  where
    wenv = mockWenvEvtUnit (TestModel 0)
      & L.theme .~ darkTheme
    sliderNode = vslider sliderVal (-100) 100
    model es = nodeHandleEventModel wenv es sliderNode

handleMouseDragH :: Spec
handleMouseDragH = describe "handleMouseDragH" $ do
  it "should not change the value when dragging vertically" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 120
    let steps = evtDrag selStart selEnd
    model steps ^. sliderVal `shouldBe` 0

  it "should drag 160 pixels right and set the slider value to 50" $ do
    let selStart = Point 320 240
    let selEnd = Point 480 240
    let steps = evtDrag selStart selEnd
    model steps ^. sliderVal `shouldBe` 50

  it "should drag 320 pixels right and set the slider value 100" $ do
    let selStart = Point 320 240
    let selEnd = Point 640 240
    let steps = evtDrag selStart selEnd
    model steps ^. sliderVal `shouldBe` 100

  it "should drag 1000 pixels right, but stay on 100" $ do
    let selStart = Point 320 240
    let selEnd = Point 1320 240
    let steps = evtDrag selStart selEnd
    model steps ^. sliderVal `shouldBe` 100

  where
    wenv = mockWenvEvtUnit (TestModel 0)
      & L.theme .~ darkTheme
    sliderNode = hslider sliderVal (-100) 100
    model es = nodeHandleEventModel wenv es sliderNode

handleMouseDragV :: Spec
handleMouseDragV = describe "handleMouseDragV" $ do
  it "should not change the value when dragging horizontally" $ do
    let selStart = Point 320 240
    let selEnd = Point 500 240
    let steps = evtDrag selStart selEnd
    model steps ^. sliderVal `shouldBe` 0

  it "should drag 100 pixels up and set the slider value to 50" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 120
    let steps = evtDrag selStart selEnd
    model steps ^. sliderVal `shouldBe` 50

  it "should drag 500 pixels up and set the slider value 100" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 (-260)
    let steps = evtDrag selStart selEnd
    model steps ^. sliderVal `shouldBe` 100

  it "should drag 1000 pixels up, but stay on 100" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 (-760)
    let steps = evtDrag selStart selEnd
    model steps ^. sliderVal `shouldBe` 100

  where
    wenv = mockWenvEvtUnit (TestModel 0)
      & L.theme .~ darkTheme
    sliderNode = vslider sliderVal (-100) 100
    model es = nodeHandleEventModel wenv es sliderNode

handleMouseDragThumb :: Spec
handleMouseDragThumb = describe "handleMouseDragThumb" $ do
  it "should not change the value when dragging horizontally" $ do
    let selStart = Point 320 240
    let selEnd = Point 500 240
    let steps = evtDrag selStart selEnd
    model steps ^. sliderVal `shouldBe` 0

  it "should drag 100 pixels up and set the slider value to 50" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 120
    let steps = evtDrag selStart selEnd
    model steps ^. sliderVal `shouldBe` 50

  it "should drag 500 pixels up and set the slider value 100" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 (-260)
    let steps = evtDrag selStart selEnd
    model steps ^. sliderVal `shouldBe` 100

  it "should drag 1000 pixels up, but stay on 100" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 (-760)
    let steps = evtDrag selStart selEnd
    model steps ^. sliderVal `shouldBe` 100

  where
    wenv = mockWenvEvtUnit (TestModel 0)
      & L.theme .~ darkTheme
    sliderNode = vslider_ sliderVal (-100) 100 [thumbVisible]
    model es = nodeHandleEventModel wenv es sliderNode

handleMouseDragValH :: Spec
handleMouseDragValH = describe "handleMouseDragValH" $ do
  it "should not change the value when dragging vertically" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 0
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList []

  it "should drag 160 pixels left and set the slider value to -250" $ do
    let selStart = Point 320 240
    let selEnd = Point 160 240
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList [SliderChanged (-250.0)]

  it "should drag right to 640 and set the slider value 500" $ do
    let selStart = Point 320 240
    let selEnd = Point 640 240
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList [SliderChanged 500]

  it "should drag 1000 pixels right, but stay on 500" $ do
    let selStart = Point 320 240
    let selEnd = Point 1000 240
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList [SliderChanged 500]

  it "should click 160 pixels left and set the slider to -250" $ do
    let point = Point 160 240
    let steps = [evtRelease point]
    evts steps `shouldBe` Seq.fromList [SliderChanged (-250)]

  it "should generate an event when focus is received" $
    evts [evtFocus] `shouldBe` Seq.singleton (GotFocus emptyPath)

  it "should generate an event when focus is lost" $
    evts [evtBlur] `shouldBe` Seq.singleton (LostFocus emptyPath)

  where
    wenv = mockWenv (TestModel 500)
      & L.theme .~ darkTheme
    sliderNode = hsliderV_ 0 SliderChanged (-500) 500 [dragRate 1, onFocus GotFocus, onBlur LostFocus]
    evts es = nodeHandleEventEvts wenv es sliderNode

handleMouseDragValV :: Spec
handleMouseDragValV = describe "handleMouseDragValV" $ do
  it "should not change the value when dragging horizontally" $ do
    let selStart = Point 320 240
    let selEnd = Point 500 240
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList []

  it "should drag 100 pixels down and set the slider value to -250" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 360
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList [SliderChanged (-250.0)]

  it "should drag up to zero and set the slider value 500" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 0
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList [SliderChanged 500]

  it "should drag 1000 pixels up, but stay on 500" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 (-760)
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList [SliderChanged 500]

  it "should click 120 pixels down and set the slider to -250" $ do
    let point = Point 320 360
    let steps = [evtRelease point]
    evts steps `shouldBe` Seq.fromList [SliderChanged (-250)]

  it "should generate an event when focus is received" $
    evts [evtFocus] `shouldBe` Seq.singleton (GotFocus emptyPath)

  it "should generate an event when focus is lost" $
    evts [evtBlur] `shouldBe` Seq.singleton (LostFocus emptyPath)

  where
    wenv = mockWenv (TestModel 500)
      & L.theme .~ darkTheme
    sliderNode = vsliderV_ 0 SliderChanged (-500) 500 [dragRate 1, onFocus GotFocus, onBlur LostFocus]
    evts es = nodeHandleEventEvts wenv es sliderNode

handleWheelH :: Spec
handleWheelH = describe "handleWheelH" $ do
  it "should update the model when using the wheel" $ do
    let p = Point 100 10
    let steps = [WheelScroll p (Point 0 100) WheelNormal]
    model steps ^. sliderVal `shouldBe` 300

  where
    wenv = mockWenvEvtUnit (TestModel 200)
      & L.theme .~ darkTheme
    sliderNode = hslider_ sliderVal (-500) 500 [wheelRate 1]
    model es = nodeHandleEventModel wenv es sliderNode

handleWheelValV :: Spec
handleWheelValV = describe "handleWheelValV" $ do
  it "should update the model when using the wheel" $ do
    let p = Point 100 10
    let steps = [WheelScroll p (Point 0 (-200)) WheelNormal]
    evts steps `shouldBe` Seq.singleton (SliderChanged (-200))

  where
    wenv = mockWenv (TestModel 0)
      & L.theme .~ darkTheme
    sliderNode = vsliderV_ 0 SliderChanged (-500) 500 [wheelRate 1]
    evts es = nodeHandleEventEvts wenv es sliderNode

handleShiftFocus :: Spec
handleShiftFocus = describe "handleShiftFocus" $ do
  it "should set focus when clicked" $ do
    evts [evtPress p] `shouldBe` Seq.fromList [GotFocus $ Seq.fromList [0, 0]]

  it "should not set focus when clicked with shift pressed" $ do
    evts [evtKS keyA, evtPress p] `shouldBe` Seq.empty

  where
    wenv = mockWenv (TestModel 0)
      & L.theme .~ darkTheme
    p = Point 100 30
    sliderNode = vstack [
        hslider_ sliderVal (-500) 500 [wheelRate 1] `styleBasic` [height 20],
        hslider_ sliderVal (-500) 500 [wheelRate 1, onFocus GotFocus] `styleBasic` [height 20]
      ]
    evts es = nodeHandleEventEvts wenv es sliderNode

getSizeReqH :: Spec
getSizeReqH = describe "getSizeReqH" $ do
  it "should return width = Expand 1000 1" $
    sizeReqW `shouldBe` expandSize 1000 1

  it "should return height = Fixed 10" $
    sizeReqH `shouldBe` fixedSize 10

  where
    wenv = mockWenvEvtUnit (TestModel 0)
      & L.theme .~ darkTheme
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv (hslider sliderVal 0 100)

getSizeReqV :: Spec
getSizeReqV = describe "getSizeReqV" $ do
  it "should return width = Fixed 10" $
    sizeReqW `shouldBe` fixedSize 10

  it "should return height = Expand 1000 1" $
    sizeReqH `shouldBe` expandSize 1000 1

  where
    wenv = mockWenvEvtUnit (TestModel 0)
      & L.theme .~ darkTheme
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv (vslider sliderVal 0 100)

getSizeReqThumb :: Spec
getSizeReqThumb = describe "getSizeReqThumb" $ do
  it "should return width = Fixed 10" $
    sizeReqW `shouldBe` fixedSize 10

  it "should return height = Expand 1000 1" $
    sizeReqH `shouldBe` expandSize 1000 1

  where
    wenv = mockWenvEvtUnit (TestModel 0)
      & L.theme .~ darkTheme
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv (vslider_ sliderVal 0 100 [thumbVisible])
