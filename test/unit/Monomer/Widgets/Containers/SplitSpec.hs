{-|
Module      : Monomer.Widgets.Containers.SplitSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for Split widget.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Containers.SplitSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Data.Sequence (Seq(..))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestUtil
import Monomer.TestEventUtil
import Monomer.Widgets.Containers.Split
import Monomer.Widgets.Singles.Button

import qualified Monomer.Lens as L

data TestEvt
  = SliderChanged Double
  | Button1
  | Button2
  deriving (Eq, Show)

newtype TestModel = TestModel {
  _tmSliderPos :: Double
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''TestModel

spec :: Spec
spec = describe "Split" $ do
  handleEventMouseDragH
  handleEventMouseDragV
  getSizeReq

handleEventMouseDragH :: Spec
handleEventMouseDragH = describe "handleEventMouseDragH" $ do
  it "should drag left 100 pixels, moving the slider" $ do
    let selStart = Point 320 240
    let selEnd = Point 220 100
    let steps = evtDrag selStart selEnd
    model steps ^. sliderPos `shouldBe` (220 / 640)
    areas steps `shouldBe` Seq.fromList [Rect 0 0 219 480, Rect 222 0 418 480]

  it "should drag left 200 pixels, but move the slider only 120" $ do
    let selStart = Point 320 240
    let selEnd = Point 120 100
    let steps = evtDrag selStart selEnd
    model steps ^. sliderPos `shouldBe` (200 / 640)
    areas steps `shouldBe` Seq.fromList [Rect 0 0 199 480, Rect 202 0 438 480]

  it "should drag right 100 pixels, but move the slider only 80" $ do
    let selStart = Point 320 240
    let selEnd = Point 420 100
    let steps = evtDrag selStart selEnd
    model steps ^. sliderPos `shouldBe` (400 / 640)
    areas steps `shouldBe` Seq.fromList [Rect 0 0 398 480, Rect 401 0 239 480]

  it "should drag up 100 pixels, keeping the slider intact" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 140
    let steps = evtDrag selStart selEnd
    model steps ^. sliderPos `shouldBe` 0.5
    areas steps `shouldBe` Seq.fromList [Rect 0 0 318 480, Rect 322 0 318 480]

  it "should drag down 100 pixels, keeping the slider intact" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 340
    let steps = evtDrag selStart selEnd
    model steps ^. sliderPos `shouldBe` 0.5
    areas steps `shouldBe` Seq.fromList [Rect 0 0 318 480, Rect 322 0 318 480]

  where
    wenv = mockWenv (TestModel 0.5)
    btn1 = button "Text" Button1 `style` [rangeWidth 200 400]
    btn2 = button "Longer" Button2 `style` [expandWidth 60]
    splitNode = hsplit_ [splitHandlePos sliderPos] (btn1, btn2)
    model es = nodeHandleEventModel wenv es splitNode
    areas es = vp where
      root = nodeHandleEventRoot wenv es splitNode
      vp = fmap (roundRectUnits . _wniViewport . _wnInfo) (root ^. L.children)

handleEventMouseDragV :: Spec
handleEventMouseDragV = describe "handleEventMouseDragV" $ do
  it "should drag up 100 pixels, moving the slider" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 140
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList [SliderChanged 0.5, SliderChanged (140 / 480)]
    areas steps `shouldBe` Seq.fromList [Rect 0 0 640 139, Rect 0 142 640 338]

  it "should drag up 200 pixels, but move the slider only 100" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 40
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList [SliderChanged 0.5, SliderChanged (80 / 480)]
    areas steps `shouldBe` Seq.fromList [Rect 0 0 640 80, Rect 0 82 640 398]

  it "should drag down 100 pixels, but move the slider only 40" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 340
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList [SliderChanged 0.5, SliderChanged (280 / 480)]
    areas steps `shouldBe` Seq.fromList [Rect 0 0 640 278, Rect 0 281 640 199]

  it "should drag left 100 pixels, keeping the slider intact" $ do
    let selStart = Point 320 240
    let selEnd = Point 220 240
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList [SliderChanged 0.5]
    areas steps `shouldBe` Seq.fromList [Rect 0 0 640 238, Rect 0 242 640 238]

  it "should drag right 100 pixels, keeping the slider intact" $ do
    let selStart = Point 320 240
    let selEnd = Point 420 240
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList [SliderChanged 0.5]
    areas steps `shouldBe` Seq.fromList [Rect 0 0 640 238, Rect 0 242 640 238]

  where
    wenv = mockWenv (TestModel 0.5)
    -- Uses flexHeight otherwise buttons are fixed size and split will not move
    btn1 = button "Text" Button1 `style` [flexHeight 20]
    btn2 = button "Longer" Button2 `style` [rangeHeight 200 400]
    splitNode = vsplit_ [splitHandlePosV 0.5, onChange SliderChanged] (btn1, btn2)
    evts es = nodeHandleEventEvts wenv es splitNode
    areas es = vp where
      root = nodeHandleEventRoot wenv es splitNode
      vp = fmap (roundRectUnits . _wniViewport . _wnInfo) (root ^. L.children)

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return width = Range 5 125 0.01" $
    hsizeReqW `shouldBe` SizeReq 3 120 120 1

  it "should return height = Fixed 20" $
    hsizeReqH `shouldBe` fixedSize 20

  it "should return width = Expand 60 1" $
    vsizeReqW `shouldBe` expandSize 60 1

  it "should return height = Fixed 45" $
    vsizeReqH `shouldBe` fixedSize 43

  where
    wenv = mockWenv (TestModel 0)
    btn1 = button "Button" Button1 `style` [expandWidth 60]
    btn2 = button "Button" Button2 `style` [expandWidth 60]
    (hsizeReqW, hsizeReqH) = nodeGetSizeReq wenv (hsplit (btn1, btn2))
    (vsizeReqW, vsizeReqH) = nodeGetSizeReq wenv (vsplit (btn1, btn2))
