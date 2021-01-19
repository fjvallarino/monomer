{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.SplitSpec (spec) where

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
import Monomer.Widgets.Button
import Monomer.Widgets.Split

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
  updateSizeReq

handleEventMouseDragH :: Spec
handleEventMouseDragH = describe "handleEventMouseDragH" $ do
  it "should drag left 100 pixels, moving the slider" $ do
    let selStart = Point 320 240
    let selEnd = Point 220 100
    let steps = evtDrag selStart selEnd
    model steps ^. sliderPos `shouldBe` (220 / 640)
    areas steps `shouldBe` Seq.fromList [Rect 0 0 218 480, Rect 223 0 417 480]

  it "should drag left 200 pixels, but move the slider only 120" $ do
    let selStart = Point 320 240
    let selEnd = Point 120 100
    let steps = evtDrag selStart selEnd
    model steps ^. sliderPos `shouldBe` (200 / 640)
    areas steps `shouldBe` Seq.fromList [Rect 0 0 198 480, Rect 203 0 437 480]

  it "should drag right 100 pixels, but move the slider only 80" $ do
    let selStart = Point 320 240
    let selEnd = Point 420 100
    let steps = evtDrag selStart selEnd
    model steps ^. sliderPos `shouldBe` (400 / 640)
    areas steps `shouldBe` Seq.fromList [Rect 0 0 397 480, Rect 402 0 238 480]

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
    btn2 = button "Longer" Button2
    splitNode = hsplit_ (btn1, btn2) [splitHandlePos sliderPos]
    model es = nodeHandleEventModel wenv es splitNode
    areas es = ra where
      root = nodeHandleEventRoot wenv es splitNode
      ra = fmap (roundRectUnits . _wniRenderArea . _wnInfo) (root ^. L.children)

handleEventMouseDragV :: Spec
handleEventMouseDragV = describe "handleEventMouseDragV" $ do
  it "should drag up 100 pixels, moving the slider" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 140
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList [SliderChanged (140 / 480)]
    areas steps `shouldBe` Seq.fromList [Rect 0 0 640 139, Rect 0 144 640 336]

  it "should drag up 200 pixels, but move the slider only 100" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 40
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList [SliderChanged (80 / 480)]
    areas steps `shouldBe` Seq.fromList [Rect 0 0 640 79, Rect 0 84 640 396]

  it "should drag down 100 pixels, but move the slider only 40" $ do
    let selStart = Point 320 240
    let selEnd = Point 320 340
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Seq.fromList [SliderChanged (280 / 480)]
    areas steps `shouldBe` Seq.fromList [Rect 0 0 640 277, Rect 0 282 640 198]

  it "should drag left 100 pixels, keeping the slider intact" $ do
    let selStart = Point 320 240
    let selEnd = Point 220 240
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Empty
    areas steps `shouldBe` Seq.fromList [Rect 0 0 640 238, Rect 0 242 640 238]

  it "should drag right 100 pixels, keeping the slider intact" $ do
    let selStart = Point 320 240
    let selEnd = Point 420 240
    let steps = evtDrag selStart selEnd
    evts steps `shouldBe` Empty
    areas steps `shouldBe` Seq.fromList [Rect 0 0 640 238, Rect 0 242 640 238]

  where
    wenv = mockWenv (TestModel 0.5)
    -- Uses flexHeight otherwise buttons are fixed size and split will not move
    btn1 = button "Text" Button1 `style` [flexHeight 20]
    btn2 = button "Longer" Button2 `style` [rangeHeight 200 400]
    splitNode = vsplit_ (btn1, btn2) [splitHandlePosV 0.5, onChange SliderChanged]
    evts es = nodeHandleEventEvts wenv es splitNode
    areas es = ra where
      root = nodeHandleEventRoot wenv es splitNode
      ra = fmap (roundRectUnits . _wniRenderArea . _wnInfo) (root ^. L.children)

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  it "should return width = Range 5 125 0.01" $
    hsizeReqW `shouldBe` RangeSize 5 125 0.01

  it "should return height = Fixed 20" $
    hsizeReqH `shouldBe` FixedSize 20

  it "should return width = Flex 60 0.01" $
    vsizeReqW `shouldBe` FlexSize 60 0.01

  it "should return height = Fixed 45" $
    vsizeReqH `shouldBe` FixedSize 45

  where
    wenv = mockWenv (TestModel 0)
    btn1 = button "Button" Button1
    btn2 = button "Button" Button2
    (hsizeReqW, hsizeReqH) = nodeUpdateSizeReq wenv (hsplit (btn1, btn2))
    (vsizeReqW, vsizeReqH) = nodeUpdateSizeReq wenv (vsplit (btn1, btn2))
