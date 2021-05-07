{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Singles.ColorPickerSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.Graphics
import Monomer.TestEventUtil
import Monomer.TestUtil
import Monomer.Widgets.Singles.ColorPicker

import qualified Monomer.Lens as L

data PickerEvent
  = ColorChanged Color
  | GotFocus Path
  | LostFocus Path
  deriving (Eq, Show)

newtype TestModel = TestModel {
  _tmColor :: Color
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''TestModel

spec :: Spec
spec = describe "ColorPicker" $ do
  handleEvent
  handleEventV
  getSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should update the model" $
    model (Point 306 10) ^. color `shouldBe` rgb 127 0 0

  it "should generate an event when focus is received" $
    events evtFocus `shouldBe` Seq.singleton (GotFocus emptyPath)

  it "should generate an event when focus is lost" $
    events evtBlur `shouldBe` Seq.singleton (LostFocus emptyPath)

  where
    wenv = mockWenv (TestModel (rgb 0 0 0))
    pckNode = colorPicker_ color [onFocus GotFocus, onBlur LostFocus]
    model p = nodeHandleEventModel wenv [evtRelease p] pckNode
    events evt = nodeHandleEventEvts wenv [evt] pckNode

handleEventV :: Spec
handleEventV = describe "handleEventV" $ do
  it "should generante a change event" $
    events (evtRelease (Point 453 30)) `shouldBe` Seq.singleton (ColorChanged (rgb 0 200 0))

  it "should generate an event when focus is received" $
    events evtFocus `shouldBe` Seq.singleton (GotFocus emptyPath)

  it "should generate an event when focus is lost" $
    events evtBlur `shouldBe` Seq.singleton (LostFocus emptyPath)

  where
    wenv = mockWenv (TestModel (rgb 0 0 0))
    pckNode = colorPickerV_ (rgb 0 0 0) ColorChanged [onFocus GotFocus, onBlur LostFocus]
    events evt = nodeHandleEventEvts wenv [evt] pckNode

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return width = Range 126 1126" $
    sizeReqW `shouldBe` rangeSize 126 1126 1

  it "should return height = Fixed 64 100" $
    sizeReqH `shouldBe` rangeSize 64 100 1

  where
    wenv = mockWenvEvtUnit (TestModel (rgb 0 0 0))
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv (colorPicker color)
