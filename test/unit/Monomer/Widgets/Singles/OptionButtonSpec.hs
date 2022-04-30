{-|
Module      : Monomer.Widgets.Singles.OptionButtonSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for OptionButton widget.

This module is intentionally copied from RadioSpec, since the behavior should be
the same.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Singles.OptionButtonSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestEventUtil
import Monomer.TestUtil
import Monomer.Widgets.Singles.OptionButton

import qualified Monomer.Lens as L

data Fruit
  = Apple
  | Orange
  | Banana
  deriving (Eq, Show)

data FruitEvt
  = FruitClicked
  | FruitSel Fruit
  | GotFocus Path
  | LostFocus Path
  deriving (Eq, Show)

newtype TestModel = TestModel {
  _tmFruit :: Fruit
} deriving (Eq)

makeLensesWith abbreviatedFields ''TestModel

spec :: Spec
spec = describe "OptionButton" $ do
  handleEvent
  handleEventValue
  getSizeReq
  testWidgetType

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should not update the model if not clicked" $
    clickModel (Point 3000 3000) orangeNode ^. fruit `shouldBe` Apple

  it "should update the model when clicked" $
    clickModel (Point 320 240) orangeNode ^. fruit `shouldBe` Orange

  it "should update the model when Enter/Space is pressed" $
    keyModel keyReturn bananaNode ^. fruit `shouldBe` Banana

  it "should generate an event when focus is received" $
    events evtFocus orangeNode `shouldBe` Seq.singleton (GotFocus emptyPath)

  it "should generate an event when focus is lost" $
    events evtBlur orangeNode `shouldBe` Seq.singleton (LostFocus emptyPath)

  it "should generate multiple click events when clicked, but a single onChange because the value did not change" $ do
    let evt = evtClick (Point 320 240)
    let events es = nodeHandleEventEvts wenv es bananaClickNode

    events [evt] `shouldBe` Seq.fromList [FruitClicked, FruitSel Banana]
    events [evt, evt] `shouldBe` Seq.fromList [FruitClicked, FruitSel Banana, FruitClicked]
    events [evt, evt, evt] `shouldBe` Seq.fromList [FruitClicked, FruitSel Banana, FruitClicked, FruitClicked]

  where
    wenv = mockWenv (TestModel Apple)
    orangeNode = optionButton_ "Orange" Orange fruit [onFocus GotFocus, onBlur LostFocus]
    bananaNode :: WidgetNode TestModel FruitEvt
    bananaNode = optionButton "Banana" Banana fruit
    bananaClickNode = optionButton_ "Banana" Banana fruit [onClick FruitClicked, onChange FruitSel]

    clickModel p node = nodeHandleEventModel wenv [evtClick p] node
    keyModel key node = nodeHandleEventModel wenv [KeyAction def key KeyPressed] node
    events evt node = nodeHandleEventEvts wenv [evt] node

handleEventValue :: Spec
handleEventValue = describe "handleEventValue" $ do
  it "should not generate an event if clicked outside" $
    clickModel (Point 3000 3000) orangeNode `shouldBe` Seq.empty

  it "should generate a user provided event when clicked" $
    clickModel (Point 320 240) orangeNode `shouldBe` Seq.singleton (FruitSel Orange)

  it "should generate a user provided event when Enter/Space is pressed" $
    keyModel keyReturn bananaNode `shouldBe` Seq.singleton (FruitSel Banana)

  where
    wenv = mockWenv (TestModel Apple)
    orangeNode = optionButtonV "Orange" Orange Apple FruitSel
    bananaNode = optionButtonV "Banana" Banana Apple FruitSel
    clickModel p node = nodeHandleEventEvts wenv [evtClick p] node
    keyModel key node = nodeHandleEventEvts wenv [KeyAction def key KeyPressed] node

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return width = Fixed 50" $
    sizeReqW `shouldBe` fixedSize 50

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` fixedSize 20

  where
    wenv = mockWenvEvtUnit (TestModel Apple)
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv (optionButton "Apple" Apple fruit)

testWidgetType :: Spec
testWidgetType = describe "testWidgetType" $ do
  it "should set the correct widgetType" $
    node ^. L.info . L.widgetType `shouldBe` "optionButton-Fruit"

  where
    node :: WidgetNode TestModel FruitEvt
    node = optionButton "Test" Apple fruit
