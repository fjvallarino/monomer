{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.RadioSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Radio

import qualified Monomer.Lens as L

data Fruit
  = Apple
  | Orange
  | Banana
  deriving (Eq, Show)

data FruitEvt
  = FruitSel Fruit
  | GotFocus
  | LostFocus
  deriving (Eq, Show)

newtype TestModel = TestModel {
  _tmFruit :: Fruit
} deriving (Eq)

makeLensesWith abbreviatedFields ''TestModel

spec :: Spec
spec = describe "Radio" $ do
  handleEvent
  handleEventValue
  updateSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should not generate an event if clicked outside" $
    clickModel (Point 3000 3000) orangeInst ^. fruit `shouldBe` Apple

  it "should generate a user provided event when clicked" $
    clickModel (Point 320 240) orangeInst ^. fruit `shouldBe` Orange

  it "should generate a user provided event when Enter/Space is pressed" $
    keyModel keyReturn bananaInst ^. fruit `shouldBe` Banana

  it "should generate an event when focus is received" $
    events Focus orangeInst `shouldBe` Seq.singleton GotFocus

  it "should generate an event when focus is lost" $
    events Blur orangeInst `shouldBe` Seq.singleton LostFocus

  where
    wenv = mockWenv (TestModel Apple) & L.theme .~ darkTheme
    orangeInst = radio_ fruit Orange [onFocus GotFocus, onBlur LostFocus]
    bananaInst = radio fruit Banana
    clickModel p inst = instHandleEventModel wenv [Click p LeftBtn] inst
    keyModel key inst = instHandleEventModel wenv [KeyAction def key KeyPressed] inst
    events evt inst = instHandleEventEvts wenv [evt] inst

handleEventValue :: Spec
handleEventValue = describe "handleEventValue" $ do
  it "should not generate an event if clicked outside" $
    clickModel (Point 3000 3000) orangeInst `shouldBe` Seq.empty

  it "should generate a user provided event when clicked" $
    clickModel (Point 320 240) orangeInst `shouldBe` Seq.singleton (FruitSel Orange)

  it "should generate a user provided event when Enter/Space is pressed" $
    keyModel keyReturn bananaInst `shouldBe` Seq.singleton (FruitSel Banana)

  where
    wenv = mockWenv (TestModel Apple) & L.theme .~ darkTheme
    orangeInst = radioV Apple FruitSel Orange
    bananaInst = radioV Apple FruitSel Banana
    clickModel p inst = instHandleEventEvts wenv [Click p LeftBtn] inst
    keyModel key inst = instHandleEventEvts wenv [KeyAction def key KeyPressed] inst

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  it "should return width = Fixed 20" $
    sizeReqW `shouldBe` FixedSize 20

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` FixedSize 20

  where
    wenv = mockWenvEvtUnit (TestModel Apple) & L.theme .~ darkTheme
    (sizeReqW, sizeReqH) = instUpdateSizeReq wenv (radio fruit Apple)
