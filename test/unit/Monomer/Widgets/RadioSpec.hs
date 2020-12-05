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
import Monomer.Core.Combinators
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
  it "should not update the model if not clicked" $
    clickModel (Point 3000 3000) orangeNode ^. fruit `shouldBe` Apple

  it "should update the model when clicked" $
    clickModel (Point 320 240) orangeNode ^. fruit `shouldBe` Orange

  it "should update the model when Enter/Space is pressed" $
    keyModel keyReturn bananaNode ^. fruit `shouldBe` Banana

  it "should generate an event when focus is received" $
    events Focus orangeNode `shouldBe` [GotFocus]

  it "should generate an event when focus is lost" $
    events Blur orangeNode `shouldBe` [LostFocus]

  where
    wenv = mockWenv (TestModel Apple) & L.theme .~ darkTheme
    orangeNode = radio_ fruit Orange [onFocus GotFocus, onBlur LostFocus]
    bananaNode = radio fruit Banana
    clickModel p node = nodeHandleEventModel wenv [Click p LeftBtn] node
    keyModel key node = nodeHandleEventModel wenv [KeyAction def key KeyPressed] node
    events evt node = nodeHandleEventEvts wenv [evt] node

handleEventValue :: Spec
handleEventValue = describe "handleEventValue" $ do
  it "should not generate an event if clicked outside" $
    clickModel (Point 3000 3000) orangeNode `shouldBe` []

  it "should generate a user provided event when clicked" $
    clickModel (Point 320 240) orangeNode `shouldBe` [FruitSel Orange]

  it "should generate a user provided event when Enter/Space is pressed" $
    keyModel keyReturn bananaNode `shouldBe` [FruitSel Banana]

  where
    wenv = mockWenv (TestModel Apple) & L.theme .~ darkTheme
    orangeNode = radioV Apple FruitSel Orange
    bananaNode = radioV Apple FruitSel Banana
    clickModel p node = nodeHandleEventEvts wenv [Click p LeftBtn] node
    keyModel key node = nodeHandleEventEvts wenv [KeyAction def key KeyPressed] node

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  it "should return width = Fixed 20" $
    sizeReqW `shouldBe` FixedSize 20

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` FixedSize 20

  where
    wenv = mockWenvEvtUnit (TestModel Apple) & L.theme .~ darkTheme
    (sizeReqW, sizeReqH) = nodeUpdateSizeReq wenv (radio fruit Apple)
