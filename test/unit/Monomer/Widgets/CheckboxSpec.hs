{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.CheckboxSpec (spec) where

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
import Monomer.Widgets.Checkbox

import qualified Monomer.Lens as L

data TestEvt
  = BoolSel Bool
  | GotFocus
  | LostFocus
  deriving (Eq, Show)

newtype TestModel = TestModel {
  _tmTestBool :: Bool
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''TestModel

spec :: Spec
spec = describe "Checkbox" $ do
  handleEvent
  handleEventValue
  updateSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should not update the model if not clicked" $
    clickModel (Point 3000 3000) ^. testBool `shouldBe` False

  it "should update the model when clicked" $
    clickModel (Point 100 100) ^. testBool `shouldBe` True

  it "should update the model when Enter/Space is pressed" $
    keyModel keyReturn ^. testBool `shouldBe` True

  it "should generate an event when focus is received" $
    events Focus `shouldBe` Seq.singleton GotFocus

  it "should generate an event when focus is lost" $
    events Blur `shouldBe` Seq.singleton LostFocus

  where
    wenv = mockWenv (TestModel False)
    chkNode = checkbox_ testBool [onFocus GotFocus, onBlur LostFocus]
    clickModel p = nodeHandleEventModel wenv [Click p LeftBtn] chkNode
    keyModel key = nodeHandleEventModel wenv [KeyAction def key KeyPressed] chkNode
    events evt = nodeHandleEventEvts wenv [evt] chkNode

handleEventValue :: Spec
handleEventValue = describe "handleEventValue" $ do
  it "should not generate an event if clicked outside" $
    clickModel (Point 3000 3000) chkNode `shouldBe` Seq.empty

  it "should generate a user provided event when clicked" $
    clickModel (Point 100 100) chkNode `shouldBe` Seq.singleton (BoolSel True)

  it "should generate a user provided event when clicked (True -> False)" $
    clickModel (Point 100 100) chkNodeT `shouldBe` Seq.singleton (BoolSel False)

  it "should generate a user provided event when Enter/Space is pressed" $
    keyModel keyReturn chkNode `shouldBe` Seq.singleton (BoolSel True)

  where
    wenv = mockWenv (TestModel False)
    chkNode = checkboxV False BoolSel
    chkNodeT = checkboxV True BoolSel
    clickModel p node = nodeHandleEventEvts wenv [Click p LeftBtn] node
    keyModel key node = nodeHandleEventEvts wenv [KeyAction def key KeyPressed] node

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  it "should return width = Fixed 20" $
    sizeReqW `shouldBe` FixedSize 20

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` FixedSize 20

  where
    wenv = mockWenvEvtUnit (TestModel False)
      & L.theme .~ darkTheme
    (sizeReqW, sizeReqH) = nodeUpdateSizeReq wenv (checkbox testBool)
