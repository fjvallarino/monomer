{-|
Module      : Monomer.Widgets.Singles.LabeledCheckboxSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for labeledCheckbox widget.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Singles.LabeledCheckboxSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Core.Themes.SampleThemes
import Monomer.Event
import Monomer.TestEventUtil
import Monomer.TestUtil
import Monomer.Widgets.Singles.LabeledCheckbox

import qualified Monomer.Lens as L

data TestEvt
  = BoolSel Bool
  | GotFocus Path
  | LostFocus Path
  deriving (Eq, Show)

newtype TestModel = TestModel {
  _tmTestBool :: Bool
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''TestModel

spec :: Spec
spec = describe "Labeled Checkbox" $ do
  handleEvent
  handleEventValue
  getSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should not update the model if not clicked" $
    clickModel (Point 3000 3000) ^. testBool `shouldBe` False

  it "should update the model when clicked on the label" $
    clickModel (Point 10 10) ^. testBool `shouldBe` True

  it "should not update the model when clicked on the spacer" $
    clickModel (Point 42 10) ^. testBool `shouldBe` False

  it "should update the model when clicked on the checkbox" $
    clickModel (Point 50 10) ^. testBool `shouldBe` True

  it "should update the model when Enter/Space is pressed" $
    keyModel keyReturn ^. testBool `shouldBe` True

  it "should generate an event when focus is received" $
    events evtFocus `shouldBe` Seq.singleton (GotFocus emptyPath)

  it "should generate an event when focus is lost" $
    events evtBlur `shouldBe` Seq.singleton (LostFocus emptyPath)

  where
    wenv = mockWenv (TestModel False)
    chkNode = labeledCheckbox_ "Test" testBool [onFocus GotFocus, onBlur LostFocus]
    clickModel p = nodeHandleEventModel wenv [evtClick p] chkNode
    keyModel key = nodeHandleEventModel wenv [KeyAction def key KeyPressed] chkNode
    events evt = nodeHandleEventEvts wenv [evt] chkNode

handleEventValue :: Spec
handleEventValue = describe "handleEventValue" $ do
  it "should not generate an event if clicked outside" $
    clickModel (Point 3000 3000) chkNode `shouldBe` Seq.empty

  it "should generate a user provided event when clicked on the label" $
    clickModel (Point 10 10) chkNode `shouldBe` Seq.singleton (BoolSel True)

  it "should not generate a user provided event when clicked on the spacer" $
    clickModel (Point 42 10) chkNode `shouldBe` Seq.empty

  it "should generate a user provided event when clicked on the checkbox" $
    clickModel (Point 50 10) chkNode `shouldBe` Seq.singleton (BoolSel True)

  it "should generate a user provided event when clicked (True -> False)" $
    clickModel (Point 10 10) chkNodeT `shouldBe` Seq.singleton (BoolSel False)

  it "should generate a user provided event when Enter/Space is pressed" $
    keyModel keyReturn chkNode `shouldBe` Seq.singleton (BoolSel True)

  where
    wenv = mockWenv (TestModel False)
    chkNode = labeledCheckboxV "Test" False BoolSel
    chkNodeT = labeledCheckboxV "Test" True BoolSel
    clickModel p node = nodeHandleEventEvts wenv [evtClick p] node
    keyModel key node = nodeHandleEventEvts wenv [KeyAction def key KeyPressed] node

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return width = Fixed 71" $
    sizeReqW `shouldBe` fixedSize 71

  it "should return height = Fixed 26" $
    sizeReqH `shouldBe` fixedSize 26

  where
    wenv = mockWenvEvtUnit (TestModel False)
      & L.theme .~ darkTheme
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv (labeledCheckbox "Test" testBool)
