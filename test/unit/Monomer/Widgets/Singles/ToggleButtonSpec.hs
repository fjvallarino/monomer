{-|
Module      : Monomer.Widgets.Singles.ToggleButtonSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for ToggleButton widget.

This module is intentionally copied from ToggleButtonSpec, since the behavior
should be the same.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Singles.ToggleButtonSpec (spec) where

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
import Monomer.Widgets.Singles.ToggleButton

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
spec = describe "ToggleButton" $ do
  handleEvent
  handleEventValue
  getSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should not update the model if not clicked" $
    clickModel (Point 3000 3000) ^. testBool `shouldBe` False

  it "should update the model when clicked" $
    clickModel (Point 100 100) ^. testBool `shouldBe` True

  it "should update the model when Enter/Space is pressed" $
    keyModel keyReturn ^. testBool `shouldBe` True

  it "should generate an event when focus is received" $
    events evtFocus `shouldBe` Seq.singleton (GotFocus emptyPath)

  it "should generate an event when focus is lost" $
    events evtBlur `shouldBe` Seq.singleton (LostFocus emptyPath)

  where
    wenv = mockWenv (TestModel False)
    chkNode = toggleButton_ "Toggle" testBool [onFocus GotFocus, onBlur LostFocus]
    clickModel p = nodeHandleEventModel wenv [evtClick p] chkNode
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
    chkNode = toggleButtonV "Toggle" False BoolSel
    chkNodeT = toggleButtonV "Toggle" True BoolSel
    clickModel p node = nodeHandleEventEvts wenv [evtClick p] node
    keyModel key node = nodeHandleEventEvts wenv [KeyAction def key KeyPressed] node

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return width = Fixed 60" $
    sizeReqW `shouldBe` fixedSize 60

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` fixedSize 20

  where
    wenv = mockWenvEvtUnit (TestModel False)
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv (toggleButton "Toggle" testBool)
