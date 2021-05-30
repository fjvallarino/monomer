{-|
Module      : Monomer.Widgets.Containers.KeystrokeSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for Keystroke widget.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Containers.KeystrokeSpec (spec) where

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
import Monomer.TestEventUtil
import Monomer.Widgets.Containers.Keystroke
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.TextField

import qualified Monomer.Lens as L

data TestEvt
  = CtrlA
  | CtrlSpace
  | CtrlShiftSpace
  | MultiKey Int
  | FunctionKey Int
  deriving (Eq, Show)

newtype TestModel = TestModel {
  _tmTextValue :: Text
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''TestModel

spec :: Spec
spec = describe "Keystroke" $ do
  handleEvent
  getSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should not generate events" $ do
    events [] `shouldBe` Seq.empty

  it "should generate an event when Ctrl-Space is pressed" $ do
    events [evtKC keySpace] `shouldBe` Seq.fromList [CtrlSpace]

  it "should generate an event when Ctrl-Shift-Space is pressed" $ do
    events [evtKCS keySpace] `shouldBe` Seq.fromList [CtrlShiftSpace]

  it "should generate events when function keys are pressed" $ do
    events [evtK keyF1] `shouldBe` Seq.fromList [FunctionKey 1]
    events [evtKC keyF3] `shouldBe` Seq.fromList [FunctionKey 3]
    events [evtKG keyF7] `shouldBe` Seq.fromList [FunctionKey 7]
    events [evtKS keyF12] `shouldBe` Seq.fromList [FunctionKey 12]

  it "should only generate events when the exact keys are pressed" $ do
    events [evtKC keyA, evtKC keyB] `shouldBe` Seq.fromList []
    events [evtKC keyA, evtKC keyB, evtKC keyD, evtKC keyC] `shouldBe` Seq.fromList []
    events [evtKC keyA, evtKC keyB, evtKC keyC] `shouldBe` Seq.fromList [MultiKey 1]
    events [
      evtKC keyA, evtKC keyB, evtKC keyC,
      evtKC keyD, evtKC keyE] `shouldBe` Seq.fromList [MultiKey 1]
    events [
      evtKC keyA, evtKC keyB, evtKC keyC,
      evtRKC keyA, evtRKC keyB, evtRKC keyC,
      evtKC keyD, evtKC keyE] `shouldBe` Seq.fromList [MultiKey 1, MultiKey 2]

  it "should not ignore children events if not explicitly requested" $ do
    events1 [evtKG keyA, evtT "d"] `shouldBe` Seq.fromList [CtrlA]
    model1 [evtKG keyA, evtT "d"] ^. textValue `shouldBe` "d"

  it "should ignore children events if requested" $ do
    events2 [evtKG keyA, evtT "d"] `shouldBe` Seq.fromList [CtrlA]
    model2 [evtKG keyA, evtT "d"] ^. textValue `shouldBe` "abcd"

  where
    wenv = mockWenv (TestModel "")
    bindings = [
        ("C-Space", CtrlSpace),
        ("C-S-Space", CtrlShiftSpace),
        ("C-a-b-c", MultiKey 1),
        ("C-d-e", MultiKey 2),
        ("F1", FunctionKey 1),
        ("Ctrl-F3", FunctionKey 3),
        ("Cmd-F7", FunctionKey 7),
        ("S-F12", FunctionKey 12)
      ]
    kstNode = keystroke bindings (textField textValue)
    events es = nodeHandleEventEvts wenv es kstNode
    wenv2 = mockWenv (TestModel "abc")
    kstModel1 = keystroke [("C-a", CtrlA)] (textField textValue)
    kstModel2 = keystroke_ [("C-a", CtrlA)] [ignoreChildrenEvts] (textField textValue)
    model1 es = nodeHandleEventModel wenv2 es kstModel1
    model2 es = nodeHandleEventModel wenv2 es kstModel2
    events1 es = nodeHandleEventEvts wenv2 es kstModel1
    events2 es = nodeHandleEventEvts wenv2 es kstModel2

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return same reqW as child node" $
    kSizeReqW `shouldBe` lSizeReqW

  it "should return same reqH as child node" $
    kSizeReqH `shouldBe` lSizeReqH

  where
    wenv = mockWenvEvtUnit (TestModel "Test value")
    lblNode = label "Test label"
    (lSizeReqW, lSizeReqH) = nodeGetSizeReq wenv lblNode
    (kSizeReqW, kSizeReqH) = nodeGetSizeReq wenv (keystroke [] lblNode)
