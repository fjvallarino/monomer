{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.PersistSpec (spec) where

import Codec.Serialise
import Control.Lens ((&), (^.), (^?), (.~), (?~), (%~), non, _Just)
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Data.Maybe
import Data.Text (Text)
import GHC.Generics
import System.Directory
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.Graphics
import Monomer.TestEventUtil
import Monomer.TestUtil
import Monomer.Widgets.Composite
import Monomer.Widgets.Label
import Monomer.Widgets.Stack
import Monomer.Widgets.TextField
import Monomer.Widgets.Util.Widget

import qualified Monomer.Lens as L

data MainEvt
  = MainBtnClicked
  | ChildClicked
  deriving (Eq, Show)

data ChildEvt
  = ChildBtnClicked
  | ChildMessage String
  deriving (Eq, Show)

newtype MainModel = MainModel {
  _tmText1 :: Text
} deriving (Eq, Show, Generic, Serialise)

instance Default MainModel where
  def = MainModel {
    _tmText1 = ""
  }

makeLensesWith abbreviatedFields ''MainModel

setFontColorL = L.text . non def . L.fontColor

spec :: Spec
spec = describe "Persist" $ do
  saveSingle
  restoreSingle
  restoreContainer
  restoreComposite

saveSingle :: Spec
saveSingle = describe "saveSingle" $ do
  it "should have same info" $
    node ^. L.info `shouldBe` inst ^. L.info

  it "should have state saved" $
    inst ^. L.state `shouldSatisfy` isJust

  it "should successfully save a file" $ do
    writeFileSerialise "tree-test.ser" inst
    removeFile "tree-test.ser"
  where
    wenv = mockWenv ()
    node = label "Test label"
    inst = widgetSave (node ^. L.widget) wenv node
    rest = widgetRestore (node ^. L.widget) wenv inst node

restoreSingle :: Spec
restoreSingle = describe "restoreSingle" $ do
  it "should have same info" $ do
    oldInfo `shouldBe` rstInfo
    model2 ^. text1 `shouldBe` "Test restore"
  where
    wenv :: WidgetEnv MainModel ()
    wenv = mockWenv (MainModel "Test")
    node1 = textField text1
    (model2, oldInfo, rstInfo) = handleRestoredEvents wenv node1

restoreContainer :: Spec
restoreContainer = describe "restoreContainer" $ do
  it "should have same info" $ do
    oldInfo `shouldBe` rstInfo
    model2 ^. text1 `shouldBe` "Test restore"

  where
    wenv = mockWenv (MainModel "Test")
    node1 = vstack [
        textField text1
      ]
    (model2, oldInfo, rstInfo) = handleRestoredEvents wenv node1

restoreComposite :: Spec
restoreComposite = describe "restoreComposite" $ do
  it "should have same info" $ do
    oldInfo `shouldBe` rstInfo
    model2 ^. text1 `shouldBe` "Test restore"

  where
    wenv = mockWenv (MainModel "Test")
    handleEvent
      :: WidgetEnv MainModel MainEvt
      -> MainModel
      -> MainEvt
      -> [EventResponse MainModel MainEvt ()]
    handleEvent wenv model evt = []
    buildUI wenv model = vstack [
        textField text1
      ]
    node1 = composite "main" id Nothing buildUI handleEvent
    (model2, oldInfo, rstInfo) = handleRestoredEvents wenv node1

handleRestoredEvents wenv node1 = (model2, oldInfo, rstInfo) where
  oldNode = nodeHandleEventRoot wenv (replicate 4 (evtK keyRight)) node1
  newNode = node1 `style` [textColor red]
  inst1 = widgetSave (oldNode ^. L.widget) wenv oldNode
  inst2 = deserialise (serialise inst1)
  ((wenv2, evts2, node2), ctx) = nodeHandleRestore wenv inst2 newNode
  model2 = nodeHandleEventModelNoInit wenv2 [evtK keyTab, evtT " restore"] node2
  oldStyle = setStyleValue (oldNode ^. L.info . L.style) setFontColorL (?~) red
  oldInfo = oldNode ^. L.info
    & L.style .~ oldStyle
  rstInfo = node2 ^. L.info
    & L.widgetId .~ WidgetId 0 rootPath
    & L.path .~ rootPath
