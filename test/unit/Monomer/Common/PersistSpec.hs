{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Common.PersistSpec (spec) where

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
import Monomer.Graphics.ColorTable
import Monomer.TestEventUtil
import Monomer.TestUtil
import Monomer.Widgets.Composite
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.TextField
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

data MainModel = MainModel {
  _tmText1 :: Text,
  _tmCount1 :: Int
} deriving (Eq, Show, Generic, Serialise)

instance WidgetModel MainModel where
  modelToByteString = serialise
  byteStringToModel = bsToSerialiseModel

makeLensesWith abbreviatedFields ''MainModel

setFontColorL = L.text . non def . L.fontColor

spec :: Spec
spec = describe "Persist" $ do
  widgetModel
  saveSingle
  restoreSingle
  restoreContainer
  restoreComposite

widgetModel :: Spec
widgetModel = describe "widgetModel" $ do
  it "should return the same value" $ do
    byteStringToModel textBS `shouldBe` Right ("Text" :: Text)
    byteStringToModel maybeBS `shouldBe` Right (Just 10 :: Maybe Rational)
    byteStringToModel listBS `shouldBe` Right ([1, 2, 3] :: [Int])
    byteStringToModel rightBS `shouldBe` Right (Right 20 :: Either Int Int)
    byteStringToModel leftBS `shouldBe` Right (Left 30 :: Either Int Int)
  where
    textBS = modelToByteString ("Text" :: Text)
    maybeBS = modelToByteString (Just 10 :: Maybe Rational)
    listBS = modelToByteString ([1, 2, 3] :: [Int])
    rightBS = modelToByteString (Right 20 :: Either Int Int)
    leftBS = modelToByteString (Left 30 :: Either Int Int)

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
    model2 ^. count1 `shouldBe` 0

  where
    wenv :: WidgetEnv MainModel ()
    wenv = mockWenv (MainModel "Test" 10)
    node1 = textField text1
    (model2, oldInfo, rstInfo) = handleRestoredEvents wenv node1

restoreContainer :: Spec
restoreContainer = describe "restoreContainer" $ do
  it "should have same info" $ do
    oldInfo `shouldBe` rstInfo
    model2 ^. text1 `shouldBe` "Test restore"
    model2 ^. count1 `shouldBe` 0

  where
    wenv = mockWenv (MainModel "Test" 10)
    node1 = vstack [
        textField text1
      ]
    (model2, oldInfo, rstInfo) = handleRestoredEvents wenv node1

restoreComposite :: Spec
restoreComposite = describe "restoreComposite" $ do
  it "should have same info" $ do
    oldInfo `shouldBe` rstInfo
    model2 ^. text1 `shouldBe` "Test restore"
    model2 ^. count1 `shouldBe` 10

  where
    wenv = mockWenv (MainModel "Test" 10)
    handleEvent
      :: WidgetEnv MainModel MainEvt
      -> WidgetNode MainModel MainEvt
      -> MainModel
      -> MainEvt
      -> [EventResponse MainModel MainEvt ()]
    handleEvent wenv node model evt = []
    buildUI wenv model = vstack [
        textField text1
      ]
    node1 = composite "main" id buildUI handleEvent
    (model2, oldInfo, rstInfo) = handleRestoredEvents wenv node1

handleRestoredEvents wenv node1 = (model2, oldInfo, rstInfo) where
  oldNode = nodeHandleEventRoot wenv (replicate 4 (evtK keyRight)) node1
  newNode = node1 `style` [textColor red]
  inst1 = widgetSave (oldNode ^. L.widget) wenv oldNode
  inst2 = deserialise (serialise inst1)
  wenvRest = mockWenv (MainModel "Test" 0)
  evts = [evtK keyTab, evtT " restore"]
  ((wenv2, node2, _, _), ctx) = nodeHandleRestore wenvRest evts inst2 newNode
  model2 = wenv2 ^. L.model
  oldStyle = setStyleValue (oldNode ^. L.info . L.style) setFontColorL (?~) red
  oldInfo = oldNode ^. L.info
    & L.style .~ oldStyle
  rstInfo = node2 ^. L.info
    & L.widgetId .~ WidgetId 0 rootPath
    & L.path .~ rootPath
