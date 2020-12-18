{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.ContainerSpec (spec) where

import Debug.Trace
import Control.Lens ((&), (^.), (.~), (%~))
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
import Monomer.Widgets.Stack
import Monomer.Widgets.TextField

import qualified Monomer.Lens as L

data TestModel = TestModel {
  _tmText1 :: Text,
  _tmText2 :: Text
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''TestModel

-- This uses Stack for testing, since Container is a template and not a real container
spec :: Spec
spec = describe "Container"
  handleEvent

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  handleEventNormal
  handleEventNoKey
  handleEventLocalKey

handleEventNormal :: Spec
handleEventNormal = describe "handleEventNormal" $
  it "should insert new text at the right location, since widgets match" $ do
    model1 ^. text1 `shouldBe` "aacc"
    model1 ^. text2 `shouldBe` ""
    modelM ^. text1 `shouldBe` "aabbcc"
    modelM ^. text2 `shouldBe` ""

  where
    wenv = mockWenv (TestModel "" "")
    cntNode1 = vstack [
        textField text1,
        textField text2
      ]
    cntNode2 = vstack [
        textField text1,
        textField text2
      ]
    evts1 = [evtK keyTab, evtT "aacc", moveCharL, moveCharL]
    model1 = nodeHandleEventModel wenv evts1 cntNode1
    (wenv1, _, oldRoot1) = fst $ nodeHandleEvents wenv evts1 cntNode1
    cntResM = widgetMerge (cntNode2 ^. L.widget) wenv1 oldRoot1 cntNode2
    evts2 = [evtK keyTab, evtT "bb"]
    modelM = nodeHandleEventModelNoInit wenv1 evts2 (cntResM ^. L.node)

handleEventNoKey :: Spec
handleEventNoKey = describe "handleEventNoKey" $
  it "should insert new text at the beginning, since its merged without a key and state is lost" $ do
    model1 ^. text1 `shouldBe` "aacc"
    model1 ^. text2 `shouldBe` ""
    modelM ^. text1 `shouldBe` "bbaacc"
    modelM ^. text2 `shouldBe` ""

  where
    wenv = mockWenv (TestModel "" "")
    cntNode1 = vstack [
        textField text1,
        textField text2
      ]
    cntNode2 = vstack [
        textField text2,
        textField text1
      ]
    evts1 = [evtK keyTab, evtT "aacc", moveCharL, moveCharL]
    model1 = nodeHandleEventModel wenv evts1 cntNode1
    (wenv1, _, oldRoot1) = fst $ nodeHandleEvents wenv evts1 cntNode1
    cntResM = widgetMerge (cntNode2 ^. L.widget) wenv1 oldRoot1 cntNode2
    evts2 = [evtK keyTab, evtK keyTab, evtT "bb"]
    modelM = nodeHandleEventModelNoInit wenv1 evts2 (cntResM ^. L.node)

handleEventLocalKey :: Spec
handleEventLocalKey = describe "handleEventLocalKey" $
  it "should insert new text at the correct location, since its merged with a key" $ do
    model1 ^. text1 `shouldBe` "aacc"
    model1 ^. text2 `shouldBe` ""
    modelM ^. text1 `shouldBe` "aabbcc"
    modelM ^. text2 `shouldBe` ""

  where
    wenv = mockWenv (TestModel "" "")
    cntNode1 = vstack [
        textField text1 `key` "txt1",
        textField text2 `key` "txt2"
      ]
    cntNode2 = vstack [
        textField text2 `key` "txt2",
        textField text1 `key` "txt1"
      ]
    evts1 = [evtK keyTab, evtT "aacc", moveCharL, moveCharL]
    model1 = nodeHandleEventModel wenv evts1 cntNode1
    (wenv1, _, oldRoot1) = fst $ nodeHandleEvents wenv evts1 cntNode1
    cntResM = widgetMerge (cntNode2 ^. L.widget) wenv1 oldRoot1 cntNode2
    evts2 = [evtK keyTab, evtK keyTab, evtT "bb"]
    modelM = nodeHandleEventModelNoInit wenv1 evts2 (cntResM ^. L.node)
