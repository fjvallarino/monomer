{-|
Module      : Monomer.Widgets.ContainerSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for Container base widget.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.ContainerSpec (spec) where

import Control.Lens ((&), (^.), (^?), (.~), (%~), ix)
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Data.List (find)
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestEventUtil
import Monomer.TestUtil
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.TextField

import qualified Monomer.Lens as L

data TestModel = TestModel {
  _tmText1 :: Text,
  _tmText2 :: Text
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''TestModel

pathLens idx = L.children . ix idx . L.info . L.path
widLens idx = L.children . ix idx . L.info . L.widgetId

-- This uses Stack for testing, since Container is a template and not a real container
spec :: Spec
spec = describe "Container" $ do
  mergeUserResize
  handleEvent

mergeUserResize :: Spec
mergeUserResize = describe "merge resize" $ do
  it "should not generate a request if user size did not change" $ do
    let result = mergeSizeReq [] []
    find isResizeWidgets (result ^. L.requests) `shouldBe` Nothing

  it "should generate a ResizeWidgets request if user size changed" $ do
    let result = mergeSizeReq [width 100] []
    find isResizeWidgets (result ^. L.requests) `shouldNotBe` Nothing

  it "should generate a ResizeWidgets request if user size changed" $ do
    let result = mergeSizeReq [width 100] [width 200]
    find isResizeWidgets (result ^. L.requests) `shouldNotBe` Nothing

  where
    wenv = mockWenvEvtUnit (TestModel "" "")
    oldNode = vstack []
    newNode = vstack []
    mergeSizeReq oldStyle newStyle = result where
      oldNode2 = nodeInit wenv $
        oldNode `styleBasic` oldStyle
      newNode2 = newNode
        `styleBasic` newStyle
      result = widgetMerge (newNode2 ^. L.widget) wenv newNode2 oldNode2

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
    newRoot ^? pathLens 0 `shouldBe` Just (Seq.fromList [0, 0])
    newRoot ^? pathLens 1 `shouldBe` Just (Seq.fromList [0, 1])
    newRoot ^? widLens 0 `shouldBe` Just (WidgetId 0 (Seq.fromList [0, 0]))
    newRoot ^? widLens 1 `shouldBe` Just (WidgetId 0 (Seq.fromList [0, 1]))

  where
    wenv = mockWenvEvtUnit (TestModel "" "")
    cntNode1 = vstack [
        textField text1,
        textField text2
      ]
    cntNode2 = vstack [
        textField text1,
        textField text2
      ]
    evts1 = [evtT "aacc", moveCharL, moveCharL]
    model1 = nodeHandleEventModel wenv evts1 cntNode1
    (wenv1, root1, _) = fst $ nodeHandleEvents wenv WInit evts1 cntNode1
    cntNodeM = nodeMerge wenv1 cntNode2 root1
    evts2 = [evtK keyTab, evtT "bb"]
    (wenv2, root2, _) = fst $ nodeHandleEvents wenv WNoInit evts2 cntNodeM
    modelM = wenv2 ^. L.model
    newRoot = root2

handleEventNoKey :: Spec
handleEventNoKey = describe "handleEventNoKey" $
  it "should insert new text at the end, since its merged without a key and state is lost" $ do
    model1 ^. text1 `shouldBe` "aacc"
    model1 ^. text2 `shouldBe` ""
    modelM ^. text1 `shouldBe` "aaccbb"
    modelM ^. text2 `shouldBe` ""
    newRoot ^? pathLens 0 `shouldBe` Just (Seq.fromList [0, 0])
    newRoot ^? pathLens 1 `shouldBe` Just (Seq.fromList [0, 1])
    newRoot ^? widLens 0 `shouldBe` Just (WidgetId 0 (Seq.fromList [0, 0]))
    newRoot ^? widLens 1 `shouldBe` Just (WidgetId 0 (Seq.fromList [0, 1]))

  where
    wenv = mockWenvEvtUnit (TestModel "" "")
    cntNode1 = vstack [
        textField text1,
        textField text2
      ]
    cntNode2 = vstack [
        textField text2,
        textField text1
      ]
    evts1 = [evtT "aacc", moveCharL, moveCharL]
    model1 = nodeHandleEventModel wenv evts1 cntNode1
    (wenv1, root1, _) = fst $ nodeHandleEvents wenv WInit evts1 cntNode1
    cntNodeM = nodeMerge wenv1 cntNode2 root1
    evts2 = [evtK keyTab, evtK keyTab, evtT "bb"]
    (wenv2, root2, _) = fst $ nodeHandleEvents wenv WNoInit evts2 cntNodeM
    modelM = wenv2 ^. L.model
    newRoot = root2

handleEventLocalKey :: Spec
handleEventLocalKey = describe "handleEventLocalKey" $
  it "should insert new text at the correct location, since its merged with a key" $ do
    model1 ^. text1 `shouldBe` "aacc"
    model1 ^. text2 `shouldBe` ""
    modelM ^. text1 `shouldBe` "aabbcc"
    modelM ^. text2 `shouldBe` ""
    -- WidgetId stays the same even after path changed
    newRoot ^? pathLens 0 `shouldBe` Just (Seq.fromList [0, 0])
    newRoot ^? pathLens 1 `shouldBe` Just (Seq.fromList [0, 1])
    newRoot ^? widLens 0 `shouldBe` Just (WidgetId 0 (Seq.fromList [0, 1]))
    newRoot ^? widLens 1 `shouldBe` Just (WidgetId 0 (Seq.fromList [0, 0]))

  where
    wenv = mockWenvEvtUnit (TestModel "" "")
    cntNode1 = vstack [
        textField text1 `nodeKey` "txt1",
        textField text2 `nodeKey` "txt2"
      ]
    cntNode2 = vstack [
        textField text2 `nodeKey` "txt2",
        textField text1 `nodeKey` "txt1"
      ]
    evts1 = [evtT "aacc", moveCharL, moveCharL]
    model1 = nodeHandleEventModel wenv evts1 cntNode1
    (wenv1, root1, _) = fst $ nodeHandleEvents wenv WInit evts1 cntNode1
    cntNodeM = nodeMerge wenv1 cntNode2 root1
    evts2 = [evtK keyTab, evtK keyTab, evtT "bb"]
    (wenv2, root2, _) = fst $ nodeHandleEvents wenv WNoInit evts2 cntNodeM
    modelM = wenv2 ^. L.model
    newRoot = root2
