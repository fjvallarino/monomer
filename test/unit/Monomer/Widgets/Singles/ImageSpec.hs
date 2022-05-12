{-|
Module      : Monomer.Widgets.Singles.ImageSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for Image widget.
-}
module Monomer.Widgets.Singles.ImageSpec (spec) where

import Control.Lens ((&), (^.), (.~), _2)
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Singles.Image

import qualified Monomer.Lens as L

spec :: Spec
spec = describe "Image"
  initMergeWidget

initMergeWidget :: Spec
initMergeWidget = describe "init/merge" $ do
  it "should create a RunTask on init" $ do
    Seq.length reqs1 `shouldBe` 1
    Seq.index reqs1 0 `shouldSatisfy` isRunTask

  it "should not create a task when merging to the same path" $
    Seq.length reqs2 `shouldBe` 0

  it "should create two tasks when merging to a different path" $ do
    Seq.length reqs3 `shouldBe` 3
    Seq.index reqs3 0 `shouldSatisfy` isRunTask
    Seq.index reqs3 1 `shouldSatisfy` isRemoveRendererImage
    Seq.index reqs3 2 `shouldSatisfy` isRunTask

  it "should have one widgetId on init (loading)" $
    ctx1 ^. L.widgetPaths `shouldSatisfy` (== 1) . length

  it "should not have any widgetId on merge with the same image (not loading)" $
    ctx2 ^. L.widgetPaths `shouldSatisfy` null

  it "should have one widgetId on merge with a different image (loading)" $
    ctx3 ^. L.widgetPaths `shouldSatisfy` (== 1) . length

  where
    wenv = mockWenvEvtUnit ()
    node1 = image "assets/images/beach.jpg"
    node2 = image "assets/images/beach.jpg"
    node3 = image "assets/images/beach2.jpg"
    initRes = widgetInit (node1 ^. L.widget) wenv node1
    WidgetResult newNode1 reqs1 = initRes
    mergeRes1 = widgetMerge (node2 ^. L.widget) wenv node2 newNode1
    WidgetResult _ reqs2 = mergeRes1
    mergeRes2 = widgetMerge (node3 ^. L.widget) wenv node3 newNode1
    WidgetResult _ reqs3 = mergeRes2
    ctx1 = nodeHandleResult wenv initRes ^. _2
    ctx2 = nodeHandleResult wenv mergeRes1 ^. _2
    ctx3 = nodeHandleResult wenv mergeRes2 ^. _2
