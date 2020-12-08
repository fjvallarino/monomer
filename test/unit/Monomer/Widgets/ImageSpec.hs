module Monomer.Widgets.ImageSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Image

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

  it "should create a task when merging to a different path" $ do
    Seq.length reqs3 `shouldBe` 1
    Seq.index reqs3 0 `shouldSatisfy` isRunTask

  where
    wenv = mockWenv ()
    node1 = image "assets/images/beach.jpg"
    node2 = image "assets/images/beach.jpg"
    node3 = image "assets/images/beach2.jpg"
    WidgetResult newNode1 reqs1 _ = widgetInit (node1 ^. L.widget) wenv node1
    WidgetResult _ reqs2 _ = widgetMerge (node2 ^. L.widget) wenv newNode1 node2
    WidgetResult _ reqs3 _ = widgetMerge (node3 ^. L.widget) wenv newNode1 node3

isRunTask :: WidgetRequest s -> Bool
isRunTask RunTask{} = True
isRunTask _ = False
