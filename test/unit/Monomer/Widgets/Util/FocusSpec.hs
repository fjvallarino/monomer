{-|
Module      : Monomer.Widgets.Util.FocusSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for Focus handling.
-}
module Monomer.Widgets.Util.FocusSpec (spec) where

import Control.Lens ((&), (^.), (.~), ix)
import Data.Default
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Util.Focus
import Monomer.TestUtil

import qualified Monomer.Lens as L

spec :: Spec
spec = describe "Focus" $ do
  testParentPath
  testNextTargetStep
  testIsFocusCandidate

testParentPath :: Spec
testParentPath = describe "parentPath" $ do
  it "should return root path" $ do
    parentPath (pathNode []) `shouldBe` emptyPath
    parentPath (pathNode [0]) `shouldBe` emptyPath

  it "should return a single element path" $ do
    parentPath (pathNode [0, 1]) `shouldBe` Seq.fromList [0]
    parentPath (pathNode [1, 4]) `shouldBe` Seq.fromList [1]

  it "should return a multiple element path" $ do
    parentPath (pathNode [0, 1, 2]) `shouldBe` Seq.fromList [0, 1]
    parentPath (pathNode [0, 1, 2, 3, 4]) `shouldBe` Seq.fromList [0, 1, 2, 3]

testNextTargetStep :: Spec
testNextTargetStep = describe "nextTargetStep" $ do
  it "should return Nothing if next step is not valid" $ do
    nextTargetStep (pathNode []) (path []) `shouldBe` Nothing
    nextTargetStep (pathNode_ [] 5) (path []) `shouldBe` Nothing
    nextTargetStep (pathNode_ [0] 5) (path [0]) `shouldBe` Nothing
    nextTargetStep (pathNode_ [0] 5) (path [3]) `shouldBe` Nothing

  it "should return a valid target step" $ do
    nextTargetStep (pathNode_ [] 5) (path [2]) `shouldBe` Just 2
    nextTargetStep (pathNode_ [0] 5) (path [0, 3]) `shouldBe` Just 3

testIsFocusCandidate :: Spec
testIsFocusCandidate = describe "isFocusCandidate" $ do
  it "should return False if not backward candidate" $ do
    isFocusCandidate (pathNode [0]) (path [0]) FocusBwd `shouldBe` False
    isFocusCandidate (pathNode [0, 1]) (path [0, 0]) FocusBwd `shouldBe` False

  it "should return True if backward candidate" $ do
    isFocusCandidate (pathNode []) (path []) FocusBwd `shouldBe` True
    isFocusCandidate (pathNode []) (path [0]) FocusBwd `shouldBe` True
    isFocusCandidate (pathNode [0]) (path [1]) FocusBwd `shouldBe` True
    isFocusCandidate (pathNode [0, 0]) (path [0, 1]) FocusBwd `shouldBe` True
    isFocusCandidate (pathNode [0, 0]) (path [0, 0, 1]) FocusBwd `shouldBe` True
    isFocusCandidate (pathNode [0, 1, 1]) (path [0, 2]) FocusBwd `shouldBe` True

  it "should return False if not forward candidate" $ do
    isFocusCandidate (pathNode []) (path []) FocusFwd `shouldBe` False
    isFocusCandidate (pathNode []) (path [0]) FocusFwd `shouldBe` False
    isFocusCandidate (pathNode [0]) (path [1]) FocusFwd `shouldBe` False
    isFocusCandidate (pathNode [0, 0]) (path [0, 1]) FocusFwd `shouldBe` False

  it "should return True if forward candidate" $ do
    isFocusCandidate (pathNode [0]) (path []) FocusFwd `shouldBe` True
    isFocusCandidate (pathNode [1]) (path [0]) FocusFwd `shouldBe` True
    isFocusCandidate (pathNode [0, 1]) (path [0, 0]) FocusFwd `shouldBe` True
    isFocusCandidate (pathNode [0, 2]) (path [0, 1, 1]) FocusFwd `shouldBe` True

path :: [PathStep] -> Path
path p = Seq.fromList p

pathNode :: [PathStep] -> WidgetNode s e
pathNode path = pathNode_ path 0

pathNode_ :: [PathStep] -> Int -> WidgetNode s e
pathNode_ path childCount = newNode where
  mkChild idx = pathNode_ (path ++ [idx]) 0
  newNode = label "Test"
    & L.info . L.path .~ Seq.fromList path
    & L.info . L.visible .~ True
    & L.info . L.enabled .~ True
    & L.info . L.focusable .~ True
    & L.children .~ Seq.fromList (fmap mkChild [0..childCount - 1])
