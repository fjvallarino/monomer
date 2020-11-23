module Monomer.Widgets.Util.FocusSpec (spec) where

import Control.Lens ((^.), ix)
import Data.Default
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Widgets.Label
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
    parentPath (pathInst []) `shouldBe` rootPath
    parentPath (pathInst [0]) `shouldBe` rootPath

  it "should return a single element path" $ do
    parentPath (pathInst [0, 1]) `shouldBe` Seq.fromList [0]
    parentPath (pathInst [1, 4]) `shouldBe` Seq.fromList [1]

  it "should return a multiple element path" $ do
    parentPath (pathInst [0, 1, 2]) `shouldBe` Seq.fromList [0, 1]
    parentPath (pathInst [0, 1, 2, 3, 4]) `shouldBe` Seq.fromList [0, 1, 2, 3]

testNextTargetStep :: Spec
testNextTargetStep = describe "nextTargetStep" $ do
  it "should return Nothing if next step is not valid" $ do
    nextTargetStep (path []) (pathInst []) `shouldBe` Nothing
    nextTargetStep (path []) (pathInst_ [] 5) `shouldBe` Nothing
    nextTargetStep (path [0]) (pathInst_ [0] 5) `shouldBe` Nothing
    nextTargetStep (path [3]) (pathInst_ [0] 5) `shouldBe` Nothing

  it "should return a valid target step" $ do
    nextTargetStep (path [2]) (pathInst_ [] 5) `shouldBe` Just 2
    nextTargetStep (path [0, 3]) (pathInst_ [0] 5) `shouldBe` Just 3

testIsFocusCandidate :: Spec
testIsFocusCandidate = describe "isFocusCandidate" $ do
  it "should return False if not backward candidate" $ do
    isFocusCandidate FocusBwd (path [0]) (pathInst [0]) `shouldBe` False
    isFocusCandidate FocusBwd (path [0, 0]) (pathInst [0, 1]) `shouldBe` False

  it "should return True if backward candidate" $ do
    isFocusCandidate FocusBwd (path []) (pathInst []) `shouldBe` True
    isFocusCandidate FocusBwd (path [0]) (pathInst []) `shouldBe` True
    isFocusCandidate FocusBwd (path [1]) (pathInst [0]) `shouldBe` True
    isFocusCandidate FocusBwd (path [0, 1]) (pathInst [0, 0]) `shouldBe` True
    isFocusCandidate FocusBwd (path [0, 0, 1]) (pathInst [0, 0]) `shouldBe` True
    isFocusCandidate FocusBwd (path [0, 2]) (pathInst [0, 1, 1]) `shouldBe` True

  it "should return False if not forward candidate" $ do
    isFocusCandidate FocusFwd (path []) (pathInst []) `shouldBe` False
    isFocusCandidate FocusFwd (path [0]) (pathInst []) `shouldBe` False
    isFocusCandidate FocusFwd (path [1]) (pathInst [0]) `shouldBe` False
    isFocusCandidate FocusFwd (path [0, 1]) (pathInst [0, 0]) `shouldBe` False

  it "should return True if forward candidate" $ do
    isFocusCandidate FocusFwd (path []) (pathInst [0]) `shouldBe` True
    isFocusCandidate FocusFwd (path [0]) (pathInst [1]) `shouldBe` True
    isFocusCandidate FocusFwd (path [0, 0]) (pathInst [0, 1]) `shouldBe` True
    isFocusCandidate FocusFwd (path [0, 1, 1]) (pathInst [0, 2]) `shouldBe` True

path :: [PathStep] -> Path
path p = Seq.fromList p

pathInst :: [PathStep] -> WidgetInstance s e
pathInst path = pathInst_ path 0

pathInst_ :: [PathStep] -> Int -> WidgetInstance s e
pathInst_ path childCount = newInst where
  mkChild idx = pathInst_ (path ++ [idx]) 0
  newInst = (label "Test") {
    _wiPath = Seq.fromList path,
    _wiChildren = Seq.fromList $ fmap mkChild [0..childCount - 1],
    _wiVisible = True,
    _wiEnabled = True,
    _wiFocusable = True
  }
