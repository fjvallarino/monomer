{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Containers.DragDropSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Data.List (delete)
import Data.Sequence (Seq(..))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestUtil
import Monomer.TestEventUtil
import Monomer.Widgets.Composite
import Monomer.Widgets.Containers.Draggable
import Monomer.Widgets.Containers.DropTarget
import Monomer.Widgets.Containers.Grid
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.Label

import qualified Monomer.Lens as L

data TestEvt
  = DropTo1 Int
  | DropTo2 Int
  deriving (Eq, Show)

data TestItem = TestItem {
  _tiIdx :: Int,
  _tiMsg :: Text
} deriving (Eq, Show)

data TestModel = TestModel {
  _tmItems1 :: [Int],
  _tmItems2 :: [Int]
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''TestItem
makeLensesWith abbreviatedFields ''TestModel

spec :: Spec
spec = describe "Drag & Drop" $ do
  handleEvent
  getSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should leave items intact if drag is not completed" $ do
    let selStart = Point 20 10
    let selEnd = Point 20 100
    let steps = evtDrag selStart selEnd
    model steps ^. items1 `shouldBe` [1, 2, 3, 4, 5]
    model steps ^. items2 `shouldBe` []

  it "should update items list if drag was successful" $ do
    let selStart = Point 20 10
    let selEnd = Point 400 10
    let steps = evtDrag selStart selEnd
    model steps ^. items1 `shouldBe` [2, 3, 4, 5]
    model steps ^. items2 `shouldBe` [1]

  it "should both update items list if successive drags were successful" $ do
    let selStart = Point 20 10
    let selEnd = Point 400 10
    let steps = evtDrag selStart selEnd
                ++ evtDrag selStart selEnd 
                ++ evtDrag selStart selEnd 
                ++ evtDrag selEnd selStart
    model steps ^. items1 `shouldBe` [4, 5, 1]
    model steps ^. items2 `shouldBe` [2, 3]

  where
    wenv = mockWenvEvtUnit (TestModel [1..5] [])
    handleEvent wenv node model evt = case evt of
      DropTo1 idx -> [Model $ model
        & items2 .~ delete idx (model ^. items2)
        & items1 .~ model ^. items1 ++ [idx]]
      DropTo2 idx -> [Model $ model
        & items1 .~ delete idx (model ^. items1)
        & items2 .~ model ^. items2 ++ [idx]]
    buildUI wenv model = hgrid [
        dropTarget DropTo1 $ vstack (fmap dragLbl (model ^. items1)),
        dropTarget DropTo2 $ vstack (fmap dragLbl (model ^. items2))
      ]
    dragLbl idx = draggable idx (label "Label")
    mainNode = composite "main" id buildUI handleEvent
    model es = nodeHandleEventModel wenv es mainNode

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return same reqW as child node" $ do
    dgSizeReqW `shouldBe` lSizeReqW
    dtSizeReqW `shouldBe` lSizeReqW

  it "should return same reqH as child node" $ do
    dgSizeReqH `shouldBe` lSizeReqH
    dtSizeReqH `shouldBe` lSizeReqH

  where
    wenv = mockWenv (TestModel [] [])
    item = TestItem 0 ""
    lblNode = label "Test label"
    (lSizeReqW, lSizeReqH) = nodeGetSizeReq wenv lblNode
    (dgSizeReqW, dgSizeReqH) = nodeGetSizeReq wenv (draggable item lblNode)
    (dtSizeReqW, dtSizeReqH) = nodeGetSizeReq wenv (dropTarget DropTo2 lblNode)
