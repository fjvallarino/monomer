{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widgets.TooltipSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Data.Default
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestUtil
import Monomer.TestEventUtil
import Monomer.Widgets.Label
import Monomer.Widgets.Tooltip

import qualified Monomer.Lens as L

spec :: Spec
spec = fdescribe "Tooltip" $ do
  handleEvent
  getSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should not generate events" $ do
    events [] `shouldBe` Seq.empty

  where
    wenv = mockWenvEvtUnit ()
    ttNode = tooltip "" (label "Test")
    events es = nodeHandleEventEvts wenv es ttNode

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return same reqW as child node" $
    tSizeReqW `shouldBe` lSizeReqW

  it "should return same reqH as child node" $
    tSizeReqH `shouldBe` lSizeReqH

  where
    wenv = mockWenvEvtUnit ()
    lblNode = label "Test label"
    (lSizeReqW, lSizeReqH) = nodeGetSizeReq wenv lblNode
    (tSizeReqW, tSizeReqH) = nodeGetSizeReq wenv (tooltip "" lblNode)
