{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widgets.TooltipSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Data.Default
import Data.Text (Text)
import Test.Hspec

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.Main
import Monomer.TestUtil
import Monomer.TestEventUtil
import Monomer.Widgets.Label
import Monomer.Widgets.Stack
import Monomer.Widgets.Tooltip

import qualified Monomer.Lens as L

spec :: Spec
spec = describe "Tooltip" $ do
  handleEvent
  getSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should not generate a render schedule" $ do
    ctx [] ^. L.renderRequested `shouldBe` True
    ctx [] ^. L.renderSchedule `shouldBe` M.empty

  it "should generate a render schedule after moving" $ do
    let evt = Move (Point 10 10)
    let path = Seq.fromList [0, 0]
    let schedule = RenderSchedule path 0 1000 (Just 1)
    ctx [evt] ^. L.renderRequested `shouldBe` True
    ctx [evt] ^. L.renderSchedule `shouldBe` M.fromList [(path, schedule)]

  it "should generate a render schedule even after moving, and RenderOnce after" $ do
    let evt1 = Move (Point 10 10)
    let evt2 = Move (Point 50 50)
    let path = Seq.fromList [0, 0]
    let schedule = RenderSchedule path 0 1000 (Just 1)
    ctx [evt1, evt2] ^. L.renderRequested `shouldBe` True
    ctx [evt1, evt2] ^. L.renderSchedule `shouldBe` M.fromList [(path, schedule)]

  where
    wenv = mockWenvEvtUnit ()
    ttNode = vstack [
        tooltip "" (label "Test")
      ]
    ctx es = nodeHandleEventCtx wenv es ttNode

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
