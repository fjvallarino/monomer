{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widgets.Animate.FadeSpec (spec) where

import Control.Lens ((&), (^.), (.~), (?~), (^?!), _1, _3, ix)
import Data.Default
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestUtil
import Monomer.TestEventUtil
import Monomer.Widgets.Animate.Fade
import Monomer.Widgets.Label
import Monomer.Widgets.Scroll
import Monomer.Widgets.Stack

import qualified Monomer.Lens as L

spec :: Spec
spec = describe "Fade" $ do
  initWidget
  handleMessage
  getSizeReq

initWidget :: Spec
initWidget = describe "initWidget" $ do
  it "should not request rendering if autoStart = False" $
    reqs nodeNormal `shouldBe` Seq.empty

  it "should request rendering if autoStart = True" $
    reqs nodeAuto ^?! ix 0 `shouldSatisfy` isRenderEvery

  where
    wenv = mockWenvEvtUnit ()
    nodeNormal = fadeIn (label "Test")
    nodeAuto = fadeIn_ [autoStart] (label "Test")
    reqs node = nodeHandleEvents_ wenv WInit [] node ^?! ix 0 . _1 . _3

handleMessage :: Spec
handleMessage = describe "handleMessage" $ do
  it "should not request rendering if an invalid message is received" $
    reqs ScrollReset `shouldBe` Seq.empty

  it "should request rendering if AnimateStart is received" $
    reqs AnimateStart ^?! ix 0 `shouldSatisfy` isRenderEvery

  it "should cancel rendering if AnimateStop is received" $
    reqs AnimateStop ^?! ix 0 `shouldSatisfy` isRenderStop

  where
    wenv = mockWenvEvtUnit ()
    node = nodeInit wenv $ fadeIn (label "Test")
    reqs msg = rqs where
      rqs = case widgetHandleMessage (node^. L.widget) wenv rootPath msg node of
        Just (WidgetResult _ rqs _) -> rqs
        _ -> Seq.empty

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
    (tSizeReqW, tSizeReqH) = nodeGetSizeReq wenv (fadeIn lblNode)
