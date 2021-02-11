{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widgets.TooltipSpec (spec) where

import Control.Lens ((&), (^.), (.~), (%~))
import Data.Default
import Data.Sequence (Seq(..))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Core.Themes.SampleThemes
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
  handleEventFollow
  getSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should not generate a render schedule" $ do
    reqs [] `shouldBe` Seq.empty

  it "should generate a render schedule after moving" $ do
    let evt = Move (Point 10 10)
    let widgetId = WidgetId 0 rootPath
    let renderEveryReq = RenderEvery widgetId 1000 (Just 1)
    reqs [evt] `shouldBe` Seq.fromList [renderEveryReq]

  it "should ony generate a render schedule even after moving, since delay has not passed" $ do
    let evt1 = Move (Point 10 10)
    let evt2 = Move (Point 50 50)
    let widgetId = WidgetId 0 rootPath
    let renderEveryReq = RenderEvery widgetId 1000 (Just 1)
    reqs [evt1, evt2] `shouldBe` Seq.fromList [renderEveryReq]

  where
    wenv = mockWenvEvtUnit ()
    ttNode = tooltip "" (label "Test")
    reqs es = getReqs wenv ttNode es

handleEventFollow :: Spec
handleEventFollow = describe "handleEventFollow" $ do
  it "should not generate a render schedule" $ do
    reqs [] `shouldBe` Seq.empty

  it "should generate a render schedule after moving" $ do
    let evt = Move (Point 10 10)
    let widgetId = WidgetId 0 rootPath
    let renderEveryReq = RenderEvery widgetId 500 (Just 1)
    reqs [evt] `shouldBe` Seq.fromList [renderEveryReq]

  it "should generate a render schedule even after moving, and RenderOnce after" $ do
    let evt1 = Move (Point 10 10)
    let evt2 = Move (Point 50 50)
    let widgetId = WidgetId 0 rootPath
    let renderEveryReq = RenderEvery widgetId 500 (Just 1)
    reqs [evt1, evt2] `shouldBe` Seq.fromList [renderEveryReq, RenderOnce]

  where
    wenv = mockWenvEvtUnit ()
    ttNode = tooltip_ "" [tooltipDelay 500, tooltipFollow] (label "Test")
    reqs es = getReqs wenv ttNode es

getReqs
  :: Eq s
  => WidgetEnv s e
  -> WidgetNode s e
  -> [SystemEvent]
  -> Seq (WidgetRequest s)
getReqs wenv node [] = Seq.empty
getReqs wenv node (e:es) = tmpReqs <> newReqs where
  -- Each component generates a RenderOnce request when Enter event is received
  tmpNode = nodeHandleEventRoot wenv [e] node
  tmpReqs = Seq.drop 2 $ nodeHandleEventReqs wenv [e] node
  newWenv = wenv & L.timestamp %~ (+1000)
  newReqs = Seq.drop 2 $ nodeHandleEventReqs newWenv es tmpNode

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return same reqW as child node" $
    tSizeReqW `shouldBe` lSizeReqW

  it "should return same reqH as child node" $
    tSizeReqH `shouldBe` lSizeReqH

  where
    wenv = mockWenvEvtUnit ()
      & L.theme .~ darkTheme
    lblNode = label "Test label"
    (lSizeReqW, lSizeReqH) = nodeGetSizeReq wenv lblNode
    (tSizeReqW, tSizeReqH) = nodeGetSizeReq wenv (tooltip "" lblNode)
