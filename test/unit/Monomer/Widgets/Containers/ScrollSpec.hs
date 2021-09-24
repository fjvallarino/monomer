{-|
Module      : Monomer.Widgets.Containers.ScrollSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for Scroll widget.
-}
module Monomer.Widgets.Containers.ScrollSpec (spec) where

import Control.Lens ((&), (^.), (^?), (^?!), (.~), _Just, ix)
import Data.Default
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Core.Themes.SampleThemes
import Monomer.Event
import Monomer.Graphics.ColorTable
import Monomer.TestEventUtil
import Monomer.TestUtil
import Monomer.Widgets.Composite
import Monomer.Widgets.Containers.Scroll
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.Button
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.Spacer

import qualified Monomer.Lens as L

data ButtonEvt
  = Button1
  | Button2
  | Button3
  | Button4
  deriving (Eq, Show)

spec :: Spec
spec = describe "Scroll" $ do
  handleEvent
  forwardStyle
  resize

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  handleBarClick
  handleChildrenFocus
  handleNestedWheel
  handleMessageReset

handleBarClick :: Spec
handleBarClick = describe "handleBarClick" $ do
  it "should click the first button" $ do
    evts [evtClick point] `shouldBe` Seq.fromList [Button1]

  it "should scroll right and click the third button" $ do
    evts [evtPress midHBar, evtClick point] `shouldBe` Seq.fromList [Button2]

  it "should scroll down and click the third button" $ do
    evts [evtPress midVBar, evtClick point] `shouldBe` Seq.fromList [Button3]

  it "should scroll down and click the third button" $ do
    evts [evtPress midVBar, evtClick point] `shouldBe` Seq.fromList [Button3]

  it "should scroll down and right and click the fourth button" $ do
    evts [evtPress midHBar, evtPress midVBar, evtClick point] `shouldBe` Seq.fromList [Button4]

  where
    wenv = mockWenv ()
      & L.theme .~ darkTheme
      & L.windowSize .~ Size 640 480
    point = Point 320 200
    midHBar = Point 630 476
    midVBar = Point 636 470
    st = [width 640, height 480]
    stackNode = vstack [
        hstack [
          button "Button 1" Button1 `styleBasic` st,
          button "Button 2" Button2 `styleBasic` st
        ],
        hstack [
          button "Button 3" Button3 `styleBasic` st,
          button "Button 4" Button4 `styleBasic` st
        ]
      ]
    scrollNode = scroll stackNode
    evts es = nodeHandleEventEvts wenv es scrollNode

handleChildrenFocus :: Spec
handleChildrenFocus = describe "handleChildrenFocus" $ do
  it "should not follow focus events" $ do
    evtsIgnore evts1 `shouldBe` Seq.fromList [Button1]
    evtsIgnore evts2 `shouldBe` Seq.fromList [Button1]
    evtsIgnore evts3 `shouldBe` Seq.fromList [Button1]

  it "should follow focus events" $ do
    evtsFollow evts1 `shouldBe` Seq.fromList [Button2]
    evtsFollow evts3 `shouldBe` Seq.fromList [Button4]

  where
    wenv = mockWenv () & L.windowSize .~ Size 640 480
    point = Point 320 200
    evts1 = [evtK keyTab, evtClick point]
    evts2 = [evtK keyTab, evtK keyTab, evtClick point]
    evts3 = [evtK keyTab, evtK keyTab, evtK keyTab, evtClick point]
    st = [width 640, height 480]
    stackNode = vstack [
        button "Button 1" Button1 `styleBasic` st,
        button "Button 2" Button2 `styleBasic` st,
        button "Button 3" Button3 `styleBasic` st,
        button "Button 4" Button4 `styleBasic` st
      ]
    ignoreNode = scroll_ [scrollFollowFocus_ False] stackNode
    followNode = scroll stackNode
    evtsIgnore es = nodeHandleEventEvts wenv es ignoreNode
    evtsFollow es = nodeHandleEventEvts wenv es followNode

handleNestedWheel :: Spec
handleNestedWheel = describe "handleNestedWheel" $ do
  it "should scroll main widget" $ do
    events evts1 `shouldBe` Seq.fromList [Button4]

  it "should scroll child widget" $ do
    events evts2 `shouldBe` Seq.fromList [Button3]

  where
    wenv = mockWenv () & L.windowSize .~ Size 640 480
    pointClick = Point 160 240
    pointWheel1 = Point 480 240
    pointWheel2 = Point 160 240
    evtWheel p = WheelScroll p (Point 0 (-2000)) WheelNormal
    evts1 = [evtWheel pointWheel1, evtClick pointClick]
    evts2 = [evtWheel pointWheel2, evtClick pointClick]
    st = [width 320, height 480]
    childNode = vscroll (vstack [
        button "Button 1" Button1 `styleBasic` st,
        button "Button 2" Button2 `styleBasic` st,
        button "Button 3" Button3 `styleBasic` st
      ]) `styleBasic` [height 480]
    mainNode = vstack [
        childNode,
        button "Button 4" Button4 `styleBasic` st
      ] `styleBasic` [width 320]
    scrollNode = vscroll $ hstack [
        mainNode,
        filler
      ]
    events es = nodeHandleEventEvts wenv es scrollNode

handleMessageReset :: Spec
handleMessageReset = describe "handleMessageReset" $ do
  it "should not generate an event if scroll does not show Button1" $
    events es `shouldBe` Seq.empty

  it "should generate an event if scroll shows Button1" $
    events (evtK keyTab : es) `shouldBe` Seq.singleton Button1

  where
    wenv = mockWenv ()
    es = [evtK keyTab, evtClick (Point 10 10), evtClick (Point 10 10)]
    handleEvent
      :: WidgetEnv () ButtonEvt
      -> WidgetNode () ButtonEvt
      -> ()
      -> ButtonEvt
      -> [EventResponse () ButtonEvt () ButtonEvt]
    handleEvent wenv node model evt = case evt of
      Button1 -> [Report Button1]
      Button3 -> [Message "mainScroll" ScrollReset]
      _ -> []
    buildUI wenv model = scroll (vstack [
        button "Button 1" Button1 `styleBasic` [height 480],
        button "Button 2" Button2 `styleBasic` [height 480],
        button "Button 3" Button3 `styleBasic` [height 480]
      ]) `nodeKey` "mainScroll"
    cmpNode = composite "main" id buildUI handleEvent
    events es = nodeHandleEventEvts wenv es cmpNode

forwardStyle :: Spec
forwardStyle = describe "forwardStyle" $ do
  it "should assign scroll the top style, while the child be set to default" $ do
    pnode1 ^? L.info . L.style . L.basic . _Just `shouldBe` Just (border 1 black <> padding 10)
    cnode1 ^? L.info . L.style . L.basic . _Just `shouldBe` Just def

  it "should split the style according to the rules of scrollFwdDefault" $ do
    pnode2 ^? L.info . L.style . L.basic . _Just `shouldBe` Just (border 1 black)
    cnode2 ^? L.info . L.style . L.basic . _Just `shouldBe` Just (padding 10)

  where
    wenv = mockWenv () & L.windowSize .~ Size 640 480
    snode1 = scroll (label "Test") `styleBasic` [border 1 black, padding 10]
    snode2 = scroll_ [scrollFwdStyle scrollFwdDefault] (label "Test") `styleBasic` [border 1 black, padding 10]
    pnode1 = nodeInit wenv snode1
    cnode1 = pnode1 ^?! L.children . ix 0
    pnode2 = nodeInit wenv snode2
    cnode2 = pnode2 ^?! L.children . ix 0

resize :: Spec
resize = describe "resize" $ do
  resizeLarge
  resizeSmall
  resizeOverlaySmall
  resizeH
  resizeV
  resizeOverlayH
  resizeOverlayV

resizeLarge :: Spec
resizeLarge = describe "resizeLarge" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign all the requested space" $
    childrenVp `shouldBe` Seq.fromList [cvp1]

  where
    wenv = mockWenv () & L.windowSize .~ Size 640 480
    vp   = Rect 0 0   640 480
    cvp1 = Rect 0 0 3000 2000
    scrollNode = scroll (label "" `styleBasic` [width 3000, height 2000])
    newNode = nodeInit wenv scrollNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> newNode ^. L.children

resizeSmall :: Spec
resizeSmall = describe "resizeSmall" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign all the requested space" $
    childrenVp `shouldBe` Seq.fromList [cvp1]

  where
    wenv = mockWenv () & L.windowSize .~ Size 640 480
    vp   = Rect 0 0 640 480
    cvp1 = Rect 0 0 640 480
    scrollNode = scroll (label "" `styleBasic` [width 300, height 200])
    newNode = nodeInit wenv scrollNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> newNode ^. L.children

resizeOverlaySmall :: Spec
resizeOverlaySmall = describe "resizeOverlaySmall" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign all the requested space" $
    childrenVp `shouldBe` Seq.fromList [cvp1]

  where
    wenv = mockWenv () & L.windowSize .~ Size 640 480
    vp   = Rect 0 0 640 480
    cvp1 = Rect 0 0 640 480
    scrollNode = scroll_ [scrollOverlay] (label "" `styleBasic` [width 300, height 200])
    newNode = nodeInit wenv scrollNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> newNode ^. L.children

resizeH :: Spec
resizeH = describe "resizeH" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign all the requested horizontal space" $
    childrenVp `shouldBe` Seq.fromList [cvp1]

  where
    wenv = mockWenv () & L.windowSize .~ Size 640 480
    vp   = Rect 0 0  640 480
    cvp1 = Rect 0 0 3000 470
    scrollNode = hscroll (label "" `styleBasic` [width 3000, height 2000])
    newNode = nodeInit wenv scrollNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> newNode ^. L.children

resizeV :: Spec
resizeV = describe "resizeV" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign all the requested vertical space" $
    childrenVp `shouldBe` Seq.fromList [cvp1]

  where
    wenv = mockWenv () & L.windowSize .~ Size 640 480
    vp   = Rect 0 0 640  480
    cvp1 = Rect 0 0 630 2000
    scrollNode = vscroll (label "" `styleBasic` [width 3000, height 2000])
    newNode = nodeInit wenv scrollNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> newNode ^. L.children

resizeOverlayH :: Spec
resizeOverlayH = describe "resizeOverlayH" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign all the requested horizontal space" $
    childrenVp `shouldBe` Seq.fromList [cvp1]

  where
    wenv = mockWenv () & L.windowSize .~ Size 640 480
    vp   = Rect 0 0  640 480
    cvp1 = Rect 0 0 3000 480
    scrollNode = hscroll_ [scrollOverlay] (label "" `styleBasic` [width 3000, height 2000])
    newNode = nodeInit wenv scrollNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> newNode ^. L.children

resizeOverlayV :: Spec
resizeOverlayV = describe "resizeOverlayV" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign all the requested vertical space" $
    childrenVp `shouldBe` Seq.fromList [cvp1]

  where
    wenv = mockWenv () & L.windowSize .~ Size 640 480
    vp   = Rect 0 0 640  480
    cvp1 = Rect 0 0 640 2000
    scrollNode = vscroll_ [scrollOverlay] (label "" `styleBasic` [width 3000, height 2000])
    newNode = nodeInit wenv scrollNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> newNode ^. L.children
