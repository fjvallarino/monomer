{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Util.StyleSpec (spec) where

import Control.Lens ((&), (^.), (^?), (^?!), (.~), (?~), _Just, ix, non)
import Data.Default
import Data.Sequence (Seq(..))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.Graphics
import Monomer.Widgets.Label
import Monomer.Widgets.Util.Style
import Monomer.TestUtil

import qualified Monomer.Lens as L

spec :: Spec
spec = describe "Style" $ do
  testActiveStyle
  testHandleSizeChange

testActiveStyle :: Spec
testActiveStyle = describe "activeStyle" $ do
  it "should return basic style" $
    activeStyle wenvBasic nodeNormal ^. L.bgColor `shouldBe` Just white

  it "should return hover style" $
    activeStyle wenvHover nodeNormal ^. L.bgColor `shouldBe` Just green

  it "should return hover style" $
    activeStyle wenvFocus nodeNormal ^. L.bgColor `shouldBe` Just blue

  it "should return hover style (takes precedence over focus)" $
    activeStyle wenvHoverFocus nodeNormal ^. L.bgColor `shouldBe` Just green

  it "should return disabled style" $ do
    activeStyle wenvBasic nodeDisabled ^. L.bgColor `shouldBe` Just gray
    activeStyle wenvHover nodeDisabled ^. L.bgColor `shouldBe` Just gray
    activeStyle wenvFocus nodeDisabled ^. L.bgColor `shouldBe` Just gray
    activeStyle wenvHoverFocus nodeDisabled ^. L.bgColor `shouldBe` Just gray

  where
    wenvBasic = mockWenv () & L.inputStatus . L.mousePos .~ Point 0 0
    wenvFocus = wenvBasic & L.focusedPath .~ Seq.fromList [0]
    wenvHover = mockWenv () & L.inputStatus . L.mousePos .~ Point 200 200
    wenvHoverFocus = wenvHover
      & L.inputStatus . L.mousePos .~ Point 200 200
      & L.focusedPath .~ Seq.fromList [0]
    nodeNormal = createNode True
    nodeDisabled = createNode False

testHandleSizeChange :: Spec
testHandleSizeChange = describe "handleSizeChange" $ do
  it "should request Resize widgets if sizeReq changed" $ do
    resHover ^? _Just . L.requests `shouldSatisfy` (==4) . maybeLength
    resHover ^? _Just . L.requests . ix 0 `shouldSatisfy` isMResizeWidgets
    resHover ^? _Just . L.requests . ix 1 `shouldSatisfy` isMRenderOnce
    resHover ^? _Just . L.requests . ix 2 `shouldSatisfy` isMSetCursorIcon
    resHover ^? _Just . L.requests . ix 3 `shouldSatisfy` isMRenderOnce

  it "should not request Resize widgets if sizeReq has not changed" $
    resFocus ^? _Just . L.requests `shouldSatisfy` (==0) . maybeLength

  where
    wenv = mockWenv ()
    style = createStyle
      & L.hover ?~ padding 10
      & L.hover . non def . L.cursorIcon ?~ CursorHand
    hoverStyle = style ^?! L.hover . _Just
    focusStyle = style ^?! L.focus . _Just
    baseNode = createNode True
      & L.info . L.style .~ style
    node = nodeInit wenv baseNode
    modNode = node & L.info . L.sizeReqW .~ FixedSize 100
    res1 = Just $ WidgetResult modNode Empty Empty
    res2 = Just $ WidgetResult node Empty Empty
    point = Point 200 200
    path = Seq.fromList [0]
    wenvHover = mockWenv () & L.inputStatus . L.mousePos .~ point
    wenvFocus = mockWenv () & L.focusedPath .~ path
    evEnter = Enter point
    resHover = handleStyleChange wenvHover path hoverStyle def node evEnter res1
    resFocus = handleStyleChange wenvFocus path focusStyle def node Focus res2

isMResizeWidgets :: Maybe (WidgetRequest s) -> Bool
isMResizeWidgets (Just ResizeWidgets) = True
isMResizeWidgets _ = False

isMRenderOnce :: Maybe (WidgetRequest s) -> Bool
isMRenderOnce (Just RenderOnce{}) = True
isMRenderOnce _ = False

isMSetCursorIcon :: Maybe (WidgetRequest s) -> Bool
isMSetCursorIcon (Just SetCursorIcon{}) = True
isMSetCursorIcon _ = False

maybeLength :: Maybe (Seq a) -> Int
maybeLength Nothing = 0
maybeLength (Just s) = Seq.length s

createStyle :: Style
createStyle = newStyle where
  basic = createStyleState 10 white
  hover = createStyleState 20 green
  focus = createStyleState 30 blue
  disabled = createStyleState 40 gray
  newStyle = Style basic hover focus disabled

createStyleState :: Double -> Color -> Maybe StyleState
createStyleState size col = Just newState where
  newState = textSize size <> bgColor col

createNode :: Bool -> WidgetNode s e
createNode enabled = newNode where
  viewport = Rect 100 100 200 200
  newNode = label "Test"
    & L.info . L.path .~ Seq.fromList [0]
    & L.info . L.viewport .~ viewport
    & L.info . L.renderArea .~ viewport
    & L.info . L.style .~ createStyle
    & L.info . L.visible .~ True
    & L.info . L.enabled .~ enabled
    & L.info . L.focusable .~ True
