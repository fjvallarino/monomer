{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Util.StyleSpec (spec) where

import Control.Lens ((&), (^.), (^?), (^?!), (.~), (?~), _Just, ix, non)
import Data.Default
import Data.Sequence (Seq)
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
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
    activeStyle wenvBasic instNormal ^. L.bgColor `shouldBe` Just white

  it "should return hover style" $
    activeStyle wenvHover instNormal ^. L.bgColor `shouldBe` Just green

  it "should return focus style" $ do
    activeStyle wenvFocus instNormal ^. L.bgColor `shouldBe` Just blue
    activeStyle wenvHoverFocus instNormal ^. L.bgColor `shouldBe` Just blue

  it "should return disabled style" $ do
    activeStyle wenvBasic instDisabled ^. L.bgColor `shouldBe` Just gray
    activeStyle wenvHover instDisabled ^. L.bgColor `shouldBe` Just gray
    activeStyle wenvFocus instDisabled ^. L.bgColor `shouldBe` Just gray
    activeStyle wenvHoverFocus instDisabled ^. L.bgColor `shouldBe` Just gray

  where
    wenvBasic = mockWenv () & L.inputStatus . L.mousePos .~ Point 0 0
    wenvFocus = wenvBasic & L.focusedPath .~ Seq.fromList [0]
    wenvHover = mockWenv () & L.inputStatus . L.mousePos .~ Point 200 200
    wenvHoverFocus = wenvHover
      & L.inputStatus . L.mousePos .~ Point 200 200
      & L.focusedPath .~ Seq.fromList [0]
    instNormal = createInst True
    instDisabled = createInst False

testHandleSizeChange :: Spec
testHandleSizeChange = describe "handleSizeChange" $ do
  it "should request Resize widgets if sizeReq changed" $ do
    resHover ^? _Just . L.requests . ix 0 `shouldSatisfy` isResizeWidgets
    resHover ^? _Just . L.requests `shouldSatisfy` (==1) . maybeLength

  it "should not request Resize widgets if sizeReq has not changed" $
    resFocus ^? _Just . L.requests `shouldSatisfy` (==0) . maybeLength

  where
    wenv = mockWenv ()
    style = createStyle
      & L.hover ?~ padding 10
    hoverStyle = style ^?! L.hover . _Just
    focusStyle = style ^?! L.focus . _Just
    baseInst = createInst True & L.style .~ style
    inst = instInit wenv baseInst
    point = Point 200 200
    path = Seq.fromList [0]
    wenvHover = mockWenv () & L.inputStatus . L.mousePos .~ point
    wenvFocus = mockWenv () & L.focusedPath .~ path
    evtEnter = Enter path point
    resHover = handleStyleChange wenvHover path evtEnter hoverStyle Nothing inst
    resFocus = handleStyleChange wenvFocus path Focus focusStyle Nothing inst

isResizeWidgets :: Maybe (WidgetRequest s) -> Bool
isResizeWidgets (Just ResizeWidgets) = True
isResizeWidgets _ = False

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

createInst :: Bool -> WidgetInstance s e
createInst enabled = newInst where
  viewport = Rect 100 100 200 200
  newInst = (label "Test") {
    _wiPath = Seq.fromList [0],
    _wiViewport = viewport,
    _wiRenderArea = viewport,
    _wiStyle = createStyle,
    _wiVisible = True,
    _wiEnabled = enabled,
    _wiFocusable = True
  }
