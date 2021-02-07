module Monomer.Widgets.ScrollSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Label
import Monomer.Widgets.Scroll

import qualified Monomer.Lens as L

spec :: Spec
spec = describe "Scroll" $ do
  resize
  resizeSmall
  resizeOverlaySmall
  resizeH
  resizeV
  resizeOverlayH
  resizeOverlayV

resize :: Spec
resize = describe "resize" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign all the requested space" $
    childrenVp `shouldBe` Seq.fromList [cvp1]

  where
    wenv = mockWenv () & L.windowSize .~ Size 640 480
    vp   = Rect 0 0   640 480
    cvp1 = Rect 0 0 3000 2000
    scrollNode = scroll (label "" `style` [width 3000, height 2000])
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
    scrollNode = scroll (label "" `style` [width 300, height 200])
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
    scrollNode = scroll_ [scrollOverlay True] (label "" `style` [width 300, height 200])
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
    scrollNode = hscroll (label "" `style` [width 3000, height 2000])
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
    scrollNode = vscroll (label "" `style` [width 3000, height 2000])
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
    scrollNode = hscroll_ [scrollOverlay True] (label "" `style` [width 3000, height 2000])
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
    scrollNode = vscroll_ [scrollOverlay True] (label "" `style` [width 3000, height 2000])
    newNode = nodeInit wenv scrollNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> newNode ^. L.children
