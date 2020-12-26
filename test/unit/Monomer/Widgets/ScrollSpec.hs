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
spec = describe "Scroll"
  resize

resize :: Spec
resize = describe "resize" $ do
  it "should have the provided viewport size" $
    viewport `shouldBe` vp

  it "should assign the same viewport scroll has" $
    childrenVp `shouldBe` Seq.fromList [cvp1]

  it "should assign all the requested space" $
    childrenRa `shouldBe` Seq.fromList [cra1]

  where
    wenv = mockWenv () & L.windowSize .~ Size 640 480
    vp   = Rect   0 0   640 480
    cvp1 = Rect   0 0   640 480
    cra1 = Rect   0 0 3000 2000
    scrollNode = scroll (label "" `style` [width 3000, height 2000])
    newNode = nodeInit wenv scrollNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = roundRectUnits . _wniViewport . _wnInfo <$> newNode ^. L.children
    childrenRa = roundRectUnits . _wniRenderArea . _wnInfo <$> newNode ^. L.children
