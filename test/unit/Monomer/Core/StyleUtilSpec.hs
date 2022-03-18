{-|
Module      : Monomer.Core.StyleUtilSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for StyleUtil functions.
-}
module Monomer.Core.StyleUtilSpec (spec) where

import Control.Lens
import Test.Hspec

import Monomer.Core.Combinators
import Monomer.Graphics (Color, rgb)
import Monomer.Widgets.Singles.Spacer

import qualified Monomer.Lens as L

red :: Color
red = rgb 255 0 0

spec :: Spec
spec = describe "StyleUtil" $ do
  styleStateSpec

styleStateSpec :: Spec
styleStateSpec = describe "StyleState actions" $ do
  describe "StyleState merge" $ do
    it "should merge basic styles" $ do
      checkStyleAction styleBasic L.basic mergedStyles

    it "should merge hover styles" $ do
      checkStyleAction styleHover L.hover mergedStyles

    it "should merge focus styles" $ do
      checkStyleAction styleFocus L.focus mergedStyles

    it "should merge hover focus styles" $ do
      checkStyleAction styleFocusHover L.focusHover mergedStyles

    it "should merge active styles" $ do
      checkStyleAction styleActive L.active mergedStyles

    it "should merge disabled styles" $ do
      checkStyleAction styleDisabled L.disabled mergedStyles

  describe "StyleState merge" $ do
    it "should set basic styles" $ do
      checkStyleAction styleBasicSet L.basic newStyles

    it "should set hover styles" $ do
      checkStyleAction styleHoverSet L.hover newStyles

    it "should set focus styles" $ do
      checkStyleAction styleFocusSet L.focus newStyles

    it "should set focus hover styles" $ do
      checkStyleAction styleFocusHoverSet L.focusHover newStyles

    it "should set active styles" $ do
      checkStyleAction styleActiveSet L.active newStyles

    it "should set disabled styles" $ do
      checkStyleAction styleDisabledSet L.disabled newStyles

  where
    baseStyles = [width 200, padding 10]
    newStyles = [padding 5, bgColor red]
    mergedStyles = [width 200, padding 5, bgColor red]

    checkStyleAction styleFn field targetStyles = do
      let node = styleFn spacer baseStyles
      let newNode = styleFn node newStyles

      newNode ^. L.info . L.style . field `shouldBe` Just (mconcat targetStyles)
