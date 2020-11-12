{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.CheckboxSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Checkbox

import qualified Monomer.Lens as L

newtype TestModel = TestModel {
  _tmTestBool :: Bool
} deriving (Eq)

makeLensesWith abbreviatedFields ''TestModel

spec :: Spec
spec = describe "Checkbox" $ do
  handleEvent
  updateSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should not generate an event if clicked outside" $
    clickModel (Point 3000 3000) ^. testBool `shouldBe` False

  it "should generate a user provided event when clicked" $
    clickModel (Point 100 100) ^. testBool `shouldBe` True

  it "should generate a user provided event when Enter/Space is pressed" $
    keyModel keyReturn ^. testBool `shouldBe` True
  where
    wenv = mockWenvEvtUnit (TestModel False)
    chkInst = checkbox testBool
    clickModel p = _weModel wenv2 where
      (wenv2, _, _) = instRunEvent wenv (Click p LeftBtn) chkInst
    keyModel key = _weModel wenv2 where
      (wenv2, _, _) = instRunEvent wenv (KeyAction def key KeyPressed) chkInst

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  it "should return Fixed width = 20" $
    sizeReqW `shouldBe` FixedSize 20

  it "should return Fixed height = 20" $
    sizeReqH `shouldBe` FixedSize 20

  where
    wenv = mockWenvEvtUnit (TestModel False) & L.theme .~ darkTheme
    (sizeReqW, sizeReqH) = instUpdateSizeReq wenv (checkbox testBool)
