{-|
Module      : Monomer.Widgets.Containers.ThemeSwitchSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for ThemeSwitch widget.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widgets.Containers.ThemeSwitchSpec (spec) where

import Control.Lens ((&), (^.), (.~), (?~))
import Data.Default
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestUtil
import Monomer.TestEventUtil
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Containers.ThemeSwitch
import Monomer.Widgets.Singles.Label

import qualified Monomer.Lens as L

spec :: Spec
spec = describe "Theme Switch" $ do
  switchTheme
  getSizeReq

switchTheme :: Spec
switchTheme = describe "switchTheme" $ do
  it "should return different sizes when theme changes" $ do
    sizeReqW1 `shouldBe` sizeReqW2
    sizeReqH1 `shouldBe` sizeReqH2
    sizeReqW1 `shouldNotBe` sizeReqW3
    sizeReqH1 `shouldNotBe` sizeReqH3

  where
    wenv = mockWenvEvtUnit ()
      & L.theme .~ theme1
    theme1 :: Theme = def
    theme2 :: Theme = def
      & L.basic . L.labelStyle . L.padding ?~ padding 10
    node = hstack [
        label "Test",
        label "Test",
        themeSwitch theme2 (label "Test")
      ]
    newNode = nodeInit wenv node
    inst = widgetGetInstanceTree (newNode ^. L.widget) wenv newNode
    child1 = Seq.index (inst ^. L.children) 0
    child2 = Seq.index (inst ^. L.children) 1
    child3 = Seq.index (inst ^. L.children) 2
    childReq ch = (ch ^. L.info . L.sizeReqW, ch ^. L.info . L.sizeReqH)
    (sizeReqW1, sizeReqH1) = childReq child1
    (sizeReqW2, sizeReqH2) = childReq child2
    (sizeReqW3, sizeReqH3) = childReq child3

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
    (tSizeReqW, tSizeReqH) = nodeGetSizeReq wenv (themeSwitch def lblNode)
