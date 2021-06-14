{-|
Module      : Monomer.Widgets.Singles.SeparatorLineSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for Separator Line widget.
-}
module Monomer.Widgets.Singles.SeparatorLineSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Core.Themes.SampleThemes
import Monomer.TestUtil
import Monomer.Widgets.Containers.Box
import Monomer.Widgets.Containers.Grid
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.SeparatorLine

import qualified Monomer.Lens as L

spec :: Spec
spec = describe "SeparatorLine" $ do
  separatorLineSizeReqBox
  separatorLineSizeReqGrid
  separatorLineSizeReqStack

separatorLineSizeReqBox :: Spec
separatorLineSizeReqBox = describe "separatorLineSizeReqBox" $ do
  it "should return (Fixed 1, Fixed 1)" $ do
    sizeReqW1 `shouldBe` fixedSize 1
    sizeReqH1 `shouldBe` fixedSize 1

  where
    wenv = mockWenvEvtUnit ()
    (sizeReqW1, sizeReqH1) = nodeGetSizeReq wenv (box separatorLine)

separatorLineSizeReqGrid :: Spec
separatorLineSizeReqGrid = describe "separatorLineSizeReqGrid" $ do
  it "should return (Fixed 1, Flex 10 0.5) for horizontal" $ do
    sizeReqW1 `shouldBe` fixedSize 1
    sizeReqH1 `shouldBe` flexSize 10 0.5

  it "should return (Flex 10 0.5, Fixed 1) for vertical" $ do
    sizeReqW2 `shouldBe` flexSize 10 0.5
    sizeReqH2 `shouldBe` fixedSize 1

  where
    wenv = mockWenvEvtUnit ()
      & L.theme .~ darkTheme
    (sizeReqW1, sizeReqH1) = nodeGetSizeReq wenv (hgrid [separatorLine])
    (sizeReqW2, sizeReqH2) = nodeGetSizeReq wenv (vgrid [separatorLine])

separatorLineSizeReqStack :: Spec
separatorLineSizeReqStack = describe "separatorLineSizeReqStack" $ do
  it "should return (Fixed 5, Flex 10 0.5) for horizontal" $ do
    sizeReqW1 `shouldBe` fixedSize 5
    sizeReqH1 `shouldBe` flexSize 10 0.5

  it "should return (Flex 10 0.5, Fixed 5) for vertical" $ do
    sizeReqW2 `shouldBe` flexSize 10 0.5
    sizeReqH2 `shouldBe` fixedSize 5

  where
    wenv = mockWenv ()
    (sizeReqW1, sizeReqH1) = nodeGetSizeReq wenv (hstack [separatorLine_ [width 5]])
    (sizeReqW2, sizeReqH2) = nodeGetSizeReq wenv (vstack [separatorLine_ [width 5]])
