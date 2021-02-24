module Monomer.Widgets.LabelSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.Graphics
import Monomer.TestUtil
import Monomer.Widgets.Label

import qualified Monomer.Lens as L

spec :: Spec
spec = describe "Label" $ do
  getSizeReq
  getSizeReqMulti
  getSizeReqMultiKeepSpaces
  getSizeReqMultiMaxLines
  getSizeReqMerge
  resize

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return width = Fixed 100" $
    sizeReqW `shouldBe` FixedSize 100

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` FixedSize 20

  it "should return width = Flex 120 1" $
    sizeReq2W `shouldBe` FlexSize 120 1

  it "should return height = Flex 20 2" $
    sizeReq2H `shouldBe` FlexSize 20 2

  where
    wenv = mockWenv ()
    lblNode = label "Test label"
    lblNode2 = label_ "Test label 2" [resizeFactorW 1, resizeFactorH 2]
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv lblNode
    (sizeReq2W, sizeReq2H) = nodeGetSizeReq wenv lblNode2

getSizeReqMulti :: Spec
getSizeReqMulti = describe "getSizeReq" $ do
  it "should return width = Fixed 50" $
    sizeReqW `shouldBe` FixedSize 50

  it "should return height = Flex 60 0.01" $
    sizeReqH `shouldBe` FlexSize 60 0.01

  where
    wenv = mockWenv ()
    lblNode = label_ "Line    line    line" [textMultiLine] `style` [width 50]
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv lblNode

getSizeReqMultiKeepSpaces :: Spec
getSizeReqMultiKeepSpaces = describe "getSizeReq" $ do
  it "should return width = Max 50 1" $
    sizeReqW `shouldBe` MaxSize 50 1

  it "should return height = Flex 100 0.01" $
    sizeReqH `shouldBe` FlexSize 100 0.01

  where
    wenv = mockWenv ()
    caption = "Line    line    line"
    lblNode = label_ caption [textMultiLine, textKeepSpaces] `style` [maxWidth 50]
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv lblNode

getSizeReqMultiMaxLines :: Spec
getSizeReqMultiMaxLines = describe "getSizeReq" $ do
  it "should return width = Max 50 1" $
    sizeReqW `shouldBe` MaxSize 50 1

  it "should return height = Flex 80 0.01" $
    sizeReqH `shouldBe` FlexSize 80 0.01

  where
    wenv = mockWenv ()
    caption = "Line    line    line    line    line"
    lblNode = label_ caption [textMultiLine, textKeepSpaces, maxLines 4] `style` [maxWidth 50]
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv lblNode

getSizeReqMerge :: Spec
getSizeReqMerge = describe "getSizeReqMerge" $ do
  it "should return width = Fixed 320" $
    sizeReqW `shouldBe` FixedSize 320

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` FixedSize 20

  it "should return width = Fixed 600" $
    sizeReq2W `shouldBe` FixedSize 600

  it "should return height = Fixed 20" $
    sizeReq2H `shouldBe` FixedSize 20

  where
    renderer = mockRenderer {
      computeTextSize = mockTextSize Nothing,
      computeGlyphsPos = mockGlyphsPos Nothing
    }
    wenv = mockWenv ()
      & L.renderer .~ renderer
    lblNode = nodeInit wenv (label "Test Label")
    lblNode2 = label "Test Label" `style` [textSize 60]
    lblRes = widgetMerge (lblNode ^. L.widget) wenv lblNode lblNode2
    WidgetResult lblMerged _ _ = lblRes
    lblInfo = lblNode ^. L.info
    mrgInfo = lblMerged ^. L.info
    (sizeReqW, sizeReqH) = (lblInfo ^. L.sizeReqW, lblInfo ^. L.sizeReqH)
    (sizeReq2W, sizeReq2H) = (mrgInfo ^. L.sizeReqW, mrgInfo ^. L.sizeReqH)

resize :: Spec
resize = describe "resize" $ do
  it "should resize single line in a single step" $
    reqsSingle `shouldBe` Seq.Empty

  it "should resize multi line in two steps" $
    reqsMulti `shouldBe` Seq.singleton ResizeWidgets

  where
    wenv = mockWenv ()
    vp = Rect 0 0 640 480
    single = label "Test label"
    resSingle = widgetResize (single ^. L.widget) wenv vp single
    reqsSingle = resSingle ^. L.requests
    multi = label_ "Test label" [textMultiLine]
    resMulti = widgetResize (multi ^. L.widget) wenv vp multi
    reqsMulti = resMulti ^. L.requests
