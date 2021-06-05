{-|
Module      : Monomer.Widgets.Singles.ExternalLinkSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for ExternalLink widget.
-}
module Monomer.Widgets.Singles.ExternalLinkSpec (spec) where

import Data.Default
import Test.Hspec

import qualified Data.Sequence as Seq
import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestEventUtil
import Monomer.TestUtil
import Monomer.Widgets.Singles.ExternalLink

data TestEvent
  = GotFocus Path
  | LostFocus Path
  deriving (Eq, Show)

spec :: Spec
spec = describe "ExternalLink" $ do
  handleEvent
  getSizeReq

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should not generate an event if clicked outside" $
    clickReqs (Point 3000 3000) `shouldBe` Seq.empty

  it "should generate a widget task when clicked" $
    Seq.index (clickReqs (Point 100 100)) 0 `shouldSatisfy` isRunTask

  it "should generate a user provided event when Enter/Space is pressed" $
    Seq.index (keyReqs keyReturn) 0 `shouldSatisfy` isRunTask

  it "should generate an event when focus is received" $
    events evtFocus `shouldBe` Seq.singleton (GotFocus emptyPath)

  it "should generate an event when focus is lost" $
    events evtBlur `shouldBe` Seq.singleton (LostFocus emptyPath)

  where
    wenv = mockWenv ()
    btnNode = externalLink_ "Link" "Url" [onFocus GotFocus, onBlur LostFocus]
    clickReqs p = nodeHandleEventReqs wenv [Click p BtnLeft] btnNode
    keyReqs key = nodeHandleEventReqs wenv [KeyAction def key KeyPressed] btnNode
    events evt = nodeHandleEventEvts wenv [evt] btnNode

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return width = Fixed 40" $
    sizeReqW `shouldBe` fixedSize 40

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` fixedSize 20

  where
    wenv = mockWenvEvtUnit ()
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv (externalLink "Link" "Url")
