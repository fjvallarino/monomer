module Monomer.Widgets.AlertSpec (spec) where

import Debug.Trace

import Control.Lens ((&), (.~))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event
import Monomer.TestUtil
import Monomer.Widgets.Alert

import qualified Monomer.Lens as L

data AlertEvent
  = CloseClick
  deriving (Eq, Show)

spec :: Spec
spec = describe "Alert"
  handleEvent

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  it "should generate a close event if clicked outside the dialog" $
    events (Point 3000 3000) `shouldBe` Seq.singleton CloseClick

  fit "should generate a close event when clicking the Accept button" $
    events (Point 150 350) `shouldBe` Seq.singleton CloseClick

  it "should not generate a close event when clicking the dialog" $
    events (Point 300 300) `shouldBe` Seq.empty

  where
    wenv = mockWenv () & L.theme .~ darkTheme
    alertInst = instInit wenv (alert "Alert!" CloseClick)
    events p = trace (widgetTreeDesc 0 alertInst) $ instHandleEventEvts wenv (Click p LeftBtn) alertInst

widgetTreeDesc :: Int -> WidgetInstance s e -> String
widgetTreeDesc level inst = desc where
  desc = instanceDesc level inst ++ "\n" ++ childDesc
  childDesc = foldMap (widgetTreeDesc (level + 1)) (_wiChildren inst)

instanceDesc :: Int -> WidgetInstance s e -> String
instanceDesc level inst = instDesc inst where
  spaces = replicate (level * 2) ' '
  instDesc i =
    spaces ++ "type: " ++ unWidgetType (_wiWidgetType inst) ++ "\n" ++
    spaces ++ "vp: " ++ rectDesc (_wiViewport inst) ++ "\n" ++
    spaces ++ "req: " ++ show (_wiSizeReqW inst, _wiSizeReqH inst) ++ "\n"
  rectDesc r = show (_rX r, _rY r, _rW r, _rH r)
