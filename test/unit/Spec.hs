-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec

import qualified Monomer.Widget.Widgets.ButtonSpec as ButtonSpec
import qualified Monomer.Widget.Widgets.GridSpec as GridSpec
import qualified Monomer.Widget.Widgets.LabelSpec as LabelSpec
import qualified Monomer.Widget.Widgets.StackSpec as StackSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  ButtonSpec.spec
  GridSpec.spec
  LabelSpec.spec
  StackSpec.spec
