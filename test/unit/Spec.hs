-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec

import qualified Monomer.Widget.Widgets.ButtonSpec as ButtonSpec
import qualified Monomer.Widget.Widgets.GridSpec as GridSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  ButtonSpec.spec
  GridSpec.spec
