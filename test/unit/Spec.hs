-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec

import qualified SDL
import qualified SDL.Raw as Raw

import qualified Monomer.Common.CursorIconSpec as CursorIconSpec
import qualified Monomer.Common.PersistSpec as PersistSpec

import qualified Monomer.Widgets.AlertSpec as AlertSpec
import qualified Monomer.Widgets.BoxSpec as BoxSpec
import qualified Monomer.Widgets.ButtonSpec as ButtonSpec
import qualified Monomer.Widgets.CheckboxSpec as CheckboxSpec
import qualified Monomer.Widgets.CompositeSpec as CompositeSpec
import qualified Monomer.Widgets.ContainerSpec as ContainerSpec
import qualified Monomer.Widgets.ConfirmSpec as ConfirmSpec
import qualified Monomer.Widgets.DialSpec as DialSpec
import qualified Monomer.Widgets.DropdownSpec as DropdownSpec
import qualified Monomer.Widgets.DragDropSpec as DragDropSpec
import qualified Monomer.Widgets.GridSpec as GridSpec
import qualified Monomer.Widgets.ImageSpec as ImageSpec
import qualified Monomer.Widgets.KeystrokeSpec as KeystrokeSpec
import qualified Monomer.Widgets.LabelSpec as LabelSpec
import qualified Monomer.Widgets.ListViewSpec as ListViewSpec
import qualified Monomer.Widgets.NumericFieldSpec as NumericFieldSpec
import qualified Monomer.Widgets.RadioSpec as RadioSpec
import qualified Monomer.Widgets.ScrollSpec as ScrollSpec
import qualified Monomer.Widgets.SpacerSpec as SpacerSpec
import qualified Monomer.Widgets.SplitSpec as SplitSpec
import qualified Monomer.Widgets.StackSpec as StackSpec
import qualified Monomer.Widgets.TextFieldSpec as TextFieldSpec
import qualified Monomer.Widgets.ThemeSwitchSpec as ThemeSwitchSpec
import qualified Monomer.Widgets.TooltipSpec as TooltipSpec
import qualified Monomer.Widgets.ZStackSpec as ZStackSpec

import qualified Monomer.Widgets.Util.FocusSpec as FocusSpec
import qualified Monomer.Widgets.Util.StyleSpec as StyleSpec
import qualified Monomer.Widgets.Util.TextSpec as TextSpec

main :: IO ()
main = do
  -- Initialize SDL
  SDL.initialize [SDL.InitVideo]
  -- Run tests
  hspec spec
  -- Shutdown SDL
  Raw.quitSubSystem Raw.SDL_INIT_VIDEO
  SDL.quit

spec :: Spec
spec = do
  common
  widgets
  widgetsUtil

common :: Spec
common = describe "Common" $ do
  CursorIconSpec.spec
  PersistSpec.spec

widgets :: Spec
widgets = describe "Widgets" $ do
  AlertSpec.spec
  BoxSpec.spec
  ButtonSpec.spec
  CheckboxSpec.spec
  CompositeSpec.spec
  ContainerSpec.spec
  ConfirmSpec.spec
  DialSpec.spec
  DropdownSpec.spec
  DragDropSpec.spec
  GridSpec.spec
  ImageSpec.spec
  KeystrokeSpec.spec
  LabelSpec.spec
  ListViewSpec.spec
  NumericFieldSpec.spec
  RadioSpec.spec
  ScrollSpec.spec
  SpacerSpec.spec
  SplitSpec.spec
  StackSpec.spec
  ThemeSwitchSpec.spec
  TextFieldSpec.spec
  TooltipSpec.spec
  ZStackSpec.spec

widgetsUtil :: Spec
widgetsUtil = describe "Widgets Util" $ do
  FocusSpec.spec
  StyleSpec.spec
  TextSpec.spec
