-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec

import qualified SDL
import qualified SDL.Raw as Raw

import Monomer.TestUtil (useVideoSubSystem)

import qualified Monomer.Common.CursorIconSpec as CursorIconSpec
import qualified Monomer.Core.SizeReqSpec as SizeReqSpec
import qualified Monomer.Graphics.UtilSpec as GraphicsUtilSpec

import qualified Monomer.Widgets.CompositeSpec as CompositeSpec
import qualified Monomer.Widgets.ContainerSpec as ContainerSpec

import qualified Monomer.Widgets.Animation.FadeSpec as AnimationFadeSpec
import qualified Monomer.Widgets.Animation.SlideSpec as AnimationSlideSpec

import qualified Monomer.Widgets.Containers.AlertSpec as AlertSpec
import qualified Monomer.Widgets.Containers.BoxSpec as BoxSpec
import qualified Monomer.Widgets.Containers.ConfirmSpec as ConfirmSpec
import qualified Monomer.Widgets.Containers.DragDropSpec as DragDropSpec
import qualified Monomer.Widgets.Containers.DropdownSpec as DropdownSpec
import qualified Monomer.Widgets.Containers.GridSpec as GridSpec
import qualified Monomer.Widgets.Containers.KeystrokeSpec as KeystrokeSpec
import qualified Monomer.Widgets.Containers.ScrollSpec as ScrollSpec
import qualified Monomer.Widgets.Containers.SelectListSpec as SelectListSpec
import qualified Monomer.Widgets.Containers.SplitSpec as SplitSpec
import qualified Monomer.Widgets.Containers.StackSpec as StackSpec
import qualified Monomer.Widgets.Containers.ThemeSwitchSpec as ThemeSwitchSpec
import qualified Monomer.Widgets.Containers.TooltipSpec as TooltipSpec
import qualified Monomer.Widgets.Containers.ZStackSpec as ZStackSpec

import qualified Monomer.Widgets.Singles.ButtonSpec as ButtonSpec
import qualified Monomer.Widgets.Singles.CheckboxSpec as CheckboxSpec
import qualified Monomer.Widgets.Singles.ColorPickerSpec as ColorPickerSpec
import qualified Monomer.Widgets.Singles.DateFieldSpec as DateFieldSpec
import qualified Monomer.Widgets.Singles.DialSpec as DialSpec
import qualified Monomer.Widgets.Singles.ExternalLinkSpec as ExternalLinkSpec
import qualified Monomer.Widgets.Singles.ImageSpec as ImageSpec
import qualified Monomer.Widgets.Singles.LabeledCheckboxSpec as LabeledCheckboxSpec
import qualified Monomer.Widgets.Singles.LabeledRadioSpec as LabeledRadioSpec
import qualified Monomer.Widgets.Singles.LabelSpec as LabelSpec
import qualified Monomer.Widgets.Singles.NumericFieldSpec as NumericFieldSpec
import qualified Monomer.Widgets.Singles.OptionButtonSpec as OptionButtonSpec
import qualified Monomer.Widgets.Singles.RadioSpec as RadioSpec
import qualified Monomer.Widgets.Singles.SeparatorLineSpec as SeparatorLineSpec
import qualified Monomer.Widgets.Singles.SliderSpec as SliderSpec
import qualified Monomer.Widgets.Singles.SpacerSpec as SpacerSpec
import qualified Monomer.Widgets.Singles.TextFieldSpec as TextFieldSpec
import qualified Monomer.Widgets.Singles.TextAreaSpec as TextAreaSpec
import qualified Monomer.Widgets.Singles.TimeFieldSpec as TimeFieldSpec
import qualified Monomer.Widgets.Singles.ToggleButtonSpec as ToggleButtonSpec

import qualified Monomer.Widgets.Util.FocusSpec as FocusSpec
import qualified Monomer.Widgets.Util.StyleSpec as StyleSpec
import qualified Monomer.Widgets.Util.TextSpec as TextSpec

main :: IO ()
main = do
  initVideo <- useVideoSubSystem

  if initVideo then do
    -- Initialize SDL
    SDL.initialize [SDL.InitVideo]
    -- Run tests
    hspec spec
    -- Shutdown SDL
    Raw.quitSubSystem Raw.SDL_INIT_VIDEO
    SDL.quit
  else do
    -- Run tests
    hspec spec

spec :: Spec
spec = do
  common
  core
  graphics
  widgets
  widgetsUtil

common :: Spec
common = describe "Common" $ do
  CursorIconSpec.spec

core :: Spec
core = describe "Core" $ do
  SizeReqSpec.spec

graphics :: Spec
graphics = describe "Graphics" $ do
  GraphicsUtilSpec.spec

widgets :: Spec
widgets = describe "Widgets" $ do
  CompositeSpec.spec
  ContainerSpec.spec
  animation
  containers
  singles

animation :: Spec
animation = describe "Animation" $ do
  AnimationFadeSpec.spec
  AnimationSlideSpec.spec

containers :: Spec
containers = describe "Containers" $ do
  AlertSpec.spec
  BoxSpec.spec
  ConfirmSpec.spec
  DragDropSpec.spec
  DropdownSpec.spec
  GridSpec.spec
  KeystrokeSpec.spec
  ScrollSpec.spec
  SelectListSpec.spec
  SplitSpec.spec
  StackSpec.spec
  ThemeSwitchSpec.spec
  TooltipSpec.spec
  ZStackSpec.spec

singles :: Spec
singles = describe "Singles" $ do
  ButtonSpec.spec
  CheckboxSpec.spec
  ColorPickerSpec.spec
  DateFieldSpec.spec
  DialSpec.spec
  ExternalLinkSpec.spec
  ImageSpec.spec
  LabelSpec.spec
  LabeledCheckboxSpec.spec
  LabeledRadioSpec.spec
  NumericFieldSpec.spec
  OptionButtonSpec.spec
  RadioSpec.spec
  SeparatorLineSpec.spec
  SliderSpec.spec
  SpacerSpec.spec
  TextAreaSpec.spec
  TextFieldSpec.spec
  TimeFieldSpec.spec
  ToggleButtonSpec.spec

widgetsUtil :: Spec
widgetsUtil = describe "Widgets Util" $ do
  FocusSpec.spec
  StyleSpec.spec
  TextSpec.spec
