{-|
Module      : Monomer.Widgets
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Widgets module, grouping and re-exporting all the existing widgets.
-}
module Monomer.Widgets (
  -- * Composite widget
  module Monomer.Widgets.Composite,
  -- * Animation
  module Monomer.Widgets.Animation.Fade,
  module Monomer.Widgets.Animation.Slide,
  module Monomer.Widgets.Animation.Types,
  -- * Containers
  module Monomer.Widgets.Containers.Alert,
  module Monomer.Widgets.Containers.Box,
  module Monomer.Widgets.Containers.Confirm,
  module Monomer.Widgets.Containers.Draggable,
  module Monomer.Widgets.Containers.Dropdown,
  module Monomer.Widgets.Containers.DropTarget,
  module Monomer.Widgets.Containers.Grid,
  module Monomer.Widgets.Containers.Keystroke,
  module Monomer.Widgets.Containers.Scroll,
  module Monomer.Widgets.Containers.SelectList,
  module Monomer.Widgets.Containers.Split,
  module Monomer.Widgets.Containers.Stack,
  module Monomer.Widgets.Containers.ThemeSwitch,
  module Monomer.Widgets.Containers.Tooltip,
  module Monomer.Widgets.Containers.ZStack,
  -- * Single widgets
  module Monomer.Widgets.Singles.Button,
  module Monomer.Widgets.Singles.Checkbox,
  module Monomer.Widgets.Singles.ColorPicker,
  module Monomer.Widgets.Singles.DateField,
  module Monomer.Widgets.Singles.Dial,
  module Monomer.Widgets.Singles.ExternalLink,
  module Monomer.Widgets.Singles.Icon,
  module Monomer.Widgets.Singles.Image,
  module Monomer.Widgets.Singles.Label,
  module Monomer.Widgets.Singles.LabeledCheckbox,
  module Monomer.Widgets.Singles.LabeledRadio,
  module Monomer.Widgets.Singles.NumericField,
  module Monomer.Widgets.Singles.OptionButton,
  module Monomer.Widgets.Singles.Radio,
  module Monomer.Widgets.Singles.SeparatorLine,
  module Monomer.Widgets.Singles.Slider,
  module Monomer.Widgets.Singles.Spacer,
  module Monomer.Widgets.Singles.TextArea,
  module Monomer.Widgets.Singles.TextDropdown,
  module Monomer.Widgets.Singles.TextField,
  module Monomer.Widgets.Singles.TimeField,
  module Monomer.Widgets.Singles.ToggleButton
) where

import Monomer.Widgets.Composite

import Monomer.Widgets.Animation.Fade
import Monomer.Widgets.Animation.Slide
import Monomer.Widgets.Animation.Types

import Monomer.Widgets.Containers.Alert
import Monomer.Widgets.Containers.Box
import Monomer.Widgets.Containers.Confirm
import Monomer.Widgets.Containers.Draggable
import Monomer.Widgets.Containers.Dropdown
import Monomer.Widgets.Containers.DropTarget
import Monomer.Widgets.Containers.Grid
import Monomer.Widgets.Containers.Keystroke
import Monomer.Widgets.Containers.Scroll
import Monomer.Widgets.Containers.SelectList
import Monomer.Widgets.Containers.Split
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Containers.ThemeSwitch
import Monomer.Widgets.Containers.Tooltip
import Monomer.Widgets.Containers.ZStack

import Monomer.Widgets.Singles.Button
import Monomer.Widgets.Singles.Checkbox
import Monomer.Widgets.Singles.ColorPicker
import Monomer.Widgets.Singles.DateField
import Monomer.Widgets.Singles.Dial
import Monomer.Widgets.Singles.ExternalLink
import Monomer.Widgets.Singles.Icon
import Monomer.Widgets.Singles.Image
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.LabeledCheckbox
import Monomer.Widgets.Singles.LabeledRadio
import Monomer.Widgets.Singles.NumericField
import Monomer.Widgets.Singles.OptionButton
import Monomer.Widgets.Singles.Radio
import Monomer.Widgets.Singles.SeparatorLine
import Monomer.Widgets.Singles.Slider
import Monomer.Widgets.Singles.Spacer
import Monomer.Widgets.Singles.TextArea
import Monomer.Widgets.Singles.TextDropdown
import Monomer.Widgets.Singles.TextField
import Monomer.Widgets.Singles.TimeField
import Monomer.Widgets.Singles.ToggleButton
