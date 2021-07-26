{-|
Module      : Monomer.Widgets.Util.Types
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Common types for widget related functions.
-}
module Monomer.Widgets.Util.Types (
  IsHovered,
  IsFocused,
  IsActive,
  GetBaseStyle,
  CurrentStyleCfg(..)
) where

import Monomer.Core.StyleTypes
import Monomer.Core.WidgetTypes

-- | Indicates whether the mouse pointer is over a valid region the given node.
type IsHovered s e = WidgetEnv s e -> WidgetNode s e -> Bool
-- | Indicates whether the given node has keyboard focus.
type IsFocused s e = WidgetEnv s e -> WidgetNode s e -> Bool
-- | Indicates whether the given node is clicked on a valid region.
type IsActive s e = WidgetEnv s e -> WidgetNode s e -> Bool

{-|
Returns the base style for a given node, if any. This is widget dependent.

Usually this style comes from the active theme.
-}
type GetBaseStyle s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> Maybe Style

{-|
Configuration for style related functions. It allows to override how each of the
states (hovered, focused and active) is defined for a given widget type.

A usage example can be found in "Monomer.Widgets.Radio".
-}
data CurrentStyleCfg s e = CurrentStyleCfg {
  _ascIsHovered :: IsHovered s e,
  _ascIsFocused :: IsFocused s e,
  _ascIsActive :: IsActive s e
}
