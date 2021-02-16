module Monomer.Widgets.Util.Types where

import Monomer.Core.StyleTypes
import Monomer.Core.WidgetTypes

type IsHovered s e = WidgetEnv s e -> WidgetNode s e -> Bool
type IsFocused s e = WidgetEnv s e -> WidgetNode s e -> Bool
type IsActive s e = WidgetEnv s e -> WidgetNode s e -> Bool

type GetBaseStyle s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> Maybe Style

data ActiveStyleCfg s e = ActiveStyleCfg {
  _ascIsHovered :: IsHovered s e,
  _ascIsFocused :: IsFocused s e,
  _ascIsActive :: IsActive s e
}
