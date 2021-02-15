{-# LANGUAGE LambdaCase #-}

module Monomer.Widgets.Util.Types where

import Data.Default

import Monomer.Core.BasicTypes
import Monomer.Core.StyleTypes
import Monomer.Core.WidgetTypes
import Monomer.Event.Types

type IsHovered s e = WidgetEnv s e -> WidgetNode s e -> Bool

type GetBaseStyle s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> Maybe Style

data StyleChangeCfg = StyleChangeCfg {
  _sccCursorIgnore :: Bool,
  _sccCursorIcon :: Maybe CursorIcon,
  _sccCursorInside :: Point -> Bool,
  _sccCursorEvt :: SystemEvent -> Bool
}

instance Default StyleChangeCfg where {
  def = StyleChangeCfg {
    _sccCursorIgnore = False,
    _sccCursorIcon = Nothing,
    _sccCursorInside = const True,
    _sccCursorEvt = \case
      Enter{} -> True
      _ -> False
  }
}
