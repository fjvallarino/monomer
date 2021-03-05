module Monomer.Widgets.Animation.Types where

data AnimationMsg
  = AnimationStart
  | AnimationStop
  | AnimationFinished
  deriving (Eq, Show)
