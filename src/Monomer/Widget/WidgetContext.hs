module Monomer.Widget.WidgetContext (
  module Monomer.Common.Tree,
  module Monomer.Widget.WidgetContext
) where

import Data.Sequence (Seq, (|>))

import qualified Data.Sequence as Seq

import Monomer.Common.Tree (Path, PathStep)

data WidgetContext = WidgetContext {
  _wcFocusedPath :: Path,
  _wcTargetPath :: Path
} deriving (Show, Eq)

rootPath :: Path
rootPath = Seq.empty
