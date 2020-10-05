{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Util where

import Control.Applicative ((<|>))
import Data.Default
import Data.Maybe

import qualified Data.Sequence as Seq

import Monomer.Core.BasicTypes
import Monomer.Core.WidgetTypes
import Monomer.Main.Types

initMonomerContext :: s -> Size -> Bool -> Double -> MonomerContext s
initMonomerContext model winSize useHiDPI devicePixelRate = MonomerContext {
  _mcMainModel = model,
  _mcWindowSize = winSize,
  _mcHdpi = useHiDPI,
  _mcDpr = devicePixelRate,
  _mcInputStatus = def,
  _mcPathFocus = Seq.empty,
  _mcPathHover = Nothing,
  _mcPathPressed = Nothing,
  _mcPathOverlay = Nothing,
  _mcWidgetTasks = Seq.empty
}

findNextFocus
  :: WidgetEnv s e -> FocusDirection -> Path -> WidgetInstance s e -> Path
findNextFocus wenv direction focus widgetRoot = fromJust nextFocus where
  widget = _wiWidget widgetRoot
  candidateFocus = widgetFindNextFocus widget wenv direction focus widgetRoot
  fromRootFocus = widgetFindNextFocus widget wenv direction rootPath widgetRoot
  nextFocus = candidateFocus <|> fromRootFocus <|> Just focus

resizeWidget
  :: WidgetEnv s e -> Size -> WidgetInstance s e -> WidgetInstance s e
resizeWidget wenv windowSize widgetRoot = newRoot where
  Size w h = windowSize
  assigned = Rect 0 0 w h
  instReqs = widgetUpdateSizeReq (_wiWidget widgetRoot) wenv widgetRoot
  newRoot = widgetResize (_wiWidget instReqs) wenv assigned assigned instReqs
