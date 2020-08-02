{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Util where

import Control.Applicative ((<|>))
import Data.Maybe

import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Tree (Path)
import Monomer.Event.Util
import Monomer.Main.Types
import Monomer.Widget.Types
import Monomer.Widget.Util

initMonomerContext :: s -> Size -> Bool -> Double -> MonomerContext s
initMonomerContext model winSize useHiDPI devicePixelRate = MonomerContext {
  _mainModel = model,
  _windowSize = winSize,
  _useHiDPI = useHiDPI,
  _devicePixelRate = devicePixelRate,
  _inputStatus = defInputStatus,
  _focused = Seq.empty,
  _latestHover = Nothing,
  _latestPressed = Nothing,
  _activeOverlay = Nothing,
  _widgetTasks = Seq.empty
}

findNextFocusable :: WidgetEnv s e -> Path -> WidgetInstance s e -> Path
findNextFocusable wenv currentFocus widgetRoot = fromJust nextFocus where
  widget = _wiWidget widgetRoot
  candidateFocus = _widgetNextFocusable widget wenv currentFocus widgetRoot
  fromRootFocus = _widgetNextFocusable widget wenv rootPath widgetRoot
  nextFocus = candidateFocus <|> fromRootFocus <|> Just currentFocus

resizeWidget
  :: WidgetEnv s e -> Size -> WidgetInstance s e -> WidgetInstance s e
resizeWidget wenv windowSize widgetRoot = newRoot where
  Size w h = windowSize
  assigned = Rect 0 0 w h
  widget = _wiWidget widgetRoot
  preferredSize = _widgetPreferredSize widget wenv widgetRoot
  newRoot = _widgetResize widget wenv assigned assigned preferredSize widgetRoot
