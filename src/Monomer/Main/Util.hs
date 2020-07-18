{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Util where

import Data.Maybe

import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Event.Util
import Monomer.Main.Types
import Monomer.Widget.PathContext
import Monomer.Widget.Types

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
findNextFocusable wenv currentFocus widgetRoot = fromMaybe rootFocus candidateFocus where
  ctxFocus = PathContext currentFocus currentFocus rootPath
  candidateFocus = _widgetNextFocusable (_instanceWidget widgetRoot) wenv ctxFocus widgetRoot
  ctxRootFocus = PathContext rootPath rootPath rootPath
  rootFocus = fromMaybe currentFocus $ _widgetNextFocusable (_instanceWidget widgetRoot) wenv ctxRootFocus widgetRoot

resizeWidget :: WidgetEnv s e -> Size -> WidgetInstance s e -> WidgetInstance s e
resizeWidget wenv windowSize widgetRoot = newWidgetRoot where
  Size w h = windowSize
  assignedRect = Rect 0 0 w h
  widget = _instanceWidget widgetRoot
  preferredSizes = _widgetPreferredSize widget wenv widgetRoot
  newWidgetRoot = _widgetResize widget wenv assignedRect assignedRect widgetRoot preferredSizes
