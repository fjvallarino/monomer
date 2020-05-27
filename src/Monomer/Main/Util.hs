module Monomer.Main.Util where

import Data.Maybe

import qualified Data.Sequence as Seq

import Monomer.Common.Tree
import Monomer.Common.Types
import Monomer.Event.Util
import Monomer.Main.Types
import Monomer.Widget.PathContext
import Monomer.Widget.Types

initMonomerContext :: s -> Rect -> Bool -> Double -> MonomerContext s e
initMonomerContext app winSize useHiDPI devicePixelRate = MonomerContext {
  _appContext = app,
  _windowSize = winSize,
  _useHiDPI = useHiDPI,
  _devicePixelRate = devicePixelRate,
  _inputStatus = defInputStatus,
  _focused = Seq.empty,
  _latestHover = Nothing,
  _userTasks = [],
  _widgetTasks = Seq.empty
}

findNextFocusable :: Path -> WidgetInstance s e m -> Path
findNextFocusable currentFocus widgetRoot = fromMaybe rootFocus candidateFocus where
  ctxFocus = PathContext currentFocus currentFocus rootPath
  candidateFocus = _widgetNextFocusable (_instanceWidget widgetRoot) ctxFocus widgetRoot
  ctxRootFocus = PathContext rootPath rootPath rootPath
  rootFocus = fromMaybe currentFocus $ _widgetNextFocusable (_instanceWidget widgetRoot) ctxRootFocus widgetRoot
