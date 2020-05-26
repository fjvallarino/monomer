module Monomer.Main.Util where

import qualified Data.Sequence as Seq

import Monomer.Common.Types
import Monomer.Event.Util
import Monomer.Main.Types

initMonomerContext :: s -> Rect -> Bool -> Double -> MonomerContext s e
initMonomerContext app winSize useHiDPI devicePixelRate = MonomerContext {
  _appContext = app,
  _windowSize = winSize,
  _useHiDPI = useHiDPI,
  _devicePixelRate = devicePixelRate,
  _inputStatus = defInputStatus,
  _focusRing = Seq.empty,
  _latestHover = Nothing,
  _userTasks = [],
  _widgetTasks = Seq.empty
}
