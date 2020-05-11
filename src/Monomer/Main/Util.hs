module Monomer.Main.Util where

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
  _focusRing = [],
  _latestHover = Nothing,
  _userTasks = [],
  _widgetTasks = []
}
