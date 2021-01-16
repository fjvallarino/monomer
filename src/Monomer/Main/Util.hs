{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Util where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~), (.=), use)
import Control.Monad.Extra
import Control.Monad.State
import Data.Default
import Data.Maybe

import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL

import Monomer.Core
import Monomer.Event
import Monomer.Main.Platform
import Monomer.Main.Types
import Monomer.Widgets.Util.Widget

import qualified Monomer.Lens as L

initMonomerCtx :: s -> SDL.Window -> Size -> Bool -> Double -> MonomerCtx s
initMonomerCtx model win winSize useHiDPI devicePixelRate = MonomerCtx {
  _mcMainModel = model,
  _mcWindow = win,
  _mcWindowSize = winSize,
  _mcHdpi = useHiDPI,
  _mcDpr = devicePixelRate,
  _mcInputStatus = def,
  _mcCurrentCursor = CursorArrow,
  _mcFocusedPath = Seq.empty,
  _mcHoveredPath = Nothing,
  _mcOverlayPath = Nothing,
  _mcMainBtnPress = Nothing,
  _mcWidgetTasks = Seq.empty,
  _mcWidgetPaths = Map.empty,
  _mcCursorIcons = Map.empty,
  _mcLeaveEnterPair = False,
  _mcResizePending = False,
  _mcRenderRequested = False,
  _mcRenderSchedule = Map.empty,
  _mcExitApplication = False
}

findNextFocus
  :: WidgetEnv s e
  -> FocusDirection
  -> Path
  -> Maybe Path
  -> WidgetNode s e
  -> Path
findNextFocus wenv direction focus overlay widgetRoot = fromJust nextFocus where
  widget = widgetRoot ^. L.widget
  restartPath = fromMaybe emptyPath overlay
  candidateFocus =
    widgetFindNextFocus widget wenv direction focus widgetRoot
  fromRootFocus =
    widgetFindNextFocus widget wenv direction restartPath widgetRoot
  nextFocus = candidateFocus <|> fromRootFocus <|> Just focus

resizeRoot
  :: WidgetEnv s e -> Size -> WidgetNode s e -> WidgetResult s e
resizeRoot wenv windowSize widgetRoot = result where
  Size w h = windowSize
  assigned = Rect 0 0 w h
  widget = widgetRoot ^. L.widget
  result = widgetResize widget wenv assigned assigned widgetRoot
