{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Util where

import Control.Applicative ((<|>))
import Data.Default
import Data.Maybe

import qualified Data.Sequence as Seq
import qualified Data.Map as Map

import Monomer.Core
import Monomer.Event
import Monomer.Main.Types

defaultWindowSize :: (Int, Int)
defaultWindowSize = (640, 480)

defaultUseHdpi :: Bool
defaultUseHdpi = True

initMonomerContext :: s -> Size -> Bool -> Double -> MonomerContext s
initMonomerContext model winSize useHiDPI devicePixelRate = MonomerContext {
  _mcMainModel = model,
  _mcWindowSize = winSize,
  _mcHdpi = useHiDPI,
  _mcDpr = devicePixelRate,
  _mcInputStatus = def,
  _mcCurrentCursor = CursorArrow,
  _mcPathFocus = Seq.empty,
  _mcPathHover = Nothing,
  _mcPathPressed = Nothing,
  _mcPathOverlay = Nothing,
  _mcWidgetTasks = Seq.empty,
  _mcCursorIcons = Map.empty
}

findNextFocus
  :: WidgetEnv s e
  -> FocusDirection
  -> Path
  -> Maybe Path
  -> WidgetInstance s e
  -> Path
findNextFocus wenv direction focus overlay widgetRoot = fromJust nextFocus where
  widget = _wiWidget widgetRoot
  restartPath = fromMaybe rootPath overlay
  candidateFocus =
    widgetFindNextFocus widget wenv direction focus widgetRoot
  fromRootFocus =
    widgetFindNextFocus widget wenv direction restartPath widgetRoot
  nextFocus = candidateFocus <|> fromRootFocus <|> Just focus

resizeWidget
  :: WidgetEnv s e -> Size -> WidgetInstance s e -> WidgetInstance s e
resizeWidget wenv windowSize widgetRoot = newRoot where
  Size w h = windowSize
  assigned = Rect 0 0 w h
  instReqs = widgetUpdateSizeReq (_wiWidget widgetRoot) wenv widgetRoot
  newRoot = widgetResize (_wiWidget instReqs) wenv assigned assigned instReqs

getTargetPath
  :: WidgetEnv s e
  -> Maybe Path
  -> Maybe Path
  -> Path
  -> SystemEvent
  -> WidgetInstance s e
  -> Maybe Path
getTargetPath wenv pressed overlay target event widgetRoot = case event of
    -- Keyboard
    KeyAction{}            -> pathEvent target
    TextInput _            -> pathEvent target
    -- Clipboard
    Clipboard _            -> pathEvent target
    -- Mouse/touch
    ButtonAction point _ _ -> pointEvent point
    Click point _          -> pointEvent point
    WheelScroll point _ _  -> pointEvent point
    Focus                  -> pathEvent target
    Blur                   -> pathEvent target
    Enter newPath _        -> pathEvent newPath
    Move point             -> pointEvent point
    Leave oldPath _        -> pathEvent oldPath
  where
    widget = _wiWidget widgetRoot
    startPath = fromMaybe rootPath overlay
    pathEvent = Just
    pathFromPoint p = widgetFindByPoint widget wenv startPath p widgetRoot
    pointEvent point = pressed <|> pathFromPoint point <|> overlay
