{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Util where

import Control.Applicative ((<|>))
import Control.Lens ((.=), use)
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

initMonomerContext :: s -> SDL.Window -> Size -> Bool -> Double -> MonomerContext s
initMonomerContext model win winSize useHiDPI devicePixelRate = MonomerContext {
  _mcMainModel = model,
  _mcWindow = win,
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
  _mcCursorIcons = Map.empty,
  _mcRenderRequested = False,
  _mcRenderSchedule = Map.empty,
  _mcExitApplication = False
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

resizeRoot
  :: WidgetEnv s e -> Size -> WidgetInstance s e -> WidgetInstance s e
resizeRoot wenv windowSize widgetRoot = newRoot where
  Size w h = windowSize
  assigned = Rect 0 0 w h
  newRoot = resizeWidget wenv assigned assigned widgetRoot

resizeWindow
  :: (MonomerM s m)
  => SDL.Window
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> m (WidgetInstance s e)
resizeWindow window wenv widgetRoot = do
  dpr <- use L.dpr
  drawableSize <- getDrawableSize window
  newWindowSize <- getWindowSize window dpr

  let position = GL.Position 0 0
  let size = GL.Size (round $ _sW drawableSize) (round $ _sH drawableSize)

  L.windowSize .= newWindowSize
  liftIO $ GL.viewport GL.$= (position, size)

  return $ resizeRoot wenv newWindowSize widgetRoot

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
