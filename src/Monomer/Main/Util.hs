{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Monomer.Main.Util where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.=), (%=), ix, at, non, use, _1)
import Control.Monad.Extra
import Control.Monad.State
import Data.Default
import Data.Maybe
import Safe (headMay)

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
  _mcCursorStack = [],
  _mcFocusedWidgetId = def,
  _mcHoveredWidgetId = Nothing,
  _mcOverlayWidgetId = Nothing,
  _mcDragAction = Nothing,
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

getWidgetIdPath :: (MonomerM s m) => WidgetId -> m Path
getWidgetIdPath widgetId =
  use $ L.widgetPaths . at widgetId . non (widgetId ^. L.path)

setWidgetIdPath :: (MonomerM s m) => WidgetId -> Path -> m ()
setWidgetIdPath widgetId path = L.widgetPaths . at widgetId .= Just path

delWidgetIdPath :: (MonomerM s m) => WidgetId -> m ()
delWidgetIdPath widgetId = L.widgetPaths . at widgetId .= Nothing

getHoveredPath :: (MonomerM s m) => m (Maybe Path)
getHoveredPath = do
  hoveredWidgetId <- use L.hoveredWidgetId
  case hoveredWidgetId of
    Just wid -> Just <$> getWidgetIdPath wid
    Nothing -> return Nothing

getFocusedPath :: (MonomerM s m) => m Path
getFocusedPath = getWidgetIdPath =<< use L.focusedWidgetId

getOverlayPath :: (MonomerM s m) => m (Maybe Path)
getOverlayPath = do
  overlayWidgetId <- use L.overlayWidgetId
  case overlayWidgetId of
    Just wid -> Just <$> getWidgetIdPath wid
    Nothing -> return Nothing

getDraggedMsgInfo :: (MonomerM s m) => m (Maybe (Path, WidgetDragMsg))
getDraggedMsgInfo = do
  dragAction <- use L.dragAction
  case dragAction of
    Just (DragAction wid msg) -> Just . (, msg) <$> getWidgetIdPath wid
    Nothing -> return Nothing

getCurrentCursor :: (MonomerM s m) => m (Maybe (Path, CursorIcon))
getCurrentCursor = do
  cursorHead <- fmap headMay (use L.cursorStack)
  case cursorHead of
    Just (wid, icon) -> do
      path <- getWidgetIdPath wid
      return $ Just (path, icon)
    otherwhise -> return Nothing
