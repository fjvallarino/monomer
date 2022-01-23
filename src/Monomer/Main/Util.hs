{-|
Module      : Monomer.Main.Util
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions for the Main module.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Strict #-}

module Monomer.Main.Util where

import Control.Applicative ((<|>))
import Control.Concurrent.STM.TChan
import Control.Lens ((&), (^.), (.=), (%=), ix, at, non, use, _1)
import Control.Monad.Extra
import Control.Monad.State
import Data.Default
import Data.Maybe

import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified SDL

import Monomer.Core
import Monomer.Event
import Monomer.Helper (headMay)
import Monomer.Main.Platform
import Monomer.Main.Types
import Monomer.Widgets.Util.Widget

import qualified Monomer.Core.Lens as L
import qualified Monomer.Main.Lens as L

-- | Initializes the Monomer context with the provided information.
initMonomerCtx
  :: SDL.Window
  -> TChan (RenderMsg s e)
  -> Size
  -> Double
  -> Double
  -> s
  -> MonomerCtx s e
initMonomerCtx ~win channel winSize dpr epr model = MonomerCtx {
  _mcMainModel = model,
  _mcWindow = win,
  _mcWindowSize = winSize,
  _mcDpr = dpr,
  _mcEpr = epr,
  _mcRenderChannel = channel,
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
  _mcResizeRequests = Seq.empty,
  _mcRenderRequested = False,
  _mcRenderSchedule = Map.empty,
  _mcExitApplication = False
}

-- | Returns the path of the provided "WidgetId".
getWidgetIdPath :: (MonomerM s e m) => WidgetId -> m Path
getWidgetIdPath widgetId =
  use $ L.widgetPaths . at widgetId . non (widgetId ^. L.path)

-- | Updates the path associated to a "WidgetId".
setWidgetIdPath :: (MonomerM s e m) => WidgetId -> Path -> m ()
setWidgetIdPath widgetId path = L.widgetPaths . at widgetId .= Just path

-- | Removes the association of a path to a "WidgetId".
delWidgetIdPath :: (MonomerM s e m) => WidgetId -> m ()
delWidgetIdPath widgetId = L.widgetPaths . at widgetId .= Nothing

-- | Returns the path of the currently hovered node, if any.
getHoveredPath :: (MonomerM s e m) => m (Maybe Path)
getHoveredPath = do
  hoveredWidgetId <- use L.hoveredWidgetId
  case hoveredWidgetId of
    Just wid -> Just <$> getWidgetIdPath wid
    Nothing -> return Nothing

-- | Returns the path of the currently focused node.
getFocusedPath :: (MonomerM s e m) => m Path
getFocusedPath = getWidgetIdPath =<< use L.focusedWidgetId

-- | Returns the path of the current overlay node, if any.
getOverlayPath :: (MonomerM s e m) => m (Maybe Path)
getOverlayPath = do
  overlayWidgetId <- use L.overlayWidgetId
  case overlayWidgetId of
    Just wid -> Just <$> getWidgetIdPath wid
    Nothing -> return Nothing

-- | Returns the current drag message and path, if any.
getDraggedMsgInfo :: (MonomerM s e m) => m (Maybe (Path, WidgetDragMsg))
getDraggedMsgInfo = do
  dragAction <- use L.dragAction
  case dragAction of
    Just (DragAction wid msg) -> Just . (, msg) <$> getWidgetIdPath wid
    Nothing -> return Nothing

-- | Returns the current cursor and path that set it, if any.
getCurrentCursorIcon :: (MonomerM s e m) => m (Maybe (Path, CursorIcon))
getCurrentCursorIcon = do
  cursorHead <- fmap headMay (use L.cursorStack)
  case cursorHead of
    Just (wid, icon) -> do
      path <- getWidgetIdPath wid
      return $ Just (path, icon)
    otherwhise -> return Nothing
