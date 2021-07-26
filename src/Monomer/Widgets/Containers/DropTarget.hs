{-|
Module      : Monomer.Widgets.Containers.DropTarget
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Drop target container for a single element. Useful for adding drag support
without having to implement a custom widget. Usually works in tandem with
'Draggable'.

Raises a user provided event when an item is dropped. The type must match with
the dragged message, otherwise it will not be raised.

Configs:

- dropTargetStyle: The style to apply to the container when a dragged item is
on top.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Containers.DropTarget (
  dropTarget,
  dropTarget_,
  dropTargetStyle
) where

import Control.Lens ((&), (^.), (.~))
import Control.Monad (when)
import Data.Default
import Data.Maybe
import Data.Typeable (cast)

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

import qualified Monomer.Lens as L

newtype DropTargetCfg = DropTargetCfg {
  _dtcDropStyle :: Maybe StyleState
}

instance Default DropTargetCfg where
  def = DropTargetCfg {
    _dtcDropStyle = Nothing
  }

instance Semigroup DropTargetCfg where
  (<>) t1 t2 = DropTargetCfg {
    _dtcDropStyle = _dtcDropStyle t1 <> _dtcDropStyle t2
  }

instance Monoid DropTargetCfg where
  mempty = def

-- | The style to apply to the container when a dragged item is on top.
dropTargetStyle :: [StyleState] -> DropTargetCfg
dropTargetStyle styles = def {
  _dtcDropStyle = Just (mconcat styles)
}

-- | Creates a drop target container with a single node as child.
dropTarget
  :: (DragMsg a, WidgetEvent e) => (a -> e) -> WidgetNode s e -> WidgetNode s e
dropTarget dropEvt managed = dropTarget_ dropEvt def managed

-- | Creates a drop target container with a single node as child. Accepts
-- | config.
dropTarget_
  :: (DragMsg a, WidgetEvent e)
  => (a -> e)
  -> [DropTargetCfg]
  -> WidgetNode s e
  -> WidgetNode s e
dropTarget_ dropEvt configs managed = makeNode widget managed where
  config = mconcat configs
  widget = makeDropTarget dropEvt config

makeNode :: Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode  widget managedWidget = defaultWidgetNode "dropTarget" widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makeDropTarget
  :: (DragMsg a, WidgetEvent e) => (a -> e) -> DropTargetCfg -> Widget s e
makeDropTarget dropEvt config = widget where
  widget = createContainer () def {
    containerGetCurrentStyle = getCurrentStyle,
    containerHandleEvent = handleEvent
  }

  getCurrentStyle wenv node
    | isDropTarget wenv node && isHovered && isJust style = fromJust style
    | otherwise = currentStyle wenv node
    where
      mousePos = wenv ^. L.inputStatus . L.mousePos
      isHovered = isPointInNodeVp node mousePos
      style = _dtcDropStyle config

  handleEvent wenv node target evt = case evt of
    Drop point path dragMsg
      | not (isNodeParentOfPath node path) -> Just result where
        widgetId = node ^. L.info . L.widgetId
        evts = msgToEvts dragMsg
        result = resultEvts node evts
    _ -> Nothing

  isDropTarget wenv node = case wenv ^. L.dragStatus of
    Just (path, msg) -> not (isNodeParentOfPath node path) && isValidMsg msg
    _ -> False
    where
      isValidMsg = not . null . msgToEvts

  msgToEvts (WidgetDragMsg dragMsg) = case cast dragMsg of
    Just msg -> [dropEvt msg]
    _ -> []
