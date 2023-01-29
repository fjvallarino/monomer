{-|
Module      : Monomer.Widgets.Containers.DropTarget
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Drop target container for a single element. Useful for adding drag support
without having to implement a custom widget. Usually works in tandem with
"Monomer.Widgets.Containers.Draggable".

Raises a user provided event when an item is dropped. The type must match with
the type of the dragged widget message, otherwise it will not be raised.

@
target = dropTarget ItemDropped $
  vstack itemsRows
    \`styleBasic\` [width 200, height 400]
@

See Tutorial 6 (Composite) for a usage example.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Containers.DropTarget (
  -- * Configuration
  DropTargetCfg,
  dropTargetStyle,
  -- * Constructors
  dropTarget,
  dropTarget_
) where

import Control.Lens ((&), (^.), (.~))
import Control.Monad (when)
import Data.Default
import Data.Maybe
import Data.Typeable (cast)

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

import qualified Monomer.Lens as L

{-|
Configuration options for dropTarget:

- 'dropTargetStyle': The style to apply to the container when a dragged item is
  on top.
-}
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
  :: (DragMsg a, WidgetEvent e)
  => (a -> e)        -- ^ The event to raise on drop.
  -> WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created drop target container.
dropTarget dropEvt managed = dropTarget_ dropEvt def managed

-- | Creates a drop target container with a single node as child. Accepts
--   config.
dropTarget_
  :: (DragMsg a, WidgetEvent e)
  => (a -> e)         -- ^ The event to raise on drop.
  -> [DropTargetCfg]  -- ^ The config options.
  -> WidgetNode s e   -- ^ The child node.
  -> WidgetNode s e   -- ^ The created drop target container.
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
    | isJust style && isDropTarget wenv node && isValid = fromJust style
    | otherwise = currentStyle wenv node
    where
      mousePos = wenv ^. L.inputStatus . L.mousePos
      isHovered = isPointInNodeVp node mousePos
      isTopLevel = isNodeTopLevel wenv node
      isValid = isHovered && isTopLevel
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
