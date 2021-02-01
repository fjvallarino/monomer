{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.DropTarget (
  dropTarget,
  dropTarget_,
  dropTargetStyle
) where

import Control.Lens ((&), (^.), (.~))
import Control.Monad (when)
import Data.Default
import Data.Maybe
import Data.Typeable (Typeable, cast)

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

import qualified Monomer.Lens as L

newtype DropTargetCfg = DropTargetCfg {
  _dtcDragStyle :: Maybe StyleState
}

instance Default DropTargetCfg where
  def = DropTargetCfg {
    _dtcDragStyle = Nothing
  }

instance Semigroup DropTargetCfg where
  (<>) t1 t2 = DropTargetCfg {
    _dtcDragStyle = _dtcDragStyle t1 <> _dtcDragStyle t2
  }

instance Monoid DropTargetCfg where
  mempty = def

dropTargetStyle :: [StyleState] -> DropTargetCfg
dropTargetStyle styles = def {
  _dtcDragStyle = Just (mconcat styles)
}

dropTarget :: DragMsg a => (a -> e) -> WidgetNode s e -> WidgetNode s e
dropTarget dropEvt managed = dropTarget_ dropEvt def managed

dropTarget_
  :: DragMsg a
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

makeDropTarget :: DragMsg a => (a -> e) -> DropTargetCfg -> Widget s e
makeDropTarget dropEvt config = widget where
  widget = createContainer () def {
    containerGetActiveStyle = getActiveStyle,
    containerHandleEvent = handleEvent,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  getActiveStyle wenv node
    | isDropTarget wenv node && isHovered && isJust style = fromJust style
    | otherwise = activeStyle wenv node
    where
      mousePos = wenv ^. L.inputStatus . L.mousePos
      isHovered = isPointInNodeVp mousePos node
      style = _dtcDragStyle config

  handleEvent wenv target evt node = case evt of
    Drop point path dragMsg
      | not (isNodeParentOfPath path node) -> Just result where
        widgetId = node ^. L.info . L.widgetId
        evts = msgToEvts dragMsg
        result = resultEvts node evts
    _ -> Nothing

  getSizeReq :: ContainerGetSizeReqHandler s e a
  getSizeReq wenv currState node children = (newReqW, newReqH) where
    child = Seq.index children 0
    newReqW = child ^. L.info . L.sizeReqW
    newReqH = child ^. L.info . L.sizeReqH

  resize :: ContainerResizeHandler s e
  resize wenv renderArea children node = resized where
    style = activeStyle wenv node
    contentArea = fromMaybe def (removeOuterBounds style renderArea)
    resized = (resultWidget node, Seq.singleton contentArea)

  isDropTarget wenv node = case wenv ^. L.dragStatus of
    Just (path, msg) -> not (isNodeParentOfPath path node) && isValidMsg msg
    _ -> False
    where
      isValidMsg = not . null . msgToEvts

  msgToEvts (WidgetDragMsg dragMsg) = case cast dragMsg of
    Just msg -> [dropEvt msg]
    _ -> []
