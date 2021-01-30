{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.DropTarget (
  dropTarget,
  dropTarget_
) where

import Debug.Trace

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~), (%~), at)
import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Char (chr, isAscii, isPrint, ord)
import Data.Default
import Data.List (foldl')
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Data.Typeable (Typeable, cast)

import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T

import Monomer.Graphics.ColorTable
import Monomer.Widgets.Container

import qualified Monomer.Lens as L

newtype DropTargetCfg = DropTargetCfg {
  _dgcIgnoreChildren :: Maybe Bool
}

instance Default DropTargetCfg where
  def = DropTargetCfg {
    _dgcIgnoreChildren = Nothing
  }

instance Semigroup DropTargetCfg where
  (<>) t1 t2 = DropTargetCfg {
    _dgcIgnoreChildren = _dgcIgnoreChildren t2 <|> _dgcIgnoreChildren t1
  }

instance Monoid DropTargetCfg where
  mempty = def

instance CmbIgnoreChildrenEvts DropTargetCfg where
  ignoreChildrenEvts = def {
    _dgcIgnoreChildren = Just True
  }

dropTarget :: DragMsg a => (a -> e) -> WidgetNode s e -> WidgetNode s e
dropTarget dropEvt managed = dropTarget_ dropEvt managed def

dropTarget_
  :: DragMsg a
  => (a -> e)
  -> WidgetNode s e
  -> [DropTargetCfg]
  -> WidgetNode s e
dropTarget_ dropEvt managed configs = makeNode widget managed where
  config = mconcat configs
  widget = makeDropTarget dropEvt config

makeNode :: Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode  widget managedWidget = defaultWidgetNode "dropTarget" widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makeDropTarget :: DragMsg a => (a -> e) -> DropTargetCfg -> Widget s e
makeDropTarget dropEvt config = widget where
  widget = createContainer () def {
    containerHandleEvent = handleEvent,
    containerGetSizeReq = getSizeReq,
    containerResize = resize,
    containerRender = render
  }

  msgToEvts (WidgetDragMsg dragMsg) = case cast dragMsg of
    Just msg -> [dropEvt msg]
    _ -> []

  handleEvent wenv target evt node = case evt of
    Drop point path dragMsg
      | not (isNodeParentOfPath path node) -> Just result where
        widgetId = node ^. L.info . L.widgetId
        reqs = [AcceptDrop widgetId]
        evts = msgToEvts dragMsg
        result = resultReqsEvts node reqs evts
    _ -> Nothing

  getSizeReq :: ContainerGetSizeReqHandler s e a
  getSizeReq wenv currState node children = (newReqW, newReqH) where
    child = Seq.index children 0
    newReqW = child ^. L.info . L.sizeReqW
    newReqH = child ^. L.info . L.sizeReqH

  resize :: ContainerResizeHandler s e
  resize wenv viewport renderArea children node = resized where
    style = activeStyle wenv node
    contentArea = fromMaybe def (removeOuterBounds style renderArea)
    resized = (resultWidget node, Seq.singleton (contentArea, contentArea))

  render renderer wenv node = do
    when (isDropTarget wenv node && isHovered) $
      drawRect renderer viewport (Just blue) Nothing
    where
      viewport = node ^. L.info . L.viewport
      mousePos = wenv ^. L.inputStatus . L.mousePos
      isHovered = isPointInNodeVp mousePos node

  isDropTarget wenv node = case wenv ^. L.dragStatus of
    Just (path, msg) -> not (isNodeParentOfPath path node) && isValidMsg msg
    _ -> False

  isValidMsg = not . null . msgToEvts
