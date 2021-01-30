{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Draggable (
  draggable,
  draggable_
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

import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T

import Monomer.Graphics.ColorTable
import Monomer.Widgets.Container

import qualified Monomer.Lens as L

newtype DraggableCfg = DraggableCfg {
  _dgcIgnoreChildren :: Maybe Bool
}

instance Default DraggableCfg where
  def = DraggableCfg {
    _dgcIgnoreChildren = Nothing
  }

instance Semigroup DraggableCfg where
  (<>) t1 t2 = DraggableCfg {
    _dgcIgnoreChildren = _dgcIgnoreChildren t2 <|> _dgcIgnoreChildren t1
  }

instance Monoid DraggableCfg where
  mempty = def

instance CmbIgnoreChildrenEvts DraggableCfg where
  ignoreChildrenEvts = def {
    _dgcIgnoreChildren = Just True
  }

draggable :: DragMsg a => a -> WidgetNode s e -> WidgetNode s e
draggable msg managed = draggable_ msg managed def

draggable_
  :: DragMsg a
  => a
  -> WidgetNode s e
  -> [DraggableCfg]
  -> WidgetNode s e
draggable_ msg managed configs = makeNode widget managed where
  config = mconcat configs
  widget = makeDraggable msg config

makeNode :: Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode widget managedWidget = defaultWidgetNode "draggable" widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makeDraggable :: DragMsg a => a -> DraggableCfg -> Widget s e
makeDraggable msg config = widget where
  widget = createContainer () def {
    containerHandleEvent = handleEvent,
    containerGetSizeReq = getSizeReq,
    containerResize = resize,
    containerRender = render
  }

  handleEvent wenv target evt node = case evt of
    ButtonAction p btn PressedBtn 1 -> Just result where
      result = resultReqs node [StartDrag wid path dragMsg]
    ButtonAction p btn ReleasedBtn _ -> Just result where
      result = trace "Cancelled" resultReqs node [CancelDrag wid]
    DragFinished accepted -> traceShow ("accepted", accepted) Nothing
    _ -> Nothing
    where
      wid = node ^. L.info . L.widgetId
      path = node ^. L.info . L.path
      dragMsg = WidgetDragMsg msg

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
    when dragged $
      createOverlay renderer $ do
        drawRect renderer rect (Just red) Nothing
    where
      dragged = isNodeDragged wenv node
      Point mx my = wenv ^. L.inputStatus . L.mousePos
      rect = Rect mx my 20 20
