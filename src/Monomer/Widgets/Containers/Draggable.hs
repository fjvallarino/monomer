{-|
Module      : Monomer.Widgets.Containers.Draggable
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Draggable container for a single item. Useful for adding drag support without
having to implement a custom widget. Usually works in tandem with 'DropTarget'.

The regular styling of this component apply only when the item is not being
dragged. To style the dragged container, use draggableStyle.

The transparency config only applies to the inner content.

Config:

- transparency: the alpha level to apply when rendering content in drag mode.
- maxDim: the maximum size of the largest axis when dragging. Keeps proportions.
- draggableStyle: the style to use when the item is being dragged.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Containers.Draggable (
  draggable,
  draggable_,
  draggableStyle,
  draggableRender
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (^?!), (.~), _Just, _1, _2, at, ix)
import Control.Monad (when)
import Data.Default
import Data.Maybe

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

import qualified Monomer.Lens as L

type DraggableRender s e = WidgetEnv s e -> WidgetNode s e -> Renderer -> IO ()

data DraggableCfg s e = DraggableCfg {
  _dgcTransparency :: Maybe Double,
  _dgcMaxDim :: Maybe Double,
  _dgcDragStyle :: Maybe StyleState,
  _dgcCustomRender :: Maybe (DraggableRender s e)
}

instance Default (DraggableCfg s e) where
  def = DraggableCfg {
    _dgcTransparency = Nothing,
    _dgcMaxDim = Nothing,
    _dgcDragStyle = Nothing,
    _dgcCustomRender = Nothing
  }

instance Semigroup (DraggableCfg s e) where
  (<>) t1 t2 = DraggableCfg {
    _dgcTransparency = _dgcTransparency t2 <|> _dgcTransparency t1,
    _dgcMaxDim = _dgcMaxDim t2 <|> _dgcMaxDim t1,
    _dgcDragStyle = _dgcDragStyle t2 <|> _dgcDragStyle t1,
    _dgcCustomRender = _dgcCustomRender t2 <|> _dgcCustomRender t1
  }

instance Monoid (DraggableCfg s e) where
  mempty = def

instance CmbTransparency (DraggableCfg s e) where
  transparency transp = def {
    _dgcTransparency = Just transp
  }

instance CmbMaxDim (DraggableCfg s e) where
  maxDim dim = def {
    _dgcMaxDim = Just dim
  }

-- | The style of the dragged container.
draggableStyle :: [StyleState] -> DraggableCfg s e
draggableStyle styles = def {
  _dgcDragStyle = Just (mconcat styles)
}

draggableRender :: DraggableRender s e -> DraggableCfg s e
draggableRender render = def {
  _dgcCustomRender = Just render
}

-- | Creates a draggable container with a single node as child.
draggable :: DragMsg a => a -> WidgetNode s e -> WidgetNode s e
draggable msg managed = draggable_ msg def managed

-- | Creates a draggable container with a single node as child. Accepts config.
draggable_
  :: DragMsg a
  => a
  -> [DraggableCfg s e]
  -> WidgetNode s e
  -> WidgetNode s e
draggable_ msg configs managed = makeNode widget managed where
  config = mconcat configs
  widget = makeDraggable msg config

makeNode :: Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode widget managedWidget = defaultWidgetNode "draggable" widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makeDraggable :: DragMsg a => a -> DraggableCfg s e -> Widget s e
makeDraggable msg config = widget where
  widget = createContainer () def {
    containerHandleEvent = handleEvent,
    containerGetSizeReq = getSizeReq,
    containerResize = resize,
    containerRender = render
  }

  handleEvent wenv node target evt = case evt of
    ButtonAction p btn PressedBtn 1 -> Just result where
      result = resultReqs node [StartDrag wid path dragMsg]
    ButtonAction p btn ReleasedBtn _ -> Just result where
      result = resultReqs node [StopDrag wid]
    _ -> Nothing
    where
      wid = node ^. L.info . L.widgetId
      path = node ^. L.info . L.path
      dragMsg = WidgetDragMsg msg

  getSizeReq :: ContainerGetSizeReqHandler s e
  getSizeReq wenv node children = (newReqW, newReqH) where
    child = Seq.index children 0
    newReqW = child ^. L.info . L.sizeReqW
    newReqH = child ^. L.info . L.sizeReqH

  resize :: ContainerResizeHandler s e
  resize wenv node viewport children = resized where
    style = activeStyle wenv node
    contentArea = fromMaybe def (removeOuterBounds style viewport)
    resized = (resultNode node, Seq.singleton contentArea)

  defaultRender wenv node renderer =
    drawStyledAction renderer (moveRect scOffset draggedRect) style $ \_ -> do
      saveContext renderer
      setTranslation renderer (addPoint scOffset offset)
      setScale renderer (Point scale scale)
      setGlobalAlpha renderer transparency
      widgetRender (cnode ^. L.widget) wenv cnode renderer
      restoreContext renderer
    where
      style = fromMaybe def (_dgcDragStyle config)
      transparency = fromMaybe 1 (_dgcTransparency config)
      cnode = Seq.index (_wnChildren node) 0
      Rect cx cy cw ch = cnode ^. L.info . L.viewport
      Point mx my = wenv ^. L.inputStatus . L.mousePos
      Point px py = wenv ^?! L.mainBtnPress . _Just . _2
      dim = fromMaybe (max cw ch) (_dgcMaxDim config)
      scale = dim / max cw ch
      offset = Point (mx - px * scale) (my - py * scale)
      -- Background rectangle (using draggable style)
      (dx, dy) = (cx - px, cy - py)
      rect = Rect (mx + dx * scale) (my + dy * scale) (cw * scale) (ch * scale)
      draggedRect = fromMaybe rect (addOuterBounds style rect)
      scOffset = wenv ^. L.offset

  render wenv node renderer = do
    when dragged $
      createOverlay renderer $ do
        renderAction wenv node renderer
    where
      dragged = isNodeDragged wenv node
      renderAction = fromMaybe defaultRender (_dgcCustomRender config)
