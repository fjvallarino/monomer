{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Draggable (
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

type DraggableRender s e = Renderer -> WidgetEnv s e -> WidgetNode s e -> IO ()

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

draggableStyle :: [StyleState] -> DraggableCfg s e
draggableStyle styles = def {
  _dgcDragStyle = Just (mconcat styles)
}

draggableRender :: DraggableRender s e -> DraggableCfg s e
draggableRender render = def {
  _dgcCustomRender = Just render
}

draggable :: DragMsg a => a -> WidgetNode s e -> WidgetNode s e
draggable msg managed = draggable_ msg managed def

draggable_
  :: DragMsg a
  => a
  -> WidgetNode s e
  -> [DraggableCfg s e]
  -> WidgetNode s e
draggable_ msg managed configs = makeNode widget managed where
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

  handleEvent wenv target evt node = case evt of
    ButtonAction p btn PressedBtn 1 -> Just result where
      result = resultReqs node [StartDrag wid path dragMsg]
    ButtonAction p btn ReleasedBtn _ -> Just result where
      result = resultReqs node [CancelDrag wid]
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

  defaultRender renderer wenv node =
    drawStyledAction renderer draggedRect style $ \_ ->
      drawInTranslation renderer offset $
        drawInScale renderer (Point scale scale) $
          drawInAlpha renderer transparency $
            widgetRender (cnode ^. L.widget) renderer wenv cnode
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

  render renderer wenv node = do
    when dragged $
      createOverlay renderer $ do
        renderAction renderer wenv node
    where
      dragged = isNodeDragged wenv node
      renderAction = fromMaybe defaultRender (_dgcCustomRender config)
