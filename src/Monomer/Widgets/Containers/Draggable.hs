{-|
Module      : Monomer.Widgets.Containers.Draggable
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Draggable container for a single item. Useful for adding drag support without
having to implement a custom widget. Usually works in tandem with
"Monomer.Widgets.Containers.DropTarget".

Requires a value to identify the content (used when the item is dropped) and the
content to display.

@
dragItem = draggable "item" $ label "This label is draggable"
@

See Tutorial 6 (Composite) for a usage example.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Containers.Draggable (
  -- * Configuration
  DraggableRender,
  DraggableCfg,
  draggableMaxDim,
  draggableStyle,
  draggableRenderSource,
  draggableRenderSource_,
  draggableRender,
  -- * Constructors
  draggable,
  draggable_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (^?!), (.~), _Just, _2)
import Control.Monad (forM_, when)
import Data.Default
import Data.Maybe

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

import qualified Monomer.Lens as L

-- | Rendering function for the dragged state.
type DraggableRender s e
  = DraggableCfg s e  -- ^ The configuration of the draggable.
  -> WidgetEnv s e    -- ^ The widget environment.
  -> WidgetNode s e   -- ^ The widget node.
  -> Renderer         -- ^ The renderer.
  -> IO ()            -- ^ The drawing actions.

{-|
Configuration options for draggable:

- 'transparency': the alpha level to apply when rendering content in drag mode.
- 'draggableMaxDim': the maximum size of the largest axis when dragging. Keeps
  proportions.
- 'draggableStyle': the style to use when the item is being dragged.
- 'draggableRenderSource': whether to render the source widget when dragging.
- 'draggableRender': rendering function for the dragged state. Allows
  customizing this step without implementing a custom widget all the lifecycle
  steps.

The regular styling of this component applies only when the item is not being
dragged. To style the dragged container, use draggableStyle.

The transparency config only applies to the inner content.
-}
data DraggableCfg s e = DraggableCfg {
  _dgcTransparency :: Maybe Double,
  _dgcMaxDim :: Maybe Double,
  _dgcDragStyle :: Maybe StyleState,
  _dgcRenderSource :: Maybe Bool,
  _dgcCustomRender :: Maybe (DraggableRender s e)
}

instance Default (DraggableCfg s e) where
  def = DraggableCfg {
    _dgcTransparency = Nothing,
    _dgcMaxDim = Nothing,
    _dgcDragStyle = Nothing,
    _dgcRenderSource = Nothing,
    _dgcCustomRender = Nothing
  }

instance Semigroup (DraggableCfg s e) where
  (<>) t1 t2 = DraggableCfg {
    _dgcTransparency = _dgcTransparency t2 <|> _dgcTransparency t1,
    _dgcMaxDim = _dgcMaxDim t2 <|> _dgcMaxDim t1,
    _dgcDragStyle = _dgcDragStyle t2 <|> _dgcDragStyle t1,
    _dgcRenderSource = _dgcRenderSource t2 <|> _dgcRenderSource t1,
    _dgcCustomRender = _dgcCustomRender t2 <|> _dgcCustomRender t1
  }

instance Monoid (DraggableCfg s e) where
  mempty = def

instance CmbTransparency (DraggableCfg s e) where
  transparency transp = def {
    _dgcTransparency = Just transp
  }

{-|
Maximum dimension. Useful when aspect ratio needs to be maintained while at the
same time restricting growth.
-}
draggableMaxDim :: Double -> DraggableCfg s e
draggableMaxDim dim = def {
  _dgcMaxDim = Just dim
}

-- | The style of the dragged container.
draggableStyle :: [StyleState] -> DraggableCfg s e
draggableStyle styles = def {
  _dgcDragStyle = Just (mconcat styles)
}

-- | Renders the source widget when dragging.
draggableRenderSource :: DraggableCfg s e
draggableRenderSource = draggableRenderSource_ True

-- | Whether to render the source widget when dragging.
draggableRenderSource_ :: Bool -> DraggableCfg s e
draggableRenderSource_ hide = def {
  _dgcRenderSource = Just hide
}

-- | Rendering function for the dragged state.
draggableRender :: DraggableRender s e -> DraggableCfg s e
draggableRender render = def {
  _dgcCustomRender = Just render
}

-- | Creates a draggable container with a single node as child.
draggable
  :: DragMsg a
  => a               -- ^ The identifying value.
  -> WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created draggable container.
draggable msg managed = draggable_ msg def managed

-- | Creates a draggable container with a single node as child. Accepts config.
draggable_
  :: DragMsg a
  => a                   -- ^ The identifying value.
  -> [DraggableCfg s e]  -- ^ The config options.
  -> WidgetNode s e      -- ^ The child node.
  -> WidgetNode s e      -- ^ The created draggable container.
draggable_ msg configs managed = makeNode widget managed where
  config = mconcat configs
  widget = makeDraggable msg config

makeNode :: Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode widget managedWidget = defaultWidgetNode "draggable" widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makeDraggable :: DragMsg a => a -> DraggableCfg s e -> Widget s e
makeDraggable msg config = widget where
  baseWidget = createContainer () def {
    containerHandleEvent = handleEvent,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }
  widget = baseWidget {
    widgetRender = render
  }

  handleEvent wenv node target evt = case evt of
    ButtonAction p btn BtnPressed 1 -> Just result where
      result = resultReqs node [StartDrag wid path dragMsg]

    ButtonAction p btn BtnReleased _ -> Just result where
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
    style = currentStyle wenv node
    contentArea = fromMaybe def (removeOuterBounds style viewport)
    resized = (resultNode node, Seq.singleton contentArea)

  defaultRender config wenv node renderer =
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
    when (not dragged || renderSource) $
      forM_ (node ^. L.children) $ \child ->
        widgetRender (child ^. L.widget) wenv child renderer
    when dragged $
      createOverlay renderer $ do
        renderAction config wenv node renderer
    where
      dragged = isNodeDragged wenv node
      renderAction = fromMaybe defaultRender (_dgcCustomRender config)
      renderSource = fromMaybe False (_dgcRenderSource config)
