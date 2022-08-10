{-|
Module      : Monomer.Widgets.Util.Hover
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions for hover related actions.
-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Util.Hover (
  isPointInNodeVp,
  isPointInNodeEllipse,
  isNodeActive,
  isNodeInfoActive,
  isNodePressed,
  isNodeInfoPressed,
  isNodeTreeActive,
  isNodeTreePressed,
  isNodeDragged,
  isNodeInfoDragged,
  isNodeHovered,
  isNodeInfoHovered,
  isNodeHoveredEllipse_,
  isNodeTopLevel,
  isNodeInfoTopLevel,
  isNodeInOverlay,
  isNodeInfoInOverlay
) where

import Control.Lens ((&), (^.), (^?), _1, _Just)
import Data.Maybe

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Helper

import qualified Monomer.Core.Lens as L
import qualified Monomer.Event.Lens as L

-- | Checks if the given point is inside the node's viewport.
isPointInNodeVp :: WidgetNode s e -> Point -> Bool
isPointInNodeVp node p = pointInRect p (node ^. L.info . L.viewport)

-- | Checks if the given point is inside the ellipse delimited by the viewport.
isPointInNodeEllipse :: WidgetNode s e -> Point -> Bool
isPointInNodeEllipse node p = pointInEllipse p (node ^. L.info . L.viewport)

-- | Checks if the main button is pressed and pointer inside the vieport.
isNodeActive :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeActive wenv node = isNodeInfoActive False wenv (node ^. L.info)

-- | Checks if the main button is pressed inside the vieport.
isNodePressed :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodePressed wenv node = isNodeInfoPressed False wenv (node ^. L.info)

-- | Checks if the node or any of its children is active.
isNodeTreeActive :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeTreeActive wenv node = isNodeInfoActive True wenv (node ^. L.info)

-- | Checks if the node or any of its children is pressed.
isNodeTreePressed :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeTreePressed wenv node = isNodeInfoPressed True wenv (node ^. L.info)

{-|
Checks if the node is active, optionally including children. An active node was
clicked with the main button and has the pointer inside its viewport.
-}
isNodeInfoActive :: Bool -> WidgetEnv s e -> WidgetNodeInfo -> Bool
isNodeInfoActive checkChildren wenv info = validPos && pressed && topLevel where
  viewport = info ^. L.viewport
  mousePos = wenv ^. L.inputStatus . L.mousePos
  validPos = pointInRect mousePos viewport
  pressed = isNodeInfoPressed checkChildren wenv info
  topLevel = isNodeInfoTopLevel wenv info

{-|
Checks if the node is pressed, optionally including children. A pressed node was
clicked with the main button, but the pointer may not be inside its viewport.
-}
isNodeInfoPressed :: Bool -> WidgetEnv s e -> WidgetNodeInfo -> Bool
isNodeInfoPressed includeChildren wenv info = result == Just True where
  path = info ^. L.path
  pressed = wenv ^. L.mainBtnPress ^? _Just . _1
  result
    | includeChildren = seqStartsWith path <$> pressed
    | otherwise = (path ==) <$> pressed

{-|
Checks if the node is being dragged. The node must have made a previous request
to be in that state.
-}
isNodeDragged :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeDragged wenv node = isNodeInfoDragged wenv (node ^. L.info)

-- | Checks if the nodeInfo is being dragged.
isNodeInfoDragged :: WidgetEnv s e -> WidgetNodeInfo -> Bool
isNodeInfoDragged wenv info = mainPressed && draggedPath == Just nodePath where
  mainPressed = isJust (wenv ^. L.mainBtnPress)
  draggedPath = wenv ^? L.dragStatus . _Just . _1
  nodePath = info ^. L.path

-- | Checks if node is hovered. Pointer must be in viewport and node top layer.
isNodeHovered :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeHovered wenv node = isNodeInfoHovered wenv (node ^. L.info)

-- | Checks if nodeInfo is hovered.
isNodeInfoHovered :: WidgetEnv s e -> WidgetNodeInfo -> Bool
isNodeInfoHovered wenv info = validPos && validPress && topLevel where
  viewport = info ^. L.viewport
  mousePos = wenv ^. L.inputStatus . L.mousePos
  validPos = pointInRect mousePos viewport
  pressed = wenv ^. L.mainBtnPress ^? _Just . _1
  validPress = isNothing pressed || isNodeInfoPressed False wenv info
  topLevel = isNodeInfoTopLevel wenv info

-- | Checks if node is hovered, limited to an elliptical shape.
isNodeHoveredEllipse_ :: Rect -> WidgetEnv s e -> WidgetNode s e -> Bool
isNodeHoveredEllipse_ area wenv node = validPos && validPress && topLevel where
  mousePos = wenv ^. L.inputStatus . L.mousePos
  validPos = pointInEllipse mousePos area
  pressed = wenv ^. L.mainBtnPress ^? _Just . _1
  validPress = isNothing pressed || isNodePressed wenv node
  topLevel = isNodeTopLevel wenv node

{-|
Checks if a node is in a top layer. Being in zstack can cause this to be False.
-}
isNodeTopLevel :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeTopLevel wenv node = isNodeInfoTopLevel wenv (node ^. L.info)

-- | Checks if a nodeInfo is in a top layer.
isNodeInfoTopLevel :: WidgetEnv s e -> WidgetNodeInfo -> Bool
isNodeInfoTopLevel wenv info = result where
  mousePos = wenv ^. L.inputStatus . L.mousePos
  inTopLayer = wenv ^. L.inTopLayer $ mousePos
  path = info ^. L.path
  isPrefix parent = Seq.take (Seq.length parent) path == parent
  result = maybe inTopLayer isPrefix (wenv ^. L.overlayPath)

-- | Checks if the node is part of the active overlay, if any.
isNodeInOverlay :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeInOverlay wenv node = isNodeInfoInOverlay wenv (node ^. L.info)

-- | Checks if the nodeInfo is part of the active overlay, if any.
isNodeInfoInOverlay :: WidgetEnv s e -> WidgetNodeInfo -> Bool
isNodeInfoInOverlay wenv info = result where
  path = info ^. L.path
  isPrefix overlayPath = Seq.take (Seq.length overlayPath) path == overlayPath
  result = maybe False isPrefix (wenv ^. L.overlayPath)
