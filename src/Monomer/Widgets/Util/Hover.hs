{-|
Module      : Monomer.Widgets.Util.Hover
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions for hover related actions.
-}
module Monomer.Widgets.Util.Hover (
  isPointInNodeVp,
  isPointInNodeEllipse,
  isNodeActive,
  isNodePressed,
  isNodeTreeActive,
  isNodeTreePressed,
  isNodeDragged,
  isNodeHovered,
  isNodeHoveredEllipse_,
  isNodeTopLevel,
  isNodeInOverlay
) where

import Control.Lens ((&), (^.), (^?), _1, _Just)
import Data.Maybe

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event.Types
import Monomer.Event.Util
import Monomer.Graphics.Types
import Monomer.Helper

import qualified Monomer.Lens as L

-- | Checks if the given point is inside the node's viewport.
isPointInNodeVp :: Point -> WidgetNode s e -> Bool
isPointInNodeVp p node = pointInRect p (node ^. L.info . L.viewport)

-- | Checks if the given point is inside the ellipse delimited by the viewport.
isPointInNodeEllipse :: Point -> WidgetNode s e -> Bool
isPointInNodeEllipse p node = pointInEllipse p (node ^. L.info . L.viewport)

-- | Checks if the main button is pressed and pointer inside the vieport.
isNodeActive :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeActive = isNodeActive_ False

-- | Checks if the main button is pressed inside the vieport.
isNodePressed :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodePressed = isNodePressed_ False

-- | Checks if the node or any of its children is active.
isNodeTreeActive :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeTreeActive = isNodeActive_ True

-- | Checks if the node or any of its children is pressed.
isNodeTreePressed :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeTreePressed = isNodePressed_ True

{-|
Checks if the node is active, optionally including children. An active node was
clicked with the main button and has the pointer inside its viewport.
-}
isNodeActive_ :: Bool -> WidgetEnv s e -> WidgetNode s e -> Bool
isNodeActive_ includeChildren wenv node = validPos && pressed where
  viewport = node ^. L.info . L.viewport
  mousePos = wenv ^. L.inputStatus . L.mousePos
  validPos = pointInRect mousePos viewport
  pressed = isNodePressed_ includeChildren wenv node

{-|
Checks if the node is pressed, optionally including children. A pressed node was
clicked with the main button.
-}
isNodePressed_ :: Bool -> WidgetEnv s e -> WidgetNode s e -> Bool
isNodePressed_ includeChildren wenv node = result == Just True where
  path = node ^. L.info . L.path
  pressed = wenv ^. L.mainBtnPress ^? _Just . _1
  result
    | includeChildren = seqStartsWith path <$> pressed
    | otherwise = (path ==) <$> pressed

{-|
Checks if the node is being dragged. The node must have made a previous
request to be in that state.
-}
isNodeDragged :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeDragged wenv node = mainPressed && draggedPath == Just nodePath where
  mainPressed = isJust (wenv ^. L.mainBtnPress)
  draggedPath = wenv ^? L.dragStatus . _Just . _1
  nodePath = node ^. L.info . L.path

-- | Checks if node is hovered. Pointer must be in viewport and node top layer.
isNodeHovered :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeHovered wenv node = validPos && validPress && topLevel where
  viewport = node ^. L.info . L.viewport
  mousePos = wenv ^. L.inputStatus . L.mousePos
  validPos = pointInRect mousePos viewport
  pressed = wenv ^. L.mainBtnPress ^? _Just . _1
  validPress = isNothing pressed || isNodePressed wenv node
  topLevel = isNodeTopLevel wenv node

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
isNodeTopLevel wenv node = maybe inTopLayer isPrefix (wenv ^. L.overlayPath) where
  mousePos = wenv ^. L.inputStatus . L.mousePos
  inTopLayer = wenv ^. L.inTopLayer $ mousePos
  path = node ^. L.info . L.path
  isPrefix parent = Seq.take (Seq.length parent) path == parent

-- | Checks if the node is part of the active overlay, if any.
isNodeInOverlay :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeInOverlay wenv node = maybe False isPrefix (wenv ^. L.overlayPath) where
  path = node ^. L.info . L.path
  isPrefix overlayPath = Seq.take (Seq.length overlayPath) path == overlayPath
