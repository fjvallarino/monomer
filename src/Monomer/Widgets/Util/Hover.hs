module Monomer.Widgets.Util.Hover (
  isPointInNodeVp,
  isPointInNodeEllipse,
  isNodeActive,
  isNodePressed,
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
import Monomer.Event (checkKeyboard, isKeyC, isKeyV)
import Monomer.Event.Types
import Monomer.Event.Util
import Monomer.Graphics.Types

import qualified Monomer.Lens as L

isPointInNodeVp :: Point -> WidgetNode s e -> Bool
isPointInNodeVp p node = pointInRect p (node ^. L.info . L.viewport)

isPointInNodeEllipse :: Point -> WidgetNode s e -> Bool
isPointInNodeEllipse p node = pointInEllipse p (node ^. L.info . L.viewport)

isNodeActive :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeActive wenv node = validPos && pressed where
  viewport = node ^. L.info . L.viewport
  mousePos = wenv ^. L.inputStatus . L.mousePos
  validPos = pointInRect mousePos viewport
  pressed = isNodePressed wenv node

isNodePressed :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodePressed wenv node = Just path == pressed where
  path = node ^. L.info . L.path
  pressed = wenv ^. L.mainBtnPress ^? _Just . _1

isNodeDragged :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeDragged wenv node = mainPressed && draggedPath == Just nodePath where
  mainPressed = isJust (wenv ^. L.mainBtnPress)
  draggedPath = wenv ^? L.dragStatus . _Just . _1
  nodePath = node ^. L.info . L.path

isNodeHovered :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeHovered wenv node = validPos && validPress && topLevel where
  viewport = node ^. L.info . L.viewport
  mousePos = wenv ^. L.inputStatus . L.mousePos
  validPos = pointInRect mousePos viewport
  pressed = wenv ^. L.mainBtnPress ^? _Just . _1
  validPress = isNothing pressed || isNodePressed wenv node
  topLevel = isNodeTopLevel wenv node

isNodeHoveredEllipse_ :: Rect -> WidgetEnv s e -> WidgetNode s e -> Bool
isNodeHoveredEllipse_ area wenv node = validPos && validPress && topLevel where
  mousePos = wenv ^. L.inputStatus . L.mousePos
  validPos = pointInEllipse mousePos area
  pressed = wenv ^. L.mainBtnPress ^? _Just . _1
  validPress = isNothing pressed || isNodePressed wenv node
  topLevel = isNodeTopLevel wenv node

isNodeTopLevel :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeTopLevel wenv node = maybe inTopLayer isPrefix (wenv ^. L.overlayPath) where
  mousePos = wenv ^. L.inputStatus . L.mousePos
  inTopLayer = wenv ^. L.inTopLayer $ mousePos
  path = node ^. L.info . L.path
  isPrefix parent = Seq.take (Seq.length parent) path == parent

isNodeInOverlay :: WidgetEnv s e -> WidgetNode s e -> Bool
isNodeInOverlay wenv node = maybe False isPrefix (wenv ^. L.overlayPath) where
  path = node ^. L.info . L.path
  isPrefix overlayPath = Seq.take (Seq.length overlayPath) path == overlayPath
