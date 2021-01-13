module Monomer.Widgets.Util.Hover (
  pointInViewport,
  isMainBtnPressed,
  isPressed,
  isHovered,
  isHoveredEllipse_,
  isTopLevel,
  isInOverlay
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

pointInViewport :: Point -> WidgetNode s e -> Bool
pointInViewport p node = pointInRect p (node ^. L.info . L.viewport)

isMainBtnPressed :: WidgetEnv s e -> WidgetNode s e -> Bool
isMainBtnPressed wenv node = isPressed where
  inputStatus = wenv ^. L.inputStatus
  mainBtn = wenv ^. L.mainButton
  viewport = node ^. L.info . L.viewport
  isPressed = isButtonPressedInRect inputStatus mainBtn viewport

isPressed :: WidgetEnv s e -> WidgetNode s e -> Bool
isPressed wenv node = Just path == pressed where
  path = node ^. L.info . L.path
  pressed = wenv ^. L.mainBtnPress ^? _Just . _1

isHovered :: WidgetEnv s e -> WidgetNode s e -> Bool
isHovered wenv node = validPos && validPress && isTopLevel wenv node where
  viewport = node ^. L.info . L.viewport
  mousePos = wenv ^. L.inputStatus . L.mousePos
  validPos = pointInRect mousePos viewport
  pressed = wenv ^. L.mainBtnPress ^? _Just . _1
  validPress = isNothing pressed || isPressed wenv node

isHoveredEllipse_ :: Rect -> WidgetEnv s e -> WidgetNode s e -> Bool
isHoveredEllipse_ area wenv node = validPos && isTopLevel wenv node where
  mousePos = wenv ^. L.inputStatus . L.mousePos
  validPos = pointInEllipse mousePos area

isTopLevel :: WidgetEnv s e -> WidgetNode s e -> Bool
isTopLevel wenv node = maybe inTopLayer isPrefix (wenv ^. L.overlayPath) where
  mousePos = wenv ^. L.inputStatus . L.mousePos
  inTopLayer = wenv ^. L.inTopLayer $ mousePos
  path = node ^. L.info . L.path
  isPrefix parent = Seq.take (Seq.length parent) path == parent

isInOverlay :: WidgetEnv s e -> WidgetNode s e -> Bool
isInOverlay wenv node = maybe False isPrefix (wenv ^. L.overlayPath) where
  path = node ^. L.info . L.path
  isPrefix overlayPath = Seq.take (Seq.length overlayPath) path == overlayPath
