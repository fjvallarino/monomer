{-# LANGUAGE FlexibleContexts #-}

module Monomer.Widgets.Util.Widget (
  defaultWidgetNode,
  pointInViewport,
  isWidgetVisible,
  isPressed,
  isFocused,
  isHovered,
  visibleChildrenChanged,
  widgetDataGet,
  widgetDataSet,
  resultWidget,
  resultEvts,
  resultReqs,
  resultReqsEvts,
  makeState,
  useState,
  instanceMatches,
  isTopLevel,
  handleFocusRequest,
  handleFocusChange,
  resizeWidget,
  buildLocalMap,
  findWidgetByKey,
  getInstanceTree
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^#), (#~), (^.), (.~), (%~))
import Data.Default
import Data.Foldable (foldl')
import Data.Maybe
import Data.Map.Strict (Map)
import Data.Sequence (Seq(..), (|>))
import Data.Typeable (cast, Typeable)

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event (checkKeyboard, isKeyC, isKeyV)
import Monomer.Event.Types
import Monomer.Event.Util
import Monomer.Graphics.Types

import qualified Monomer.Lens as L

defaultWidgetNode :: WidgetType -> Widget s e -> WidgetNode s e
defaultWidgetNode widgetType widget = WidgetNode {
  _wnWidget = widget,
  _wnInfo = def & L.widgetType .~ widgetType,
  _wnChildren = Seq.empty
}

pointInViewport :: Point -> WidgetNode s e -> Bool
pointInViewport p node = pointInRect p (node ^. L.info . L.viewport)

isWidgetVisible :: WidgetNode s e -> Rect -> Bool
isWidgetVisible node vp = isVisible && isOverlapped where
  info = node ^. L.info
  isVisible = info ^. L.visible
  isOverlapped = rectsOverlap vp (info ^. L.viewport)

isPressed :: WidgetEnv s e -> WidgetNode s e -> Bool
isPressed wenv node = validPress where
  path = node ^. L.info . L.path
  pressed = wenv ^. L.pressedPath
  validPress = isNothing pressed || Just path == pressed

isFocused :: WidgetEnv s e -> WidgetNode s e -> Bool
isFocused wenv node = wenv ^. L.focusedPath == node ^. L.info . L.path

isHovered :: WidgetEnv s e -> WidgetNode s e -> Bool
isHovered wenv node = validPos && validPress && isTopLevel wenv node where
  viewport = node ^. L.info . L.viewport
  mousePos = wenv ^. L.inputStatus . L.mousePos
  validPos = pointInRect mousePos viewport
  validPress = isPressed wenv node

visibleChildrenChanged :: WidgetNode s e -> WidgetNode s e -> Bool
visibleChildrenChanged oldNode newNode = oldVisible /= newVisible  where
  oldVisible = fmap (^. L.info . L.visible) (oldNode ^. L.children)
  newVisible = fmap (^. L.info . L.visible) (newNode ^. L.children)

widgetDataGet :: s -> WidgetData s a -> a
widgetDataGet _ (WidgetValue value) = value
widgetDataGet model (WidgetLens lens) = model ^# lens

widgetDataSet :: WidgetData s a -> a -> [WidgetRequest s]
widgetDataSet WidgetValue{} _ = []
widgetDataSet (WidgetLens lens) value = [UpdateModel updateFn] where
  updateFn model = model & lens #~ value

resultWidget :: WidgetNode s e -> WidgetResult s e
resultWidget node = WidgetResult node Seq.empty Seq.empty

resultEvts :: WidgetNode s e -> [e] -> WidgetResult s e
resultEvts node events = result where
  result = WidgetResult node Seq.empty (Seq.fromList events)

resultReqs :: WidgetNode s e -> [WidgetRequest s] -> WidgetResult s e
resultReqs node requests = result where
  result = WidgetResult node (Seq.fromList requests) Seq.empty

resultReqsEvts :: WidgetNode s e -> [WidgetRequest s] -> [e] -> WidgetResult s e
resultReqsEvts node requests events = result where
  result = WidgetResult node (Seq.fromList requests) (Seq.fromList events)

makeState :: Typeable i => i -> s -> Maybe WidgetState
makeState state model = Just (WidgetState state)

useState ::  Typeable i => Maybe WidgetState -> Maybe i
useState Nothing = Nothing
useState (Just (WidgetState state)) = cast state

instanceMatches :: WidgetNode s e -> WidgetNode s e -> Bool
instanceMatches newNode oldNode = typeMatches && keyMatches where
  oldInfo = oldNode ^. L.info
  newInfo = newNode ^. L.info
  typeMatches = oldInfo ^. L.widgetType == newInfo ^. L.widgetType
  keyMatches = oldInfo ^. L.key == newInfo ^. L.key

isTopLevel :: WidgetEnv s e -> WidgetNode s e -> Bool
isTopLevel wenv node = maybe inTopLayer isPrefix (wenv ^. L.overlayPath) where
  mousePos = wenv ^. L.inputStatus . L.mousePos
  inTopLayer = wenv ^. L.inTopLayer $ mousePos
  path = node ^. L.info . L.path
  isPrefix parent = Seq.take (Seq.length parent) path == parent

handleFocusRequest
  :: WidgetEnv s e
  -> SystemEvent
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)
  -> Maybe (WidgetResult s e)
handleFocusRequest wenv evt node mResult = newResult where
  prevReqs = maybe Empty (^. L.requests) mResult
  isFocusable = node ^. L.info . L.focusable
  btnPressed = case evt of
    ButtonAction _ btn PressedBtn -> Just btn
    _ -> Nothing
  isFocusReq = btnPressed == Just (wenv ^. L.mainButton)
    && isFocusable
    && not (isFocused wenv node)
    && isTopLevel wenv node
    && isNothing (Seq.findIndexL isFocusRequest prevReqs)
  focusReq = SetFocus (node ^. L.info . L.path)
  newResult
    | isFocusReq && isJust mResult = (& L.requests %~ (|> focusReq)) <$> mResult
    | isFocusReq = Just $ resultReqs node [focusReq]
    | otherwise = mResult

handleFocusChange
  :: (c -> [e])
  -> (c -> [WidgetRequest s])
  -> c
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)
handleFocusChange evtFn reqFn config node = result where
  evts = evtFn config
  reqs = reqFn config
  result
    | not (null evts && null reqs) = Just $ resultReqsEvts node reqs evts
    | otherwise = Nothing

resizeWidget
  :: WidgetEnv s e
  -> Rect
  -> Rect
  -> WidgetNode s e
  -> WidgetNode s e
resizeWidget wenv viewport renderArea widgetRoot = newRoot where
  reqNode = widgetUpdateSizeReq (_wnWidget widgetRoot) wenv widgetRoot
  newRoot = widgetResize (reqNode ^. L.widget) wenv viewport renderArea reqNode

findWidgetByKey
  :: WidgetKey
  -> LocalKeys s e
  -> GlobalKeys s e
  -> Maybe (WidgetNode s e)
findWidgetByKey key localMap globalMap = local <|> global where
  local = M.lookup key localMap
  global = case key of
    WidgetKeyGlobal{} -> M.lookup key globalMap
    _ -> Nothing

buildLocalMap :: Seq (WidgetNode s e) -> Map WidgetKey (WidgetNode s e)
buildLocalMap widgets = newMap where
  addWidget map widget
    | isJust key = M.insert (fromJust key) widget map
    | otherwise = map
    where
      key = widget ^. L.info . L.key
  newMap = foldl' addWidget M.empty widgets

getInstanceTree
  :: WidgetEnv s e
  -> WidgetNode s e
  -> WidgetInstanceNode
getInstanceTree wenv node = instNode where
  instNode = WidgetInstanceNode {
    _winInfo = node ^. L.info,
    _winChildren = fmap (getChildNode wenv) (node ^. L.children)
  }
  getChildNode wenv child = widgetGetInstanceTree (child ^. L.widget) wenv child
