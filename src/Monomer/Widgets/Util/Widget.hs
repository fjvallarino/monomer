module Monomer.Widgets.Util.Widget (
  pointInViewport,
  defaultWidgetInstance,
  isWidgetVisible,
  isFocused,
  isHovered,
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
  handleFocusChange
) where

import Control.Lens ((&), (^#), (#~), (^.))
import Data.Default
import Data.Maybe
import Data.Typeable (cast, Typeable)

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event (checkKeyboard, isKeyC, isKeyV)
import Monomer.Graphics.Types

import qualified Monomer.Lens as L

pointInViewport :: Point -> WidgetInstance s e -> Bool
pointInViewport p inst = pointInRect p (_wiViewport inst)

defaultWidgetInstance :: WidgetType -> Widget s e -> WidgetInstance s e
defaultWidgetInstance widgetType widget = WidgetInstance {
  _wiWidgetType = widgetType,
  _wiKey = Nothing,
  _wiPath = Seq.empty,
  _wiWidget = widget,
  _wiChildren = Seq.empty,
  _wiSizeReqW = def,
  _wiSizeReqH = def,
  _wiEnabled = True,
  _wiVisible = True,
  _wiFocusable = False,
  _wiViewport = def,
  _wiRenderArea = def,
  _wiStyle = def
}

isWidgetVisible :: WidgetInstance s e -> Rect -> Bool
isWidgetVisible inst vp = _wiVisible inst && rectsOverlap vp (_wiViewport inst)

isFocused :: WidgetEnv s e -> WidgetInstance s e -> Bool
isFocused wenv inst = _weFocusedPath wenv == _wiPath inst

isHovered :: WidgetEnv s e -> WidgetInstance s e -> Bool
isHovered wenv inst = validPos && isTopLevel wenv inst where
  viewport = inst ^. L.viewport
  mousePos = wenv ^. L.inputStatus . L.mousePos
  validPos = pointInRect mousePos viewport

widgetDataGet :: s -> WidgetData s a -> a
widgetDataGet _ (WidgetValue value) = value
widgetDataGet model (WidgetLens lens) = model ^# lens

widgetDataSet :: WidgetData s a -> a -> [WidgetRequest s]
widgetDataSet WidgetValue{} _ = []
widgetDataSet (WidgetLens lens) value = [UpdateModel updateFn] where
  updateFn model = model & lens #~ value

resultWidget :: WidgetInstance s e -> WidgetResult s e
resultWidget inst = WidgetResult inst Seq.empty Seq.empty

resultEvts :: WidgetInstance s e -> [e] -> WidgetResult s e
resultEvts inst events = result where
  result = WidgetResult inst Seq.empty (Seq.fromList events)

resultReqs :: WidgetInstance s e -> [WidgetRequest s] -> WidgetResult s e
resultReqs inst requests = result where
  result = WidgetResult inst (Seq.fromList requests) Seq.empty

resultReqsEvts
  :: WidgetInstance s e -> [WidgetRequest s] -> [e] -> WidgetResult s e
resultReqsEvts inst requests events = result where
  result = WidgetResult inst (Seq.fromList requests) (Seq.fromList events)

makeState :: Typeable i => i -> s -> Maybe WidgetState
makeState state model = Just (WidgetState state)

useState ::  Typeable i => Maybe WidgetState -> Maybe i
useState Nothing = Nothing
useState (Just (WidgetState state)) = cast state

instanceMatches :: WidgetInstance s e -> WidgetInstance s e -> Bool
instanceMatches newInstance oldInstance = typeMatches && keyMatches where
  typeMatches = _wiWidgetType oldInstance == _wiWidgetType newInstance
  keyMatches = _wiKey oldInstance == _wiKey newInstance

isTopLevel :: WidgetEnv s e -> WidgetInstance s e -> Bool
isTopLevel wenv inst = maybe inTopLayer isPrefix (wenv ^. L.overlayPath) where
  mousePos = wenv ^. L.inputStatus . L.mousePos
  inTopLayer = wenv ^. L.inTopLayer $ mousePos
  path = _wiPath inst
  isPrefix parent = Seq.take (Seq.length parent) path == parent

handleFocusChange
  :: (c -> [e])
  -> (c -> [WidgetRequest s])
  -> c
  -> WidgetInstance s e
  -> Maybe (WidgetResult s e)
handleFocusChange evtFn reqFn config inst = result where
  evts = evtFn config
  reqs = reqFn config
  result
    | not (null evts && null reqs) = Just $ resultReqsEvts inst reqs evts
    | otherwise = Nothing
