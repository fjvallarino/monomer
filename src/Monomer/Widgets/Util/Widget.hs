module Monomer.Widgets.Util.Widget (
  pointInViewport,
  defaultWidgetInstance,
  isWidgetVisible,
  isFocused,
  isHovered,
  widgetDataGet,
  widgetDataSet,
  resultWidget,
  resultEvents,
  resultReqs,
  resultReqsEvents,
  mergeResults,
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
resultWidget inst = WidgetResult Seq.empty Seq.empty inst

resultEvents :: [e] -> WidgetInstance s e -> WidgetResult s e
resultEvents events inst = result where
  result = WidgetResult Seq.empty (Seq.fromList events) inst

resultReqs :: [WidgetRequest s] -> WidgetInstance s e -> WidgetResult s e
resultReqs requests inst = result where
  result = WidgetResult (Seq.fromList requests) Seq.empty inst

resultReqsEvents
  :: [WidgetRequest s] -> [e] -> WidgetInstance s e -> WidgetResult s e
resultReqsEvents requests events inst = result where
  result = WidgetResult (Seq.fromList requests) (Seq.fromList events) inst

mergeResults :: WidgetResult s e -> WidgetResult s e -> WidgetResult s e
mergeResults res1 res2 = newRes where
  WidgetResult reqs1 evts1 inst1 = res1
  WidgetResult reqs2 evts2 inst2 = res2
  newRes = WidgetResult (reqs1 <> reqs2) (evts1 <> evts2) inst2

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
    | not (null evts && null reqs) = Just $ resultReqsEvents reqs evts inst
    | otherwise = Nothing
