{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Monomer.Widgets.Single (
  module Monomer.Core,
  module Monomer.Event,
  module Monomer.Graphics,
  module Monomer.Widgets.Util,

  Single(..),
  createSingle
) where

import Data.Default
import Data.Typeable (Typeable)

import Monomer.Core
import Monomer.Event
import Monomer.Graphics
import Monomer.Widgets.Util

type SingleGetBaseStyle s e
  = WidgetEnv s e
  -> WidgetInstance s e
  -> Maybe Style

type SingleInitHandler s e
  = WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetResult s e

type SingleMergeHandler s e
  = WidgetEnv s e
  -> Maybe WidgetState
  -> WidgetInstance s e
  -> WidgetResult s e

type SingleDisposeHandler s e
  = WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetResult s e

type SingleGetStateHandler s e
  = WidgetEnv s e
  -> Maybe WidgetState

type SingleFindNextFocusHandler s e
  = WidgetEnv s e
  -> FocusDirection
  -> Path
  -> WidgetInstance s e
  -> Maybe Path

type SingleFindByPointHandler s e
  = WidgetEnv s e
  -> Path
  -> Point
  -> WidgetInstance s e
  -> Maybe Path

type SingleEventHandler s e
  = WidgetEnv s e
  -> Path
  -> SystemEvent
  -> WidgetInstance s e
  -> Maybe (WidgetResult s e)

type SingleMessageHandler s e
  = forall i . Typeable i
  => WidgetEnv s e
  -> Path
  -> i
  -> WidgetInstance s e
  -> Maybe (WidgetResult s e)

type SingleGetSizeReqHandler s e
  = WidgetEnv s e
  -> WidgetInstance s e
  -> (SizeReq, SizeReq)

type SingleResizeHandler s e
  = WidgetEnv s e
  -> Rect
  -> Rect
  -> WidgetInstance s e
  -> WidgetInstance s e

type SingleRenderHandler s e
  =  Renderer
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> IO ()

data Single s e = Single {
  singleGetBaseStyle :: SingleGetBaseStyle s e,
  singleInit :: SingleInitHandler s e,
  singleMerge :: SingleMergeHandler s e,
  singleDispose :: SingleDisposeHandler s e,
  singleGetState :: SingleGetStateHandler s e,
  singleFindNextFocus :: SingleFindNextFocusHandler s e,
  singleFindByPoint :: SingleFindByPointHandler s e,
  singleHandleEvent :: SingleEventHandler s e,
  singleHandleMessage :: SingleMessageHandler s e,
  singleGetSizeReq :: SingleGetSizeReqHandler s e,
  singleResize :: SingleResizeHandler s e,
  singleRender :: SingleRenderHandler s e
}

instance Default (Single s e) where
  def = Single {
    singleGetBaseStyle = defaultGetBaseStyle,
    singleInit = defaultInit,
    singleMerge = defaultMerge,
    singleDispose = defaultDispose,
    singleGetState = defaultGetState,
    singleFindNextFocus = defaultFindNextFocus,
    singleFindByPoint = defaultFindByPoint,
    singleHandleEvent = defaultHandleEvent,
    singleHandleMessage = defaultHandleMessage,
    singleGetSizeReq = defaultGetSizeReq,
    singleResize = defaultResize,
    singleRender = defaultRender
  }

createSingle :: Single s e -> Widget s e
createSingle single = Widget {
  widgetInit = initWrapper single,
  widgetMerge = mergeWrapper single,
  widgetDispose = singleDispose single,
  widgetGetState = singleGetState single,
  widgetFindNextFocus = singleFindNextFocus single,
  widgetFindByPoint = singleFindByPoint single,
  widgetHandleEvent = handleEventWrapper single,
  widgetHandleMessage = singleHandleMessage single,
  widgetUpdateSizeReq = updateSizeReqWrapper single,
  widgetResize = resizeHandlerWrapper single,
  widgetRender = renderWrapper single
}

defaultGetBaseStyle :: SingleGetBaseStyle s e
defaultGetBaseStyle wenv inst = Nothing

defaultInit :: SingleInitHandler s e
defaultInit _ inst = resultWidget inst

initWrapper
  :: Single s e
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetResult s e
initWrapper single wenv inst = newResult where
  initHandler = singleInit single
  getBaseStyle = singleGetBaseStyle single
  baseStyle = getBaseStyle wenv inst
  styledInst = initInstanceStyle wenv baseStyle inst
  newResult = initHandler wenv styledInst

defaultMerge :: SingleMergeHandler s e
defaultMerge wenv oldState newInst = resultWidget newInst

mergeWrapper
  :: Single s e
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetInstance s e
  -> WidgetResult s e
mergeWrapper single wenv oldInst newInst = newResult where
  mergeHandler = singleMerge single
  getBaseStyle = singleGetBaseStyle single
  oldState = widgetGetState (_wiWidget oldInst) wenv
  tempInst = newInst {
    _wiViewport = _wiViewport oldInst,
    _wiRenderArea = _wiRenderArea oldInst
  }
  baseStyle = getBaseStyle wenv tempInst
  styledInst = initInstanceStyle wenv baseStyle tempInst
  newResult = mergeHandler wenv oldState styledInst

defaultDispose :: SingleDisposeHandler s e
defaultDispose _ inst = resultWidget inst

defaultGetState :: SingleGetStateHandler s e
defaultGetState _ = Nothing

defaultFindNextFocus :: SingleFindNextFocusHandler s e
defaultFindNextFocus wenv direction startFrom inst
  | isFocusCandidate direction startFrom inst = Just (_wiPath inst)
  | otherwise = Nothing

defaultFindByPoint :: SingleFindByPointHandler s e
defaultFindByPoint wenv path point inst
  | _wiVisible inst && pointInViewport point inst = Just (_wiPath inst)
  | otherwise = Nothing

defaultHandleEvent :: SingleEventHandler s e
defaultHandleEvent wenv target evt inst = Nothing

handleEventWrapper
  :: Single s e
  -> WidgetEnv s e
  -> Path
  -> SystemEvent
  -> WidgetInstance s e
  -> Maybe (WidgetResult s e)
handleEventWrapper single wenv target evt inst
  | not (_wiVisible inst) = Nothing
  | otherwise = handleStyleChange wenv target evt style result inst
  where
    style = activeStyle wenv inst
    handler = singleHandleEvent single
    result = handler wenv target evt inst

defaultHandleMessage :: SingleMessageHandler s e
defaultHandleMessage wenv target message inst = Nothing

defaultGetSizeReq :: SingleGetSizeReqHandler s e
defaultGetSizeReq wenv inst = def

updateSizeReqWrapper
  :: Single s e
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> WidgetInstance s e
updateSizeReqWrapper single wenv inst = newInst where
  handler = singleGetSizeReq single
  style = activeStyle wenv inst
  reqs = handler wenv inst
  (newReqW, newReqH) = sizeReqAddStyle style reqs
  newInst = inst {
    _wiSizeReqW = newReqW,
    _wiSizeReqH = newReqH
  }

defaultResize :: SingleResizeHandler s e
defaultResize wenv viewport renderArea inst = inst

resizeHandlerWrapper
  :: Single s e
  -> WidgetEnv s e
  -> Rect
  -> Rect
  -> WidgetInstance s e
  -> WidgetInstance s e
resizeHandlerWrapper single wenv viewport renderArea inst = newInst where
  handler = singleResize single
  tempInst = handler wenv viewport renderArea inst
  newInst = tempInst {
    _wiViewport = viewport,
    _wiRenderArea = renderArea
  }

defaultRender :: SingleRenderHandler s e
defaultRender renderer wenv inst = return ()

renderWrapper
  :: Single s e
  -> Renderer
  -> WidgetEnv s e
  -> WidgetInstance s e
  -> IO ()
renderWrapper single renderer wenv inst =
  drawInScissor renderer True viewport $
    drawStyledAction renderer renderArea style $ \_ ->
      rHandler renderer wenv inst
  where
    rHandler = singleRender single
    style = activeStyle wenv inst
    viewport = _wiViewport inst
    renderArea = _wiRenderArea inst
