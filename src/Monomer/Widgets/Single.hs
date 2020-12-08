{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Monomer.Widgets.Single (
  module Monomer.Core,
  module Monomer.Core.Combinators,
  module Monomer.Event,
  module Monomer.Graphics,
  module Monomer.Widgets.Util,

  Single(..),
  createSingle
) where

import Control.Lens ((&), (^.), (.~))
import Data.Default
import Data.Typeable (Typeable)

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.Graphics
import Monomer.Widgets.Util

import qualified Monomer.Lens as L

type SingleGetBaseStyle s e
  = GetBaseStyle s e

type SingleInitHandler s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e

type SingleMergeHandler s e
  = WidgetEnv s e
  -> Maybe WidgetState
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e

type SingleDisposeHandler s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e

type SingleGetStateHandler s e
  = WidgetEnv s e
  -> Maybe WidgetState

type SingleFindNextFocusHandler s e
  = WidgetEnv s e
  -> FocusDirection
  -> Path
  -> WidgetNode s e
  -> Maybe Path

type SingleFindByPointHandler s e
  = WidgetEnv s e
  -> Path
  -> Point
  -> WidgetNode s e
  -> Maybe Path

type SingleEventHandler s e
  = WidgetEnv s e
  -> Path
  -> SystemEvent
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)

type SingleMessageHandler s e
  = forall i . Typeable i
  => WidgetEnv s e
  -> Path
  -> i
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)

type SingleGetSizeReqHandler s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> (SizeReq, SizeReq)

type SingleResizeHandler s e
  = WidgetEnv s e
  -> Rect
  -> Rect
  -> WidgetNode s e
  -> WidgetNode s e

type SingleRenderHandler s e
  =  Renderer
  -> WidgetEnv s e
  -> WidgetNode s e
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
  widgetGetInstanceTree = getInstanceTree,
  widgetFindNextFocus = singleFindNextFocus single,
  widgetFindByPoint = singleFindByPoint single,
  widgetHandleEvent = handleEventWrapper single,
  widgetHandleMessage = singleHandleMessage single,
  widgetGetSizeReq = getSizeReqWrapper single,
  widgetResize = resizeHandlerWrapper single,
  widgetRender = renderWrapper single
}

defaultGetBaseStyle :: SingleGetBaseStyle s e
defaultGetBaseStyle wenv node = Nothing

defaultInit :: SingleInitHandler s e
defaultInit wenv node = resultWidget node

initWrapper
  :: Single s e
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e
initWrapper single wenv node = newResult where
  initHandler = singleInit single
  getBaseStyle = singleGetBaseStyle single
  styledNode = initNodeStyle getBaseStyle wenv node
  newResult = initHandler wenv styledNode

defaultMerge :: SingleMergeHandler s e
defaultMerge wenv oldState oldNode newNode = resultWidget newNode

mergeWrapper
  :: Single s e
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e
mergeWrapper single wenv oldNode newNode = newResult where
  mergeHandler = singleMerge single
  getBaseStyle = singleGetBaseStyle single
  oldState = widgetGetState (oldNode ^. L.widget) wenv
  oldInfo = oldNode ^. L.info
  tempNode = newNode
    & L.info . L.viewport .~ oldInfo ^. L.viewport
    & L.info . L.renderArea .~ oldInfo ^. L.renderArea
    & L.info . L.sizeReqW .~ oldInfo ^. L.sizeReqW
    & L.info . L.sizeReqH .~ oldInfo ^. L.sizeReqH
  styledNode = initNodeStyle getBaseStyle wenv tempNode
  newResult = mergeHandler wenv oldState oldNode styledNode

defaultDispose :: SingleDisposeHandler s e
defaultDispose wenv node = resultWidget node

defaultGetState :: SingleGetStateHandler s e
defaultGetState wenv = Nothing

defaultFindNextFocus :: SingleFindNextFocusHandler s e
defaultFindNextFocus wenv direction startFrom node
  | isFocusCandidate direction startFrom node = Just path
  | otherwise = Nothing
  where
    path = node ^. L.info . L.path

defaultFindByPoint :: SingleFindByPointHandler s e
defaultFindByPoint wenv path point node
  | isVisible && pointInViewport point node = Just path
  | otherwise = Nothing
  where
    isVisible = node ^. L.info . L.visible
    path = node ^. L.info . L.path

defaultHandleEvent :: SingleEventHandler s e
defaultHandleEvent wenv target evt node = Nothing

handleEventWrapper
  :: Single s e
  -> WidgetEnv s e
  -> Path
  -> SystemEvent
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)
handleEventWrapper single wenv target evt node
  | not (node ^. L.info . L.visible) = Nothing
  | otherwise = handleStyleChange wenv target evt style result node
  where
    style = activeStyle wenv node
    handler = singleHandleEvent single
    result = handler wenv target evt node

defaultHandleMessage :: SingleMessageHandler s e
defaultHandleMessage wenv target message node = Nothing

defaultGetSizeReq :: SingleGetSizeReqHandler s e
defaultGetSizeReq wenv node = def

getSizeReqWrapper
  :: Single s e
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetSizeReq s e
getSizeReqWrapper single wenv node = newSizeReq where
  handler = singleGetSizeReq single
  style = activeStyle wenv node
  (sizeReqW, sizeReqH) = handler wenv node
  newSizeReq = sizeReqAddStyle style (WidgetSizeReq node sizeReqW sizeReqH)

defaultResize :: SingleResizeHandler s e
defaultResize wenv viewport renderArea node = node

resizeHandlerWrapper
  :: Single s e
  -> WidgetEnv s e
  -> Rect
  -> Rect
  -> WidgetNode s e
  -> WidgetNode s e
resizeHandlerWrapper single wenv viewport renderArea node = newNode where
  handler = singleResize single
  tempNode = handler wenv viewport renderArea node
  newNode = tempNode
    & L.info . L.viewport .~ viewport
    & L.info . L.renderArea .~ renderArea

defaultRender :: SingleRenderHandler s e
defaultRender renderer wenv node = return ()

renderWrapper
  :: Single s e
  -> Renderer
  -> WidgetEnv s e
  -> WidgetNode s e
  -> IO ()
renderWrapper single renderer wenv node =
  drawInScissor renderer True viewport $
    drawStyledAction renderer renderArea style $ \_ ->
      rHandler renderer wenv node
  where
    rHandler = singleRender single
    style = activeStyle wenv node
    viewport = node ^. L.info . L.viewport
    renderArea = node ^. L.info . L.renderArea
