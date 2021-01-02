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

import Control.Lens ((&), (^.), (^?), (.~), _Just)
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

type SingleGetActiveStyle s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> StyleState

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
  singleStyleChangeCfg :: StyleChangeCfg,
  singleFocusOnPressedBtn :: Bool,
  singleUseCustomSize :: Bool,
  singleGetBaseStyle :: SingleGetBaseStyle s e,
  singleGetActiveStyle :: SingleGetActiveStyle s e,
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
    singleStyleChangeCfg = def,
    singleFocusOnPressedBtn = True,
    singleUseCustomSize = False,
    singleGetBaseStyle = defaultGetBaseStyle,
    singleGetActiveStyle = defaultGetActiveStyle,
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
  widgetUpdateSizeReq = updateSizeReqWrapper single,
  widgetResize = resizeHandlerWrapper single,
  widgetRender = renderWrapper single
}

defaultGetBaseStyle :: SingleGetBaseStyle s e
defaultGetBaseStyle wenv node = Nothing

defaultGetActiveStyle :: SingleGetActiveStyle s e
defaultGetActiveStyle wenv node = activeStyle wenv node

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
  | otherwise = handleStyleChange wenv target evt style resFocus styleCfg node
  where
    style = singleGetActiveStyle single wenv node
    styleCfg = singleStyleChangeCfg single
    handler = singleHandleEvent single
    result = handler wenv target evt node
    resFocus
      | singleFocusOnPressedBtn single = handleFocusRequest wenv evt node result
      | otherwise = result

defaultHandleMessage :: SingleMessageHandler s e
defaultHandleMessage wenv target message node = Nothing

defaultGetSizeReq :: SingleGetSizeReqHandler s e
defaultGetSizeReq wenv node = def

updateSizeReqWrapper
  :: Single s e
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
updateSizeReqWrapper single wenv node = newNode where
  handler = singleGetSizeReq single
  style = singleGetActiveStyle single wenv node
  reqs = handler wenv node
  (newReqW, newReqH) = sizeReqAddStyle style reqs
  newNode = node
    & L.info . L.sizeReqW .~ newReqW
    & L.info . L.sizeReqH .~ newReqH

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
  useCustomSize = singleUseCustomSize single
  handler = singleResize single
  tempNode = handler wenv viewport renderArea node
  lensVp = L.info . L.viewport
  lensRa = L.info . L.renderArea
  newVp
    | useCustomSize = tempNode ^. lensVp
    | otherwise = viewport
  newRa
    | useCustomSize = tempNode ^. lensRa
    | otherwise = renderArea
  newNode = tempNode
    & L.info . L.viewport .~ newVp
    & L.info . L.renderArea .~ newRa

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
    style = singleGetActiveStyle single wenv node
    viewport = node ^. L.info . L.viewport
    renderArea = node ^. L.info . L.renderArea
