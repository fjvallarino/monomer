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
import Data.Maybe
import Data.Sequence (Seq(..))
import Data.Typeable (Typeable)

import qualified Data.Sequence as Seq

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

type SingleMergeHandler s e a
  = WidgetEnv s e
  -> a
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e

type SingleDisposeHandler s e
  = WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e

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

type SingleGetSizeReqHandler s e a
  = WidgetEnv s e
  -> a
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

data Single s e a = Single {
  singleFocusOnPressedBtn :: Bool,
  singleStyleChangeCfg :: StyleChangeCfg,
  singleUseCustomSize :: Bool,
  singleUseScissor :: Bool,
  singleGetBaseStyle :: SingleGetBaseStyle s e,
  singleGetActiveStyle :: SingleGetActiveStyle s e,
  singleInit :: SingleInitHandler s e,
  singleMerge :: SingleMergeHandler s e a,
  singleDispose :: SingleDisposeHandler s e,
  singleFindNextFocus :: SingleFindNextFocusHandler s e,
  singleFindByPoint :: SingleFindByPointHandler s e,
  singleHandleEvent :: SingleEventHandler s e,
  singleHandleMessage :: SingleMessageHandler s e,
  singleGetSizeReq :: SingleGetSizeReqHandler s e a,
  singleResize :: SingleResizeHandler s e,
  singleRender :: SingleRenderHandler s e
}

instance Default (Single s e a) where
  def = Single {
    singleFocusOnPressedBtn = True,
    singleStyleChangeCfg = def,
    singleUseCustomSize = False,
    singleUseScissor = True,
    singleGetBaseStyle = defaultGetBaseStyle,
    singleGetActiveStyle = defaultGetActiveStyle,
    singleInit = defaultInit,
    singleMerge = defaultMerge,
    singleDispose = defaultDispose,
    singleFindNextFocus = defaultFindNextFocus,
    singleFindByPoint = defaultFindByPoint,
    singleHandleEvent = defaultHandleEvent,
    singleHandleMessage = defaultHandleMessage,
    singleGetSizeReq = defaultGetSizeReq,
    singleResize = defaultResize,
    singleRender = defaultRender
  }

createSingle :: Typeable a => a -> Single s e a -> Widget s e
createSingle state single = Widget {
  widgetInit = initWrapper single,
  widgetMerge = mergeWrapper single,
  widgetDispose = singleDispose single,
  widgetGetState = makeState state,
  widgetGetInstanceTree = getInstanceTree,
  widgetFindNextFocus = singleFindNextFocus single,
  widgetFindByPoint = singleFindByPoint single,
  widgetHandleEvent = handleEventWrapper single,
  widgetHandleMessage = handleMessageWrapper single,
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
  :: Typeable a
  => Single s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e
initWrapper single wenv node = newResult where
  initHandler = singleInit single
  getBaseStyle = singleGetBaseStyle single
  styledNode = initNodeStyle getBaseStyle wenv node
  tmpResult = initHandler wenv styledNode
  newResult = tmpResult
    & L.node .~ updateSizeReq single wenv (tmpResult ^. L.node)

defaultMerge :: SingleMergeHandler s e a
defaultMerge wenv oldState oldNode newNode = resultWidget newNode

mergeWrapper
  :: Typeable a
  => Single s e a
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
  tmpResult = case useState oldState of
    Just state -> mergeHandler wenv state oldNode styledNode
    _ -> resultWidget styledNode
  newResult
    | isResizeResult (Just tmpResult) = tmpResult
        & L.node .~ updateSizeReq single wenv (tmpResult ^. L.node)
    | otherwise = tmpResult

defaultDispose :: SingleDisposeHandler s e
defaultDispose wenv node = resultWidget node

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
  :: Typeable a
  => Single s e a
  -> WidgetEnv s e
  -> Path
  -> SystemEvent
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)
handleEventWrapper single wenv target evt node
  | not (node ^. L.info . L.visible) = Nothing
  | otherwise = handleStyleChange wenv target style styleCfg node evt result
  where
    style = singleGetActiveStyle single wenv node
    styleCfg = singleStyleChangeCfg single
    focusOnPressed = singleFocusOnPressedBtn single
    handler = singleHandleEvent single
    sizeResult = handleSizeReqChange single wenv node (Just evt)
      $ handler wenv target evt node
    newNode = maybe node (^. L.node) sizeResult
    result
      | focusOnPressed = handleFocusRequest wenv evt newNode sizeResult
      | otherwise = sizeResult

defaultHandleMessage :: SingleMessageHandler s e
defaultHandleMessage wenv target message node = Nothing

handleMessageWrapper :: forall s e a i . Typeable i
  => Typeable a
  => Single s e a
  -> WidgetEnv s e
  -> Path
  -> i
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)
handleMessageWrapper single wenv target msg node = result where
  handler = singleHandleMessage single
  result = handleSizeReqChange single wenv node Nothing
    $ handler wenv target msg node

defaultGetSizeReq :: SingleGetSizeReqHandler s e a
defaultGetSizeReq wenv node = def

updateSizeReq
  :: Typeable a
  => Single s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
updateSizeReq single wenv node = newNode where
  handler = singleGetSizeReq single
  style = singleGetActiveStyle single wenv node
  currState = widgetGetState (node ^. L.widget) wenv
  reqs = case useState currState of
    Just state -> handler wenv state node
    _ -> def
  (newReqW, newReqH) = sizeReqAddStyle style reqs
  newNode = node
    & L.info . L.sizeReqW .~ newReqW
    & L.info . L.sizeReqH .~ newReqH

handleSizeReqChange
  :: Typeable a
  => Single s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> Maybe SystemEvent
  -> Maybe (WidgetResult s e)
  -> Maybe (WidgetResult s e)
handleSizeReqChange single wenv node evt mResult = result where
  baseResult = fromMaybe (resultWidget node) mResult
  newNode = baseResult ^. L.node
  resizeReq = isResizeResult mResult
  styleChanged = isJust evt && styleStateChanged wenv newNode (fromJust evt)
  result
    | styleChanged || resizeReq = Just $ baseResult
      & L.node .~ updateSizeReq single wenv newNode
    | otherwise = mResult

defaultResize :: SingleResizeHandler s e
defaultResize wenv viewport renderArea node = node

resizeHandlerWrapper
  :: Single s e a
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
  :: Single s e a
  -> Renderer
  -> WidgetEnv s e
  -> WidgetNode s e
  -> IO ()
renderWrapper single renderer wenv node =
  drawInScissor renderer useScissor viewport $
    drawStyledAction renderer renderArea style $ \_ ->
      handler renderer wenv node
  where
    handler = singleRender single
    useScissor = singleUseScissor single
    style = singleGetActiveStyle single wenv node
    viewport = node ^. L.info . L.viewport
    renderArea = node ^. L.info . L.renderArea
