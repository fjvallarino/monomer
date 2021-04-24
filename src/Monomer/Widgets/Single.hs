{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Monomer.Widgets.Single (
  module Monomer.Core,
  module Monomer.Core.Combinators,
  module Monomer.Event,
  module Monomer.Graphics,
  module Monomer.Widgets.Util,

  Single(..),
  createSingle,
  updateSizeReq
) where

import Control.Exception (AssertionFailed(..), throw)
import Control.Lens ((&), (^.), (^?), (.~), (%~), _Just)
import Data.Default
import Data.Maybe
import Data.Sequence (Seq(..), (|>))
import Data.Typeable (Typeable, cast)

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
  -> Maybe WidgetNodeInfo

type SingleFindByPointHandler s e
  = WidgetEnv s e
  -> Path
  -> Point
  -> WidgetNode s e
  -> Maybe WidgetNodeInfo

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
  -> WidgetNode s e
  -> WidgetResult s e

type SingleRenderHandler s e
  =  Renderer
  -> WidgetEnv s e
  -> WidgetNode s e
  -> IO ()

data Single s e a = Single {
  singleAddStyleReq :: Bool,
  singleFocusOnPressedBtn :: Bool,
  singleUseCustomCursor :: Bool,
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
    singleAddStyleReq = True,
    singleFocusOnPressedBtn = True,
    singleUseCustomCursor = False,
    singleUseCustomSize = False,
    singleUseScissor = False,
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

createSingle :: WidgetModel a => a -> Single s e a -> Widget s e
createSingle state single = Widget {
  widgetInit = initWrapper single,
  widgetMerge = mergeWrapper single,
  widgetDispose = disposeWrapper single,
  widgetGetState = makeState state,
  widgetSave = saveWrapper single,
  widgetFindNextFocus = singleFindNextFocus single,
  widgetFindByPoint = singleFindByPoint single,
  widgetFindByPath = singleFindByPath,
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
  :: WidgetModel a
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
  :: WidgetModel a
  => Single s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e
mergeWrapper single wenv oldNode newNode = newResult where
  mergeHandler = singleMerge single
  oldState = widgetGetState (oldNode ^. L.widget) wenv
  oldInfo = oldNode ^. L.info
  nodeHandler wenv styledNode = case useState oldState of
    Just state -> mergeHandler wenv state oldNode styledNode
    _ -> resultWidget styledNode
  tmpResult = runNodeHandler single wenv oldInfo newNode nodeHandler
  newResult = handleWidgetIdChange oldNode tmpResult

runNodeHandler
  :: WidgetModel a
  => Single s e a
  -> WidgetEnv s e
  -> WidgetNodeInfo
  -> WidgetNode s e
  -> (WidgetEnv s e -> WidgetNode s e -> WidgetResult s e)
  -> WidgetResult s e
runNodeHandler single wenv oldInfo newNode nodeHandler = newResult where
  getBaseStyle = singleGetBaseStyle single
  tempNode = newNode
    & L.info . L.widgetId .~ oldInfo ^. L.widgetId
    & L.info . L.viewport .~ oldInfo ^. L.viewport
    & L.info . L.sizeReqW .~ oldInfo ^. L.sizeReqW
    & L.info . L.sizeReqH .~ oldInfo ^. L.sizeReqH
  styledNode = initNodeStyle getBaseStyle wenv tempNode
  tmpResult = nodeHandler wenv styledNode
  newResult
    | isResizeResult (Just tmpResult) = tmpResult
        & L.node .~ updateSizeReq single wenv (tmpResult ^. L.node)
    | otherwise = tmpResult

saveWrapper
  :: WidgetModel a
  => Single s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetInstanceNode
saveWrapper container wenv node = instNode where
  instNode = WidgetInstanceNode {
    _winInfo = node ^. L.info,
    _winState = widgetGetState (node ^. L.widget) wenv,
    _winChildren = fmap (saveChildNode wenv) (node ^. L.children)
  }
  saveChildNode wenv child = widgetSave (child ^. L.widget) wenv child

defaultDispose :: SingleDisposeHandler s e
defaultDispose wenv node = resultWidget node

disposeWrapper
  :: Single s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetResult s e
disposeWrapper single wenv node = result where
  disposeHandler = singleDispose single
  WidgetResult newNode reqs = disposeHandler wenv node
  widgetId = node ^. L.info . L.widgetId
  newReqs = reqs |> ResetWidgetPath widgetId
  result = WidgetResult newNode newReqs

defaultFindNextFocus :: SingleFindNextFocusHandler s e
defaultFindNextFocus wenv direction startFrom node
  | isFocusCandidate direction startFrom node = Just (node ^. L.info)
  | otherwise = Nothing

defaultFindByPoint :: SingleFindByPointHandler s e
defaultFindByPoint wenv start point node
  | visible && validPath && isPointInNodeVp point node = Just info
  | otherwise = Nothing
  where
    info = node ^. L.info
    visible = info ^. L.visible
    path = node ^. L.info . L.path
    validPath = seqStartsWith start path

singleFindByPath
  :: WidgetEnv s e
  -> Path
  -> WidgetNode s e
  -> Maybe WidgetNodeInfo
singleFindByPath wenv path node
  | info ^. L.path == path = Just info
  | otherwise = Nothing
  where
    info = node ^. L.info

defaultHandleEvent :: SingleEventHandler s e
defaultHandleEvent wenv target evt node = Nothing

handleEventWrapper
  :: WidgetModel a
  => Single s e a
  -> WidgetEnv s e
  -> Path
  -> SystemEvent
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)
handleEventWrapper single wenv target evt node
  | not (node ^. L.info . L.visible) = Nothing
  | otherwise = handleStyleChange wenv target style handleCursor node evt result
  where
    style = singleGetActiveStyle single wenv node
    handleCursor = not (singleUseCustomCursor single)
    focusOnPressed = singleFocusOnPressedBtn single
    handler = singleHandleEvent single
    handlerRes = handler wenv target evt node
    sizeResult = handleSizeReqChange single wenv node (Just evt) handlerRes
    result
      | focusOnPressed = handleFocusRequest wenv evt node sizeResult
      | otherwise = sizeResult

handleFocusRequest
  :: WidgetEnv s e
  -> SystemEvent
  -> WidgetNode s e
  -> Maybe (WidgetResult s e)
  -> Maybe (WidgetResult s e)
handleFocusRequest wenv evt oldNode mResult = newResult where
  node = maybe oldNode (^. L.node) mResult
  prevReqs = maybe Empty (^. L.requests) mResult
  isFocusable = node ^. L.info . L.focusable
  btnPressed = case evt of
    ButtonAction _ btn PressedBtn _ -> Just btn
    _ -> Nothing
  isFocusReq = btnPressed == Just (wenv ^. L.mainButton)
    && isFocusable
    && not (isNodeFocused wenv node)
    && isNodeTopLevel wenv node
    && isNothing (Seq.findIndexL isFocusRequest prevReqs)
  focusReq = SetFocus (node ^. L.info . L.widgetId)
  newResult
    | isFocusReq && isJust mResult = (& L.requests %~ (|> focusReq)) <$> mResult
    | isFocusReq = Just $ resultReqs node [focusReq]
    | otherwise = mResult

defaultHandleMessage :: SingleMessageHandler s e
defaultHandleMessage wenv target message node = Nothing

handleMessageWrapper :: forall s e a i . (WidgetModel a, Typeable i)
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
  :: WidgetModel a
  => Single s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
updateSizeReq single wenv node = newNode where
  addStyleReq = singleAddStyleReq single
  handler = singleGetSizeReq single
  style = singleGetActiveStyle single wenv node
  currState = widgetGetState (node ^. L.widget) wenv
  reqs = case useState currState of
    Just state -> handler wenv state node
    _ -> def
  (tmpReqW, tmpReqH)
    | addStyleReq = sizeReqAddStyle style reqs
    | otherwise = reqs
  -- User settings take precedence
  newReqW = fromMaybe tmpReqW (style ^. L.sizeReqW)
  newReqH = fromMaybe tmpReqH (style ^. L.sizeReqH)
  newNode = node
    & L.info . L.sizeReqW .~ newReqW
    & L.info . L.sizeReqH .~ newReqH

handleSizeReqChange
  :: WidgetModel a
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
defaultResize wenv viewport node = resultWidget node

resizeHandlerWrapper
  :: WidgetModel a
  => Single s e a
  -> WidgetEnv s e
  -> Rect
  -> WidgetNode s e
  -> WidgetResult s e
resizeHandlerWrapper single wenv viewport node = result where
  useCustomSize = singleUseCustomSize single
  handler = singleResize single
  tmpRes = handler wenv viewport node
  lensVp = L.info . L.viewport
  newVp
    | useCustomSize = tmpRes ^. L.node . lensVp
    | otherwise = viewport
  tmpResult = Just $ tmpRes
    & L.node . L.info . L.viewport .~ newVp
  newNode = tmpRes ^. L.node
  result = fromJust $ handleSizeReqChange single wenv newNode Nothing tmpResult

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
    drawStyledAction renderer viewport style $ \_ ->
      handler renderer wenv node
  where
    handler = singleRender single
    useScissor = singleUseScissor single
    style = singleGetActiveStyle single wenv node
    viewport = node ^. L.info . L.viewport
