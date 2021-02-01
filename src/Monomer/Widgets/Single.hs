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

import Codec.Serialise
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

type SingleRestoreHandler s e a
  = WidgetEnv s e
  -> a
  -> WidgetNodeInfo
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
  -> WidgetNode s e
  -> WidgetResult s e

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
  singleMerge :: Maybe (SingleMergeHandler s e a),
  singleRestore :: SingleRestoreHandler s e a,
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
    singleMerge = Nothing,
    singleRestore = defaultRestore,
    singleDispose = defaultDispose,
    singleFindNextFocus = defaultFindNextFocus,
    singleFindByPoint = defaultFindByPoint,
    singleHandleEvent = defaultHandleEvent,
    singleHandleMessage = defaultHandleMessage,
    singleGetSizeReq = defaultGetSizeReq,
    singleResize = defaultResize,
    singleRender = defaultRender
  }

createSingle :: (Typeable a, Serialise a) => a -> Single s e a -> Widget s e
createSingle state single = Widget {
  widgetInit = initWrapper single,
  widgetMerge = mergeWrapper single,
  widgetDispose = singleDispose single,
  widgetGetState = makeState state,
  widgetSave = saveWrapper single,
  widgetRestore = restoreWrapper single,
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
  :: (Typeable a, Serialise a)
  => Single s e a
  -> WidgetEnv s e
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e
mergeWrapper single wenv oldNode newNode = newResult where
  mergeHandler = case singleMerge single of
    Just handler -> handler
    _ -> mergeWithRestore (singleRestore single)
  oldState = widgetGetState (oldNode ^. L.widget) wenv
  oldInfo = oldNode ^. L.info
  nodeHandler styledNode = case useState oldState of
    Just state -> mergeHandler wenv state oldNode styledNode
    _ -> resultWidget styledNode
  tmpResult = loadStateHandler single wenv oldInfo newNode nodeHandler
  newResult = handleWidgetIdChange oldNode tmpResult

mergeWithRestore
  :: SingleRestoreHandler s e a
  -> WidgetEnv s e
  -> a
  -> WidgetNode s e
  -> WidgetNode s e
  -> WidgetResult s e
mergeWithRestore restore wenv oldState oldNode newNode = result where
  info = oldNode ^. L.info
  result = restore wenv oldState info newNode

saveWrapper
  :: (Typeable a, Serialise a)
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

defaultRestore :: SingleRestoreHandler s e a
defaultRestore wenv oldState oldInfo newNode = resultWidget newNode

restoreWrapper
  :: (Typeable a, Serialise a)
  => Single s e a
  -> WidgetEnv s e
  -> WidgetInstanceNode
  -> WidgetNode s e
  -> WidgetResult s e
restoreWrapper single wenv win newNode = newResult where
  restoreHandler = singleRestore single
  oldInfo = win ^. L.info
  nodeHandler styledNode = case loadState (win ^. L.state) of
    Just state -> restoreHandler wenv state oldInfo styledNode
    _ -> resultWidget styledNode
  valid = infoMatches (win ^. L.info) (newNode ^. L.info)
  message = matchFailedMsg (win ^. L.info) (newNode ^. L.info)
  newResult
    | valid = loadStateHandler single wenv oldInfo newNode nodeHandler
    | otherwise = throw (AssertionFailed $ "Restore failed. " ++ message)

loadStateHandler
  :: (Typeable a, Serialise a)
  => Single s e a
  -> WidgetEnv s e
  -> WidgetNodeInfo
  -> WidgetNode s e
  -> (WidgetNode s e -> WidgetResult s e)
  -> WidgetResult s e
loadStateHandler single wenv oldInfo newNode nodeHandler = newResult where
  getBaseStyle = singleGetBaseStyle single
  tempNode = newNode
    & L.info . L.widgetId .~ oldInfo ^. L.widgetId
    & L.info . L.viewport .~ oldInfo ^. L.viewport
    & L.info . L.sizeReqW .~ oldInfo ^. L.sizeReqW
    & L.info . L.sizeReqH .~ oldInfo ^. L.sizeReqH
  styledNode = initNodeStyle getBaseStyle wenv tempNode
  tmpResult = nodeHandler styledNode
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
  | isVisible && isPointInNodeVp point node = Just path
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
    ButtonAction _ btn PressedBtn _ -> Just btn
    _ -> Nothing
  isFocusReq = btnPressed == Just (wenv ^. L.mainButton)
    && isFocusable
    && not (isNodeFocused wenv node)
    && isNodeTopLevel wenv node
    && isNothing (Seq.findIndexL isFocusRequest prevReqs)
  focusReq = SetFocus (node ^. L.info . L.path)
  newResult
    | isFocusReq && isJust mResult = (& L.requests %~ (|> focusReq)) <$> mResult
    | isFocusReq = Just $ resultReqs node [focusReq]
    | otherwise = mResult

defaultHandleMessage :: SingleMessageHandler s e
defaultHandleMessage wenv target message node = Nothing

handleMessageWrapper :: forall s e a i . (Typeable a, Typeable i)
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
defaultResize wenv viewport node = resultWidget node

resizeHandlerWrapper
  :: Single s e a
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
  result = tmpRes
    & L.node . L.info . L.viewport .~ newVp

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
