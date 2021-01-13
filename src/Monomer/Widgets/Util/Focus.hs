module Monomer.Widgets.Util.Focus (
  parentPath,
  nextTargetStep,
  isFocused,
  isFocusCandidate,
  isTargetReached,
  isTargetValid,
  isWidgetParentOfPath,
  isWidgetBeforePath,
  isWidgetAfterPath,
  handleFocusRequest,
  handleFocusChange
) where

import Control.Lens ((&), (^.), (.~), (%~))
import Data.Maybe
import Data.Sequence (Seq(..), (|>))

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event.Types
import Monomer.Widgets.Util.Hover
import Monomer.Widgets.Util.Widget

import qualified Monomer.Lens as L

parentPath :: WidgetNode s e -> Path
parentPath node = Seq.take (Seq.length path - 1) path where
  path = node ^. L.info . L.path

nextTargetStep :: Path -> WidgetNode s e -> Maybe PathStep
nextTargetStep target node = nextStep where
  currentPath = node ^. L.info . L.path
  nextStep = Seq.lookup (Seq.length currentPath) target

isFocused :: WidgetEnv s e -> WidgetNode s e -> Bool
isFocused wenv node = wenv ^. L.focusedPath == node ^. L.info . L.path

isFocusCandidate :: FocusDirection -> Path -> WidgetNode s e -> Bool
isFocusCandidate FocusFwd = isFocusFwdCandidate
isFocusCandidate FocusBwd = isFocusBwdCandidate

isFocusFwdCandidate :: Path -> WidgetNode s e -> Bool
isFocusFwdCandidate startFrom node = isValid where
  info = node ^. L.info
  isAfter = isWidgetAfterPath startFrom node
  isFocusable = info ^. L.focusable
  isEnabled = info ^. L.visible && info ^. L.enabled
  isValid = isAfter && isFocusable && isEnabled

isFocusBwdCandidate :: Path -> WidgetNode s e -> Bool
isFocusBwdCandidate startFrom node = isValid where
  info = node ^. L.info
  isBefore = isWidgetBeforePath startFrom node
  isFocusable = info ^. L.focusable
  isEnabled = info ^. L.visible && info ^. L.enabled
  isValid = isBefore && isFocusable && isEnabled

isTargetReached :: Path -> WidgetNode s e -> Bool
isTargetReached target node = target == node ^. L.info . L.path

isTargetValid :: Path -> WidgetNode s e -> Bool
isTargetValid target node = valid where
  children = node ^. L.children
  valid = case nextTargetStep target node of
    Just step -> step < Seq.length children
    Nothing -> False

isWidgetParentOfPath :: Path -> WidgetNode s e -> Bool
isWidgetParentOfPath path node = result where
  widgetPath = node ^. L.info . L.path
  lenWidgetPath = Seq.length widgetPath
  pathPrefix = Seq.take lenWidgetPath path
  result = widgetPath == pathPrefix

isWidgetAfterPath :: Path -> WidgetNode s e -> Bool
isWidgetAfterPath path node = result where
  widgetPath = node ^. L.info . L.path
  lenPath = Seq.length path
  lenWidgetPath = Seq.length widgetPath
  widgetPathPrefix = Seq.take lenPath widgetPath
  result
    | lenWidgetPath > lenPath = path <= widgetPathPrefix
    | otherwise = path < widgetPath

isWidgetBeforePath :: Path -> WidgetNode s e -> Bool
isWidgetBeforePath path node = result where
  widgetPath = node ^. L.info . L.path
  result
    | path == emptyPath = True
    | otherwise = path > widgetPath

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
