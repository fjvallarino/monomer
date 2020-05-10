{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Handlers where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Lens.Micro.Mtl

import qualified Data.List as L
import qualified SDL

import Monomer.Common.Core
import Monomer.Common.Keyboard
import Monomer.Common.Types
import Monomer.Common.Util
import Monomer.Data.Tree
import Monomer.Event.Core
import Monomer.Event.Types
import Monomer.Main.Platform
import Monomer.Main.UserTask
import Monomer.Main.WidgetTask
import Monomer.Main.Util
import Monomer.Graphics.Renderer

type HandlerStep s e m = (s, WidgetNode s e m, [e])

handleEvent :: (MonomerM s e m) => Renderer m -> s -> SystemEvent -> Path -> WidgetNode s e m -> ChildEventResult s e m
handleEvent renderer app systemEvent targetPath widgetRoot = case systemEvent of
  -- Keyboard
  KeyAction _ _ _       -> handleEventFromPath app targetPath widgetRoot systemEvent
  TextInput _           -> handleEventFromPath app targetPath widgetRoot systemEvent
  -- Clipboard
  Clipboard _           -> handleEventFromPath app targetPath widgetRoot systemEvent
  -- Mouse/touch
  Click point _ _       -> handleEventFromPoint app point widgetRoot systemEvent
  WheelScroll point _ _ -> handleEventFromPoint app point widgetRoot systemEvent
  Focus                 -> handleEventFromPath app targetPath widgetRoot systemEvent
  Blur                  -> handleEventFromPath app targetPath widgetRoot systemEvent
  Enter point           -> handleEventFromPoint app point widgetRoot systemEvent
  Move point            -> handleEventFromPoint app point widgetRoot systemEvent
  Leave oldPath _       -> handleEventFromPath app oldPath widgetRoot systemEvent

handleSystemEvents :: (MonomerM s e m) => Renderer m -> MonomerApp s e m -> s -> [SystemEvent] -> WidgetNode s e m -> m (HandlerStep s e m)
handleSystemEvents renderer mapp app systemEvents widgetRoot = foldM reducer (app, widgetRoot, []) systemEvents where
  reducer (currApp, currWidgetRoot, currAppEvents) systemEvent = do
    focus <- getCurrentFocus
    (ca2, ws2, as2) <- handleSystemEvent renderer mapp currApp systemEvent focus currWidgetRoot
    return (ca2, ws2, currAppEvents ++ as2)

handleSystemEvent :: (MonomerM s e m) => Renderer m -> MonomerApp s e m -> s -> SystemEvent -> Path -> WidgetNode s e m -> m (HandlerStep s e m)
handleSystemEvent renderer mapp app systemEvent currentFocus widgetRoot = do
  let (ChildEventResult stopProcessing eventRequests appEvents newWidgets newStates) = handleEvent renderer app systemEvent currentFocus widgetRoot
  let tempRoot = fromMaybe widgetRoot newWidgets
  let tempApp = compose newStates app

  launchWidgetTasks eventRequests

  (newApp, newRoot, newAppEvents) <- handleFocusChange renderer mapp currentFocus systemEvent stopProcessing (tempApp, tempRoot, [])
    >>= handleClipboardGet renderer mapp eventRequests
    >>= handleClipboardSet renderer eventRequests
    >>= handleResizeChildren renderer mapp eventRequests

  return (newApp, newRoot, appEvents ++ newAppEvents)

handleFocusChange :: (MonomerM s e m) => Renderer m -> MonomerApp s e m -> Path -> SystemEvent -> Bool -> (HandlerStep s e m) -> m (HandlerStep s e m)
handleFocusChange renderer mapp currentFocus systemEvent stopProcessing (app, widgetRoot, events)
  | focusChangeRequested = do
      ring <- use focusRing
      oldFocus <- getCurrentFocus
      (newApp1, newRoot1, newEvents1) <- handleSystemEvent renderer mapp app Blur oldFocus widgetRoot
      focusRing .= rotate ring
      newFocus <- getCurrentFocus
      (newApp2, newRoot2, newEvents2) <- handleSystemEvent renderer mapp newApp1 Focus newFocus newRoot1

      let newFocusedRoot = setFocusedStatus newFocus True (setFocusedStatus currentFocus False newRoot2)

      return (newApp2, newFocusedRoot, events ++ newEvents1 ++ newEvents2)
  | otherwise = return (app, widgetRoot, events)
  where
    focusChangeRequested = not stopProcessing && isKeyPressed systemEvent keyTab
    rotate = if isShiftPressed systemEvent then inverseRotateList else rotateList

handleResizeChildren :: (MonomerM s e m) => Renderer m -> MonomerApp s e m -> [(Path, EventRequest)] -> (HandlerStep s e m) -> m (HandlerStep s e m)
handleResizeChildren renderer mapp eventRequests (app, widgetRoot, events) =
  case L.find (\(path, evt) -> isResizeChildren evt) eventRequests of
    Just (path, event) -> do
      windowSize <- use windowSize
      newRoot <- resizeUI renderer app windowSize widgetRoot

      return (app, newRoot, events)
    Nothing -> return (app, widgetRoot, events)

handleClipboardGet :: (MonomerM s e m) => Renderer m -> MonomerApp s e m -> [(Path, EventRequest)] -> (HandlerStep s e m) -> m (HandlerStep s e m)
handleClipboardGet renderer mapp eventRequests (app, widgetRoot, events) =
  case L.find (\(path, evt) -> isGetClipboard evt) eventRequests of
    Just (path, event) -> do
      hasText <- SDL.hasClipboardText
      contents <- if hasText then fmap ClipboardText SDL.getClipboardText else return ClipboardEmpty

      handleSystemEvent renderer mapp app (Clipboard contents) path widgetRoot
    Nothing -> return (app, widgetRoot, events)

handleClipboardSet :: (MonomerM s e m) => Renderer m -> [(Path, EventRequest)] -> (HandlerStep s e m) -> m (HandlerStep s e m)
handleClipboardSet renderer eventRequests (app, widgetRoot, events) =
  case L.find (\(path, evt) -> isSetClipboard evt) eventRequests of
    Just (path, SetClipboard (ClipboardText text)) -> do
      SDL.setClipboardText text

      return (app, widgetRoot, events)
    _ -> return (app, widgetRoot, events)
