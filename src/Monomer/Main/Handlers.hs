{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Handlers where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Sequence (Seq(..), (<|), (|>), (><))
import Lens.Micro.Mtl

import qualified Data.List as L
import qualified Data.Sequence as Seq
import qualified SDL

import Monomer.Common.Types
import Monomer.Common.Util
import Monomer.Common.Tree
import Monomer.Event.Core
import Monomer.Event.Keyboard
import Monomer.Event.Types
import Monomer.Main.Internal
import Monomer.Main.Platform
import Monomer.Main.UserTask
import Monomer.Main.WidgetTask
import Monomer.Main.Types
import Monomer.Main.Util
import Monomer.Graphics.Renderer
import Monomer.Widget.Core
import Monomer.Widget.PathContext
import Monomer.Widget.Types

type HandlerStep s e m = (s, Seq e, WidgetInstance s e m)

createEventContext :: Path -> Path -> SystemEvent -> WidgetInstance s e m -> Maybe PathContext
createEventContext currentFocus currentTarget systemEvent widgetRoot = case systemEvent of
    -- Keyboard
    KeyAction _ _ _       -> pathEvent currentTarget
    TextInput _           -> pathEvent currentTarget
    -- Clipboard
    Clipboard _           -> pathEvent currentTarget
    -- Mouse/touch
    Click point _ _       -> pointEvent point
    WheelScroll point _ _ -> pointEvent point
    Focus                 -> pathEvent currentTarget
    Blur                  -> pathEvent currentTarget
    Enter point           -> pointEvent point
    Move point            -> pointEvent point
    Leave oldPath _       -> pathEvent oldPath
  where
    pathEvent = Just . makePathCtx
    pointEvent point = fmap makePathCtx $ _widgetFind (_instanceWidget widgetRoot) point widgetRoot
    makePathCtx targetPath = PathContext currentFocus targetPath rootPath

handleSystemEvents :: (MonomerM s e m) => Renderer m -> MonomerApp s e m -> s -> [SystemEvent] -> WidgetInstance s e m -> m (HandlerStep s e m)
handleSystemEvents renderer mapp app systemEvents widgetRoot = foldM reducer (app, Seq.empty, widgetRoot) systemEvents where
  reducer (currApp, currAppEvents, currWidgetRoot) systemEvent = do
    currentFocus <- use focused

    (ca2, as2, ws2) <- handleSystemEvent renderer mapp currApp systemEvent currentFocus currentFocus currWidgetRoot
    return (ca2, currAppEvents >< as2, ws2)

handleSystemEvent :: (MonomerM s e m) => Renderer m -> MonomerApp s e m -> s -> SystemEvent -> Path -> Path -> WidgetInstance s e m -> m (HandlerStep s e m)
handleSystemEvent renderer mapp app systemEvent currentFocus currentTarget widgetRoot = case createEventContext currentFocus currentTarget systemEvent widgetRoot of
  Nothing -> return (app, Seq.empty, widgetRoot)
  Just ctx -> do
    let widget = _instanceWidget widgetRoot
    let emptyResult = EventResult Seq.empty Seq.empty widgetRoot
    let EventResult eventRequests appEvents evtRoot = fromMaybe emptyResult $ _widgetHandleEvent widget ctx systemEvent app widgetRoot
    let evtStates = getUpdateUserStates eventRequests
    let stopProcessing = isJust $ Seq.findIndexL isIgnoreParentEvents eventRequests
    let evtApp = compose evtStates app

    launchWidgetTasks eventRequests

    (newApp, newAppEvents, newRoot) <- handleFocusChange renderer mapp ctx systemEvent stopProcessing (evtApp, Seq.empty, evtRoot)
      >>= handleClipboardGet renderer mapp ctx eventRequests
      >>= handleClipboardSet renderer eventRequests
      >>= handleResizeChildren renderer mapp ctx eventRequests

    return (newApp, appEvents >< newAppEvents, newRoot)

handleFocusChange :: (MonomerM s e m) => Renderer m -> MonomerApp s e m -> PathContext -> SystemEvent -> Bool -> (HandlerStep s e m) -> m (HandlerStep s e m)
handleFocusChange renderer mapp ctx systemEvent stopProcessing (app, events, widgetRoot)
  | focusChangeRequested = do
      oldFocus <- use focused
      (newApp1, newEvents1, newRoot1) <- handleSystemEvent renderer mapp app Blur oldFocus oldFocus widgetRoot

      let newFocus = findNextFocusable oldFocus widgetRoot
      (newApp2, newEvents2, newRoot2) <- handleSystemEvent renderer mapp newApp1 Focus newFocus newFocus newRoot1
      focused .= newFocus

      return (newApp2, events >< newEvents1 >< newEvents2, widgetRoot)
  | otherwise = return (app, events, widgetRoot)
  where
    focusChangeRequested = not stopProcessing && isKeyPressed systemEvent keyTab
    rotate = if isShiftPressed systemEvent then inverseRotateSeq else rotateSeq

handleResizeChildren :: (MonomerM s e m) => Renderer m -> MonomerApp s e m -> PathContext -> Seq (EventRequest s) -> (HandlerStep s e m) -> m (HandlerStep s e m)
handleResizeChildren renderer mapp ctx eventRequests (app, widgetRoot, events) =
  case Seq.filter isResizeChildren eventRequests of
    ResizeChildren path :<| _ -> do
      windowSize <- use windowSize
      --newRoot <- resizeUI renderer app windowSize widgetRoot
      newRoot <- return widgetRoot

      return (app, newRoot, events)
    _ -> return (app, widgetRoot, events)

handleClipboardGet :: (MonomerM s e m) => Renderer m -> MonomerApp s e m -> PathContext -> Seq (EventRequest s) -> (HandlerStep s e m) -> m (HandlerStep s e m)
handleClipboardGet renderer mapp ctx eventRequests (app, events, widgetRoot) =
  case Seq.filter isGetClipboard eventRequests of
    GetClipboard path :<| _ -> do
      hasText <- SDL.hasClipboardText
      contents <- if hasText then fmap ClipboardText SDL.getClipboardText else return ClipboardEmpty

      handleSystemEvent renderer mapp app (Clipboard contents) (_pathCurrent ctx) path widgetRoot
    _ -> return (app, events, widgetRoot)

handleClipboardSet :: (MonomerM s e m) => Renderer m -> Seq (EventRequest s) -> (HandlerStep s e m) -> m (HandlerStep s e m)
handleClipboardSet renderer eventRequests (app, events, widgetRoot) =
  case Seq.filter isSetClipboard eventRequests of
    SetClipboard (ClipboardText text) :<| _ -> do
      SDL.setClipboardText text

      return (app, events, widgetRoot)
    _ -> return (app, events, widgetRoot)
