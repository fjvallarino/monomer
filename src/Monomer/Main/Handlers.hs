{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Handlers where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Lens.Micro.Mtl
import SDL (($=))

import qualified Data.List as L
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL

import Monomer.Common.Core
import Monomer.Common.Event
import Monomer.Common.Keyboard
import Monomer.Common.Types
import Monomer.Common.Util
import Monomer.Data.Tree
import Monomer.Main.Core
import Monomer.Main.Platform
import Monomer.Main.UserTask
import Monomer.Main.WidgetTask
import Monomer.Main.Util

handleEvent :: (MonomerM s e m) => Renderer m -> s -> SystemEvent -> Path -> WidgetNode s e m -> ChildEventResult s e m
handleEvent renderer app systemEvent targetPath widgets = case systemEvent of
  -- Keyboard
  KeyAction _ _ _       -> handleEventFromPath app targetPath widgets systemEvent
  TextInput _           -> handleEventFromPath app targetPath widgets systemEvent
  -- Clipboard
  Clipboard _           -> handleEventFromPath app targetPath widgets systemEvent
  -- Mouse/touch
  Click point _ _       -> handleEventFromPoint app point widgets systemEvent
  WheelScroll point _ _ -> handleEventFromPoint app point widgets systemEvent
  Focus                 -> handleEventFromPath app targetPath widgets systemEvent
  Blur                  -> handleEventFromPath app targetPath widgets systemEvent
  Enter point           -> handleEventFromPoint app point widgets systemEvent
  Move point            -> handleEventFromPoint app point widgets systemEvent
  Leave oldPath _       -> handleEventFromPath app oldPath widgets systemEvent

handleSystemEvents :: (MonomerM s e m) => Renderer m -> MonomerApp s e m -> s -> [SystemEvent] -> WidgetNode s e m -> m (WidgetNode s e m)
handleSystemEvents renderer mapp app systemEvents widgets = do
  foldM (\newWidgets event -> do
    focus <- getCurrentFocus
    handleSystemEvent renderer mapp app event focus newWidgets) widgets systemEvents

handleSystemEvent :: (MonomerM s e m) => Renderer m -> MonomerApp s e m -> s -> SystemEvent -> Path -> WidgetNode s e m -> m (WidgetNode s e m)
handleSystemEvent renderer mapp app systemEvent currentFocus widgets = do
  let (ChildEventResult stopProcessing eventRequests appEvents newWidgets newStates) = handleEvent renderer app systemEvent currentFocus widgets
  let newRoot = fromMaybe widgets newWidgets

  appContext %= compose newStates
  launchWidgetTasks eventRequests

  handleAppEvents mapp appEvents
    >>  handleFocusChange renderer mapp currentFocus systemEvent stopProcessing newRoot
    >>= handleClipboardGet renderer mapp eventRequests
    >>= handleClipboardSet renderer eventRequests
    >>= handleResizeChildren renderer mapp eventRequests

handleFocusChange :: (MonomerM s e m) => Renderer m -> MonomerApp s e m -> Path -> SystemEvent -> Bool -> WidgetNode s e m -> m (WidgetNode s e m)
handleFocusChange renderer mapp currentFocus systemEvent stopProcessing widgetRoot
  | focusChangeRequested = do
      ring <- use focusRing
      app <- use appContext
      oldFocus <- getCurrentFocus
      newRoot1 <- handleSystemEvent renderer mapp app Blur oldFocus widgetRoot
      focusRing .= rotate ring
      newApp <- use appContext
      newFocus <- getCurrentFocus
      newRoot2 <- handleSystemEvent renderer mapp newApp Focus newFocus newRoot1
      return $ setFocusedStatus newFocus True (setFocusedStatus currentFocus False newRoot2)
  | otherwise = return widgetRoot
  where
    focusChangeRequested = not stopProcessing && isKeyPressed systemEvent keyTab
    rotate = if isShiftPressed systemEvent then inverseRotateList else rotateList

handleResizeChildren :: (MonomerM s e m) => Renderer m -> MonomerApp s e m -> [(Path, EventRequest)] -> WidgetNode s e m -> m (WidgetNode s e m)
handleResizeChildren renderer mapp eventRequests widgetRoot =
  case L.find (\(path, evt) -> isResizeChildren evt) eventRequests of
    Just (path, event) -> do
      windowSize <- use windowSize
      app <- use appContext

      resizeUI renderer app windowSize widgetRoot
    Nothing -> return widgetRoot

handleClipboardGet :: (MonomerM s e m) => Renderer m -> MonomerApp s e m -> [(Path, EventRequest)] -> WidgetNode s e m -> m (WidgetNode s e m)
handleClipboardGet renderer mapp eventRequests widgetRoot =
  case L.find (\(path, evt) -> isGetClipboard evt) eventRequests of
    Just (path, event) -> do
      app <- use appContext
      hasText <- SDL.hasClipboardText
      contents <- if hasText then fmap ClipboardText SDL.getClipboardText else return ClipboardEmpty

      handleSystemEvent renderer mapp app (Clipboard contents) path widgetRoot
    Nothing -> return widgetRoot

handleClipboardSet :: (MonomerM s e m) => Renderer m -> [(Path, EventRequest)] -> WidgetNode s e m -> m (WidgetNode s e m)
handleClipboardSet renderer eventRequests widgetRoot =
  case L.find (\(path, evt) -> isSetClipboard evt) eventRequests of
    Just (path, SetClipboard (ClipboardText text)) -> do
      SDL.setClipboardText text

      return widgetRoot
    Just _ -> return widgetRoot
    Nothing -> return widgetRoot

handleWindowResize :: (MonomerM s e m) => SDL.Window -> Renderer m -> WidgetNode s e m -> m (WidgetNode s e m)
handleWindowResize window renderer widgets = do
  app <- use appContext
  dpr <- use devicePixelRate
  Rect rx ry rw rh <- getWindowSize window

  let newWindowSize = Rect rx ry (rw / dpr) (rh / dpr)

  windowSize .= newWindowSize

  liftIO $ GL.viewport GL.$= (GL.Position 0 0, GL.Size (round rw) (round rh))

  resizeUI renderer app newWindowSize widgets
