{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Handlers (
  HandlerStep,
  handleEventResult,
  handleSystemEvents
) where

import Control.Concurrent.Async (async)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan)
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Sequence (Seq(..), (><))
import Lens.Micro.Mtl

import qualified Data.List as L
import qualified Data.Sequence as Seq
import qualified SDL

import Monomer.Common.Geometry
import Monomer.Event.Core
import Monomer.Event.Keyboard
import Monomer.Event.Types
import Monomer.Main.Platform
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

handleSystemEvents :: (MonomerM s m) => Renderer m -> s -> [SystemEvent] -> WidgetInstance s e m -> m (HandlerStep s e m)
handleSystemEvents renderer app systemEvents widgetRoot = foldM reducer (app, Seq.empty, widgetRoot) systemEvents where
  reducer (currApp, currAppEvents, currWidgetRoot) systemEvent = do
    currentFocus <- use focused

    (ca2, as2, ws2) <- handleSystemEvent renderer currApp systemEvent currentFocus currentFocus currWidgetRoot
    return (ca2, currAppEvents >< as2, ws2)

handleSystemEvent :: (MonomerM s m) => Renderer m -> s -> SystemEvent -> Path -> Path -> WidgetInstance s e m -> m (HandlerStep s e m)
handleSystemEvent renderer app systemEvent currentFocus currentTarget widgetRoot = case createEventContext currentFocus currentTarget systemEvent widgetRoot of
  Nothing -> return (app, Seq.empty, widgetRoot)
  Just ctx -> do
    let widget = _instanceWidget widgetRoot
    let emptyResult = EventResult Seq.empty Seq.empty widgetRoot
    let eventResult = fromMaybe emptyResult $ _widgetHandleEvent widget ctx systemEvent app widgetRoot
    let stopProcessing = isJust $ Seq.findIndexL isIgnoreParentEvents (_eventResultRequest eventResult)

    handleEventResult renderer ctx app eventResult
      >>= handleFocusChange renderer ctx systemEvent stopProcessing

handleEventResult :: (MonomerM s m) => Renderer m -> PathContext -> s -> EventResult s e m -> m (HandlerStep s e m)
handleEventResult renderer ctx app (EventResult eventRequests appEvents evtRoot) = do
  let evtStates = getUpdateUserStates eventRequests
  let evtApp = compose evtStates app

  handleNewWidgetTasks eventRequests

  handleFocusSet renderer eventRequests (evtApp, appEvents, evtRoot)
    >>= handleClipboardGet renderer ctx eventRequests
    >>= handleClipboardSet renderer eventRequests

handleFocusChange :: (MonomerM s m) => Renderer m -> PathContext -> SystemEvent -> Bool -> (HandlerStep s e m) -> m (HandlerStep s e m)
handleFocusChange renderer ctx systemEvent stopProcessing (app, events, widgetRoot)
  | focusChangeRequested = do
      oldFocus <- use focused
      (newApp1, newEvents1, newRoot1) <- handleSystemEvent renderer app Blur oldFocus oldFocus widgetRoot

      let newFocus = findNextFocusable oldFocus widgetRoot
      (newApp2, newEvents2, newRoot2) <- handleSystemEvent renderer newApp1 Focus newFocus newFocus newRoot1
      focused .= newFocus

      return (newApp2, events >< newEvents1 >< newEvents2, widgetRoot)
  | otherwise = return (app, events, widgetRoot)
  where
    focusChangeRequested = not stopProcessing && isKeyPressed systemEvent keyTab

handleFocusSet :: (MonomerM s m) => Renderer m -> Seq (EventRequest s) -> (HandlerStep s e m) -> m (HandlerStep s e m)
handleFocusSet renderer eventRequests previousStep =
  case Seq.filter isSetFocus eventRequests of
    SetFocus newFocus :<| _ -> do
      focused .= newFocus

      return previousStep
    _ -> return previousStep

handleClipboardGet :: (MonomerM s m) => Renderer m -> PathContext -> Seq (EventRequest s) -> (HandlerStep s e m) -> m (HandlerStep s e m)
handleClipboardGet renderer ctx eventRequests (app, events, widgetRoot) =
  case Seq.filter isGetClipboard eventRequests of
    GetClipboard path :<| _ -> do
      hasText <- SDL.hasClipboardText
      contents <- if hasText then fmap ClipboardText SDL.getClipboardText else return ClipboardEmpty

      (newApp2, newEvents2, newRoot2) <- handleSystemEvent renderer app (Clipboard contents) (_pathCurrent ctx) path widgetRoot

      return (newApp2, events >< newEvents2, newRoot2)
    _ -> return (app, events, widgetRoot)

handleClipboardSet :: (MonomerM s m) => Renderer m -> Seq (EventRequest s) -> (HandlerStep s e m) -> m (HandlerStep s e m)
handleClipboardSet renderer eventRequests previousStep =
  case Seq.filter isSetClipboard eventRequests of
    SetClipboard (ClipboardText text) :<| _ -> do
      SDL.setClipboardText text

      return previousStep
    _ -> return previousStep

handleNewWidgetTasks :: (MonomerM s m) => Seq (EventRequest s) -> m ()
handleNewWidgetTasks eventRequests = do
  let customHandlers = Seq.filter isCustomHandler eventRequests
  let producerHandlers = Seq.filter isProducerHandler eventRequests

  customTasks <- forM customHandlers $ \(RunCustom path handler) -> do
    asyncTask <- liftIO $ async (liftIO handler)
    return $ WidgetTask path asyncTask

  producerTasks <- forM producerHandlers $ \(RunProducer adapter path handler) -> do
    newChannel <- liftIO newTChanIO
    asyncTask <- liftIO $ async (liftIO $ handler (sendMessage adapter newChannel))
    return $ WidgetProducer path newChannel asyncTask

  previousTasks <- use widgetTasks
  widgetTasks .= previousTasks >< customTasks >< producerTasks

sendMessage :: (e -> a) -> TChan a -> e -> IO ()
sendMessage adapter channel message = atomically $ writeTChan channel (adapter message)
