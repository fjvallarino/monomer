{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Handlers (
  HandlerStep,
  handleWidgetResult,
  handleSystemEvents,
  handleWidgetInit
) where

import Control.Concurrent.Async (async)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan)
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Sequence (Seq(..), (><))
import Lens.Micro.Mtl

import qualified Data.Sequence as Seq
import qualified SDL

import Monomer.Event.Core
import Monomer.Event.Keyboard
import Monomer.Event.Types
import Monomer.Main.Types
import Monomer.Main.Util
import Monomer.Graphics.Renderer
import Monomer.Widget.Core
import Monomer.Widget.PathContext
import Monomer.Widget.Types
import Monomer.Widget.Util

type HandlerStep s e = (s, Seq e, WidgetInstance s e)

createEventContext :: Path -> Path -> SystemEvent -> WidgetInstance s e -> Maybe PathContext
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

handleSystemEvents :: (MonomerM s m) => Renderer m -> s -> [SystemEvent] -> WidgetInstance s e -> m (HandlerStep s e)
handleSystemEvents renderer app systemEvents widgetRoot = foldM reducer (app, Seq.empty, widgetRoot) systemEvents where
  reducer (currApp, currAppEvents, currWidgetRoot) systemEvent = do
    currentFocus <- use focused

    (ca2, as2, ws2) <- handleSystemEvent renderer currApp systemEvent currentFocus currentFocus currWidgetRoot
    return (ca2, currAppEvents >< as2, ws2)

handleSystemEvent :: (MonomerM s m) => Renderer m -> s -> SystemEvent -> Path -> Path -> WidgetInstance s e -> m (HandlerStep s e)
handleSystemEvent renderer app systemEvent currentFocus currentTarget widgetRoot = case createEventContext currentFocus currentTarget systemEvent widgetRoot of
  Nothing -> return (app, Seq.empty, widgetRoot)
  Just ctx -> do
    let widget = _instanceWidget widgetRoot
    let emptyResult = WidgetResult Seq.empty Seq.empty widgetRoot
    let widgetResult = fromMaybe emptyResult $ _widgetHandleEvent widget ctx systemEvent app widgetRoot
    let stopProcessing = isJust $ Seq.findIndexL isIgnoreParentEvents (_resultRequests widgetResult)

    handleWidgetResult renderer ctx app widgetResult
      >>= handleFocusChange renderer ctx systemEvent stopProcessing

handleWidgetInit :: (MonomerM s m) => Renderer m -> s -> WidgetInstance s e -> m (HandlerStep s e)
handleWidgetInit renderer app widgetRoot = do
  let widget = _instanceWidget widgetRoot
  let ctx = PathContext rootPath rootPath rootPath
  let widgetResult = _widgetInit widget ctx app widgetRoot

  handleWidgetResult renderer ctx app widgetResult

handleWidgetResult :: (MonomerM s m) => Renderer m -> PathContext -> s -> WidgetResult s e -> m (HandlerStep s e)
handleWidgetResult renderer ctx app (WidgetResult eventRequests appEvents evtRoot) = do
  let evtStates = getUpdateUserStates eventRequests
  let evtApp = foldr (.) id evtStates app

  handleNewWidgetTasks eventRequests

  handleFocusSet renderer eventRequests (evtApp, appEvents, evtRoot)
    >>= handleClipboardGet renderer ctx eventRequests
    >>= handleClipboardSet renderer eventRequests
    >>= handleSendMessages renderer eventRequests

handleFocusChange :: (MonomerM s m) => Renderer m -> PathContext -> SystemEvent -> Bool -> (HandlerStep s e) -> m (HandlerStep s e)
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

handleFocusSet :: (MonomerM s m) => Renderer m -> Seq (WidgetRequest s) -> (HandlerStep s e) -> m (HandlerStep s e)
handleFocusSet renderer eventRequests previousStep =
  case Seq.filter isSetFocus eventRequests of
    SetFocus newFocus :<| _ -> do
      focused .= newFocus

      return previousStep
    _ -> return previousStep

handleClipboardGet :: (MonomerM s m) => Renderer m -> PathContext -> Seq (WidgetRequest s) -> (HandlerStep s e) -> m (HandlerStep s e)
handleClipboardGet renderer ctx eventRequests previousStep = do
    hasText <- SDL.hasClipboardText
    contents <- if hasText
                  then fmap ClipboardText SDL.getClipboardText
                  else return ClipboardEmpty

    foldM (reducer contents) previousStep eventRequests
  where
    reducer contents (app, events, widgetRoot) (GetClipboard path) = do
      (newApp2, newEvents2, newRoot2) <- handleSystemEvent renderer app (Clipboard contents) (_pathCurrent ctx) path widgetRoot

      return (newApp2, events >< newEvents2, newRoot2)
    reducer contents previousStep _ = return previousStep

handleClipboardSet :: (MonomerM s m) => Renderer m -> Seq (WidgetRequest s) -> (HandlerStep s e) -> m (HandlerStep s e)
handleClipboardSet renderer eventRequests previousStep =
  case Seq.filter isSetClipboard eventRequests of
    SetClipboard (ClipboardText text) :<| _ -> do
      SDL.setClipboardText text

      return previousStep
    _ -> return previousStep

handleSendMessages :: (MonomerM s m) => Renderer m -> Seq (WidgetRequest s) -> (HandlerStep s e) -> m (HandlerStep s e)
handleSendMessages renderer eventRequests previousStep = foldM reducer previousStep eventRequests where
  reducer previousStep (SendMessage path message) = do
    currentFocus <- use focused

    let (app, events, widgetRoot) = previousStep
    let ctx = PathContext currentFocus path rootPath
    let emptyResult = WidgetResult Seq.empty Seq.empty widgetRoot
    let widgetResult = fromMaybe emptyResult $ _widgetHandleMessage (_instanceWidget widgetRoot) ctx message app widgetRoot

    (newApp, newEvents, newWidgetRoot) <- handleWidgetResult renderer ctx app widgetResult

    return (newApp, events >< newEvents, newWidgetRoot)
  reducer previousStep _ = return previousStep

handleNewWidgetTasks :: (MonomerM s m) => Seq (WidgetRequest s) -> m ()
handleNewWidgetTasks eventRequests = do
  let taskHandlers = Seq.filter isTaskHandler eventRequests
  let producerHandlers = Seq.filter isProducerHandler eventRequests

  singleTasks <- forM taskHandlers $ \(RunTask path handler) -> do
    asyncTask <- liftIO $ async (liftIO handler)
    return $ WidgetTask path asyncTask

  producerTasks <- forM producerHandlers $ \(RunProducer path handler) -> do
    newChannel <- liftIO newTChanIO
    asyncTask <- liftIO $ async (liftIO $ handler (sendMessage newChannel))
    return $ WidgetProducer path newChannel asyncTask

  previousTasks <- use widgetTasks
  widgetTasks .= previousTasks >< singleTasks >< producerTasks

sendMessage :: TChan e -> e -> IO ()
sendMessage channel message = atomically $ writeTChan channel message
