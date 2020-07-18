{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Handlers (
  HandlerStep,
  handleWidgetResult,
  handleSystemEvents,
  handleWidgetInit
) where

import Control.Concurrent.Async (async)
import Control.Lens (use, (.=))
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan)
import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Sequence (Seq(..), (><))

import qualified Data.Sequence as Seq
import qualified SDL

import Monomer.Event.Core
import Monomer.Event.Keyboard
import Monomer.Event.Types
import Monomer.Main.Types
import Monomer.Main.Util
import Monomer.Graphics.Renderer
import Monomer.Widget.PathContext
import Monomer.Widget.Types
import Monomer.Widget.Util

type HandlerStep s e = (WidgetContext s e, Seq e, WidgetInstance s e)

createEventContext :: WidgetContext s e -> Maybe Path -> Maybe Path -> Path -> Path -> SystemEvent -> WidgetInstance s e -> Maybe PathContext
createEventContext wctx latestPressed activeOverlay currentFocus currentTarget systemEvent widgetRoot = case systemEvent of
    -- Keyboard
    KeyAction{}            -> pathEvent currentTarget
    TextInput _            -> pathEvent currentTarget
    -- Clipboard
    Clipboard _            -> pathEvent currentTarget
    -- Mouse/touch
    ButtonAction point _ _ -> pointEvent point
    Click point _          -> pointEvent point
    WheelScroll point _ _  -> pointEvent point
    Focus                  -> pathEvent currentTarget
    Blur                   -> pathEvent currentTarget
    Enter point            -> pointEvent point
    Move point             -> pointEvent point
    Leave oldPath _        -> pathEvent oldPath
  where
    pathEvent = Just . makePathCtx
    findStartPath = fromMaybe rootPath activeOverlay
    pathFromPoint point = _widgetFind (_instanceWidget widgetRoot) wctx findStartPath point widgetRoot
    pointEvent point = makePathCtx <$> (pathFromPoint point <|> activeOverlay <|> latestPressed)
    makePathCtx targetPath = PathContext currentFocus targetPath rootPath

handleSystemEvents :: (MonomerM s m) => Renderer m -> WidgetContext s e -> [SystemEvent] -> WidgetInstance s e -> m (HandlerStep s e)
handleSystemEvents renderer wctx systemEvents widgetRoot = foldM reducer (wctx, Seq.empty, widgetRoot) systemEvents where
  reducer (currWctx, currEvents, currWidgetRoot) systemEvent = do
    currentFocus <- use focused

    (wctx2, evts2, wroot2) <- handleSystemEvent renderer currWctx systemEvent currentFocus currentFocus currWidgetRoot
    return (wctx2, currEvents >< evts2, wroot2)

handleSystemEvent :: (MonomerM s m) => Renderer m -> WidgetContext s e -> SystemEvent -> Path -> Path -> WidgetInstance s e -> m (HandlerStep s e)
handleSystemEvent renderer wctx systemEvent currentFocus currentTarget widgetRoot = do
  latestPressed <- use latestPressed
  activeOverlay <- use activeOverlay

  case createEventContext wctx latestPressed activeOverlay currentFocus currentTarget systemEvent widgetRoot of
    Nothing -> return (wctx, Seq.empty, widgetRoot)
    Just ctx -> do
      let widget = _instanceWidget widgetRoot
      let emptyResult = WidgetResult Seq.empty Seq.empty widgetRoot
      let widgetResult = fromMaybe emptyResult $ _widgetHandleEvent widget wctx ctx systemEvent widgetRoot
      let stopProcessing = isJust $ Seq.findIndexL isIgnoreParentEvents (_resultRequests widgetResult)

      handleWidgetResult renderer wctx ctx widgetResult
        >>= handleFocusChange renderer ctx systemEvent stopProcessing

handleWidgetInit :: (MonomerM s m) => Renderer m -> WidgetContext s e -> WidgetInstance s e -> m (HandlerStep s e)
handleWidgetInit renderer wctx widgetRoot = do
  let widget = _instanceWidget widgetRoot
  let ctx = PathContext rootPath rootPath rootPath
  let widgetResult = _widgetInit widget wctx ctx widgetRoot

  handleWidgetResult renderer wctx ctx widgetResult

handleWidgetResult :: (MonomerM s m) => Renderer m -> WidgetContext s e -> PathContext -> WidgetResult s e -> m (HandlerStep s e)
handleWidgetResult renderer wctx ctx (WidgetResult reqs events evtRoot) = do
  let evtUpdates = getUpdateUserStates reqs
  let evtModel = foldr (.) id evtUpdates (_wcModel wctx)
  let evtWctx = wctx { _wcModel = evtModel }

  handleNewWidgetTasks reqs

  handleFocusSet renderer reqs (evtWctx, events, evtRoot)
    >>= handleClipboardGet renderer ctx reqs
    >>= handleClipboardSet renderer reqs
    >>= handleSendMessages renderer reqs
    >>= handleOverlaySet renderer reqs
    >>= handleOverlayReset renderer reqs

handleFocusChange :: (MonomerM s m) => Renderer m -> PathContext -> SystemEvent -> Bool -> HandlerStep s e -> m (HandlerStep s e)
handleFocusChange renderer ctx systemEvent stopProcessing (model, events, widgetRoot)
  | focusChangeRequested = do
      oldFocus <- use focused
      (newWctx, newEvents1, newRoot1) <- handleSystemEvent renderer model Blur oldFocus oldFocus widgetRoot

      let newFocus = findNextFocusable newWctx oldFocus widgetRoot
      (newWctx2, newEvents2, newRoot2) <- handleSystemEvent renderer newWctx Focus newFocus newFocus newRoot1
      focused .= newFocus

      return (newWctx2, events >< newEvents1 >< newEvents2, widgetRoot)
  | otherwise = return (model, events, widgetRoot)
  where
    focusChangeRequested = not stopProcessing && isKeyPressed systemEvent keyTab

handleFocusSet :: (MonomerM s m) => Renderer m -> Seq (WidgetRequest s) -> HandlerStep s e -> m (HandlerStep s e)
handleFocusSet renderer reqs previousStep =
  case Seq.filter isSetFocus reqs of
    SetFocus newFocus :<| _ -> do
      focused .= newFocus

      return previousStep
    _ -> return previousStep

handleClipboardGet :: (MonomerM s m) => Renderer m -> PathContext -> Seq (WidgetRequest s) -> HandlerStep s e -> m (HandlerStep s e)
handleClipboardGet renderer ctx reqs previousStep = do
    hasText <- SDL.hasClipboardText
    contents <- if hasText
                  then fmap ClipboardText SDL.getClipboardText
                  else return ClipboardEmpty

    foldM (reducer contents) previousStep reqs
  where
    reducer contents (model, events, widgetRoot) (GetClipboard path) = do
      (newWctx2, newEvents2, newRoot2) <- handleSystemEvent renderer model (Clipboard contents) (_pathCurrent ctx) path widgetRoot

      return (newWctx2, events >< newEvents2, newRoot2)
    reducer contents previousStep _ = return previousStep

handleClipboardSet :: (MonomerM s m) => Renderer m -> Seq (WidgetRequest s) -> HandlerStep s e -> m (HandlerStep s e)
handleClipboardSet renderer reqs previousStep =
  case Seq.filter isSetClipboard reqs of
    SetClipboard (ClipboardText text) :<| _ -> do
      SDL.setClipboardText text

      return previousStep
    _ -> return previousStep

handleOverlaySet :: (MonomerM s m) => Renderer m -> Seq (WidgetRequest s) -> HandlerStep s e -> m (HandlerStep s e)
handleOverlaySet renderer reqs previousStep =
  case Seq.filter isSetOverlay reqs of
    SetOverlay path :<| _ -> do
      activeOverlay .= Just path

      return previousStep
    _ -> return previousStep

handleOverlayReset :: (MonomerM s m) => Renderer m -> Seq (WidgetRequest s) -> HandlerStep s e -> m (HandlerStep s e)
handleOverlayReset renderer reqs previousStep =
  case Seq.filter isSetOverlay reqs of
    ResetOverlay :<| _ -> do
      activeOverlay .= Nothing

      return previousStep
    _ -> return previousStep

handleSendMessages :: (MonomerM s m) => Renderer m -> Seq (WidgetRequest s) -> HandlerStep s e -> m (HandlerStep s e)
handleSendMessages renderer reqs previousStep = foldM reducer previousStep reqs where
  reducer previousStep (SendMessage path message) = do
    currentFocus <- use focused

    let (wctx, events, widgetRoot) = previousStep
    let ctx = PathContext currentFocus path rootPath
    let emptyResult = WidgetResult Seq.empty Seq.empty widgetRoot
    let widgetResult = fromMaybe emptyResult $ _widgetHandleMessage (_instanceWidget widgetRoot) wctx ctx message widgetRoot

    (newWctx, newEvents, newWidgetRoot) <- handleWidgetResult renderer wctx ctx widgetResult

    return (newWctx, events >< newEvents, newWidgetRoot)
  reducer previousStep _ = return previousStep

handleNewWidgetTasks :: (MonomerM s m) => Seq (WidgetRequest s) -> m ()
handleNewWidgetTasks reqs = do
  let taskHandlers = Seq.filter isTaskHandler reqs
  let producerHandlers = Seq.filter isProducerHandler reqs

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
