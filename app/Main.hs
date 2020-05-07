{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, poll)
import Control.Exception.Base
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Default
import Data.Maybe
import Data.Typeable
import Foreign.C.Types
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import NanoVG (Context(..), createGL3, CreateFlags(..), createFont, FileName(..), beginFrame, endFrame)
import SDL (($=))
import Unsafe.Coerce

import System.Remote.Monitoring

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as SQ
import qualified Data.Text as T
import qualified Data.Vector.Generic as V
import qualified Foreign.C.String as STR
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL
import qualified SDL.Vect
import qualified SDL.Input.Keyboard as Keyboard
import qualified SDL.Input.Keyboard.Codes as KeyCodes
import qualified SDL.Input.Mouse as Mouse
import qualified SDL.Raw.Error as SRE
import qualified SDL.Raw.Event as SREv
import qualified SDL.Raw.Video as SRV

import Types
import Monomer.Common.Core
import Monomer.Common.Event
import Monomer.Common.Keyboard
import Monomer.Common.Style
import Monomer.Common.Types
import Monomer.Common.Util
import Monomer.Data.Tree
import Monomer.Widgets

import qualified Monomer.Platform.NanoVGRenderer as NV

foreign import ccall unsafe "initGlew" glewInit :: IO CInt

launchUserTasks :: MonomerM a e m => [IO (Maybe e)] -> m ()
launchUserTasks handlers = do
  tasks <- forM handlers $ \handler -> do
    asyncTask <- liftIO $ async handler

    return $ UserTask asyncTask

  previousTasks <- use userTasks
  userTasks .= previousTasks ++ tasks

checkUserTasks :: MonomerM a e m => m [e]
checkUserTasks = do
  tasks <- use userTasks
  (active, finished) <- partitionM (\(UserTask task) -> fmap isNothing (liftIO $ poll task)) tasks
  userTasks .= active

  processUserTaskHandlers finished

processUserTaskHandlers :: MonomerM a e m => [UserTask (Maybe e)] -> m [e]
processUserTaskHandlers tasks = do
  results <- forM tasks stepUserTask
  return $ catMaybes results

stepUserTask :: MonomerM a e m => UserTask (Maybe e) -> m (Maybe e)
stepUserTask (UserTask task) = do
  taskStatus <- liftIO $ poll task

  return $ maybe Nothing processUserTaskHandler taskStatus

processUserTaskHandler :: Either SomeException (Maybe e) -> Maybe e
processUserTaskHandler (Left _) = Nothing
processUserTaskHandler (Right Nothing) = Nothing
processUserTaskHandler (Right evt) = evt

data AppEvent = RunLongTask | PrintTextFields | IncreaseCount Int | UpdateText3 T.Text deriving (Show, Eq)

type AppContext = GUIContext App AppEvent
type AppM = StateT AppContext IO
type WidgetTree = Tree (WidgetInstance App AppEvent AppM)

main :: IO ()
main = do
  --forkServer "localhost" 28000

  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  let customOpenGL = SDL.OpenGLConfig {
    SDL.glColorPrecision = SDL.V4 8 8 8 0,
    SDL.glDepthPrecision = 24,
    SDL.glStencilPrecision = 8,
    SDL.glProfile = SDL.Core SDL.Debug 3 2,
    SDL.glMultisampleSamples = 1
  }

  let (screenWidth, screenHeight) = (640, 480)
      windowHiDPI = True
      useHiDPI = True

  window <-
    SDL.createWindow
      "SDL / OpenGL Example"
      SDL.defaultWindow {
        SDL.windowInitialSize = SDL.V2 screenWidth screenHeight,
        SDL.windowHighDPI = windowHiDPI,
        SDL.windowResizable = True,
        SDL.windowOpenGL = Just customOpenGL
      }

  err <- SRE.getError
  err <- STR.peekCString err
  putStrLn err

  _ <- SDL.glCreateContext window

  _ <- glewInit

  c@(Context c') <- createGL3 (S.fromList [Antialias, StencilStrokes, Debug])

  fontRes <- createFont c "sans" (FileName "./assets/fonts/Roboto-Regular.ttf")

  SREv.startTextInput

  winSize@(Rect rx ry rw rh) <- getWindowSize window

  let devicePixelRate = _rw winSize / fromIntegral screenWidth

  runStateT (runWidgets window c) (initGUIContext def handleAppEvent winSize useHiDPI devicePixelRate)

  putStrLn "About to destroyWindow"
  SDL.destroyWindow window
  SDL.quit

handleAppEvent :: App -> AppEvent -> EventResponse App AppEvent
handleAppEvent app evt = do
  case evt of
    RunLongTask -> Task app $ do
      threadDelay $ 1 * 1000 * 1000

      return $ Just (UpdateText3 "HOLA")
    PrintTextFields -> Task app $ do
      putStrLn $ "Current text 1 is: " ++ show (app ^. textField1)
      putStrLn $ "Current text 2 is: " ++ show (app ^. textField2)
      putStrLn $ "Current text 3 is: " ++ show (app ^. textField3)
      return Nothing
    IncreaseCount v -> Task (app & clickCount %~ (+1)) $ do
      putStrLn $ "Clicked button: " ++ (show v) ++ " - Count is: " ++ show (app ^. clickCount)

      return Nothing
    UpdateText3 txt -> State $ app & textField3 .~ txt

buildUI :: App -> WidgetTree
buildUI model = styledTree where
  border1 = border 5 (rgb 0 255 0) 20
  border2 = borderLeft 20 (rgb 200 200 0) <> borderRight 20 (rgb 200 0 200)
  buttonStyle = bgColor (rgb 0 0 255) <> textSize 64 <> border1 <> border2 <> bgRadius 20
  labelStyle = bgColor (rgb 100 100 100) <> textSize 48
  textStyle = textColor (rgb 0 255 0) <> textAlignH ACenter
  extraWidgets = map (\i -> sandbox (IncreaseCount (10 + i))) [1..(_clickCount model)]
  widgetTree = vstack [
      hstack [
        (scroll $ vstack [
          textField textField1 `style` textStyle,
          spacer `visible` False,
          label "Label 1",
          spacer,
          label "Label 2",
          spacer `visible` False,
          label "Label 3" `visible` False,
          spacer `visible` False,
          label "Label 4",
          spacer,
          label "Label 5",
          spacer,
          label "Label 6",
          spacer,
          label "Label 7",
          spacer,
          label "Label 8",
          spacer,
          label "Label 9",
          spacer,
          label "Label 10",
          spacer,
          label "Label 11",
          spacer,
          label "Label 12"
        ]) `style` (swidth 400 <> sheight 300),
        vstack [
          textField textField2 `style` textStyle,
          scroll $ label "This is a really really really long label, you know?" `style` labelStyle
        ]
      ],
      vgrid ([
        scroll $ hstack [
          label "Short",
          spacer,
          label "Long",
          spacer,
          label "Very Long",
          spacer,
          label "Very Very Long",
          spacer,
          label "Very Very Very Long",
          spacer,
          label "Very Very Very Very Long"
        ],
        hstack [
          sandbox RunLongTask `style` buttonStyle,
          sandbox PrintTextFields `style` buttonStyle,
          sandbox (IncreaseCount 2) `style` buttonStyle
        ],
        button "Add items" (IncreaseCount 1) `style` buttonStyle,
        textField textField3 `style` textStyle
      ] ++ extraWidgets)
    ]
  styledTree = cascadeStyle mempty widgetTree

runWidgets :: SDL.Window -> Context -> AppM ()
runWidgets window c = do
  useHiDPI <- use useHiDPI
  devicePixelRate <- use devicePixelRate
  Rect rx ry rw rh <- use windowSize

  let dpr = if useHiDPI then devicePixelRate else 1
  let renderer = NV.makeRenderer c dpr
  let newWindowSize = Rect rx ry (rw / dpr) (rh / dpr)

  windowSize .= newWindowSize
  ticks <- SDL.ticks
  newUI <- doInDrawingContext window c $ updateUI renderer empty

  mainLoop window c renderer (fromIntegral ticks) 0 0 newUI

getWindowSize :: (MonadIO m) => SDL.Window -> m Rect
getWindowSize window = do
  SDL.V2 fbWidth fbHeight <- SDL.glGetDrawableSize window
  return (Rect 0 0 (fromIntegral fbWidth) (fromIntegral fbHeight))

updateUI :: Renderer AppM -> WidgetTree -> AppM WidgetTree
updateUI renderer oldWidgets = do
  windowSize <- use windowSize
  oldFocus <- getCurrentFocus
  app <- use appContext

  resizedUI <- resizeUI renderer app windowSize (mergeTrees app (buildUI app) oldWidgets)

  let paths = map snd $ filter (isFocusable . fst) $ collectPaths resizedUI []
  focusRing .= rotateUntil oldFocus paths
  currentFocus <- getCurrentFocus

  return (setFocusedStatus currentFocus True resizedUI)

mainLoop :: SDL.Window -> Context -> Renderer AppM -> Int -> Int -> Int -> WidgetTree -> AppM ()
mainLoop window c renderer !prevTicks !tsAccum !frames widgets = do
  useHiDPI <- use useHiDPI
  devicePixelRate <- use devicePixelRate
  startTicks <- fmap fromIntegral SDL.ticks
  events <- SDL.pollEvents
  mousePos <- getCurrentMousePos

  let !ts = (startTicks - prevTicks)
  let eventsPayload = fmap SDL.eventPayload events
  let quit = elem SDL.QuitEvent eventsPayload
  let resized = not $ null [ e | e@SDL.WindowResizedEvent {} <- eventsPayload ]
  let mousePixelRate = if not useHiDPI then devicePixelRate else 1
  let baseSystemEvents = convertEvents mousePixelRate mousePos eventsPayload
  let newSecond = tsAccum + ts > 1000
  let newTsAccum = if newSecond then 0 else tsAccum + ts
  let newFrameCount = if newSecond then 0 else frames + 1

  when newSecond $
    liftIO . putStrLn $ "Frames: " ++ (show frames)

  -- Pre process events (change focus, add Enter/Leave events when Move is received, etc)
  pendingEvents <- checkUserTasks
  systemEvents <- preProcessEvents widgets baseSystemEvents
  oldApp <- use appContext

  newWidgets <- handleAppEvents pendingEvents
    >>  handleSystemEvents renderer oldApp systemEvents widgets
    >>= rebuildIfNecessary renderer oldApp
    >>= processWidgetTasks renderer
    >>= bindIf resized (handleWindowResize window renderer)

  newApp <- use appContext

  renderWidgets window c renderer newApp newWidgets startTicks

  endTicks <- fmap fromIntegral SDL.ticks

  let fps = 30
  let frameLength = 0.9 * 1000000 / fps
  let newTs = fromIntegral $ (endTicks - startTicks)
  let nextFrameDelay = round . abs $ (frameLength - newTs * 1000)

  liftIO $ threadDelay nextFrameDelay
  unless quit (mainLoop window c renderer startTicks newTsAccum newFrameCount newWidgets)

rebuildIfNecessary :: Renderer AppM -> App -> WidgetTree -> AppM WidgetTree
rebuildIfNecessary renderer oldApp widgets = do
  newApp <- use appContext

  if oldApp /= newApp
    then updateUI renderer widgets
    else return widgets

preProcessEvents :: WidgetTree -> [SystemEvent] -> AppM [SystemEvent]
preProcessEvents widgets events = do
  systemEvents <- concatMapM (preProcessEvent widgets) events
  mapM_ updateInputStatus systemEvents
  return systemEvents

preProcessEvent :: WidgetTree -> SystemEvent -> AppM [SystemEvent]
preProcessEvent widgets evt@(Move point) = do
  hover <- use latestHover
  let current = findPathFromPoint point widgets
  let hoverChanged = isJust hover && current /= fromJust hover
  let enter = if isNothing hover || hoverChanged then [Enter point] else []
  let leave = if hoverChanged then [Leave (fromJust hover) point] else []

  when (isNothing hover || hoverChanged) $
    latestHover .= Just current

  return $ leave ++ enter ++ [evt]
preProcessEvent widgets event = return [event]

updateInputStatus :: SystemEvent -> AppM ()
updateInputStatus (Click _ btn btnState) = inputStatus %= \ist -> ist {
    statusButtons = M.insert btn btnState (statusButtons ist)
  }
updateInputStatus (KeyAction kMod kCode kStatus) = inputStatus %= \ist -> ist {
    statusKeyMod = kMod,
    statusKeys = M.insert kCode kStatus (statusKeys ist)
  }
updateInputStatus _ = return ()

getCurrentMousePos :: AppM Point
getCurrentMousePos = do
  SDL.P (SDL.V2 x y) <- Mouse.getAbsoluteMouseLocation
  return $ Point (fromIntegral x) (fromIntegral y)

getCurrentFocus :: (MonomerM s e m) => m Path
getCurrentFocus = do
  ring <- use focusRing
  return (if length ring > 0 then ring!!0 else [])

handleEvent :: Renderer AppM -> App -> SystemEvent -> Path -> WidgetTree -> ChildEventResult App AppEvent AppM
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

handleSystemEvents :: Renderer AppM -> App -> [SystemEvent] -> WidgetTree -> AppM WidgetTree
handleSystemEvents renderer app systemEvents widgets = do
  foldM (\newWidgets event -> do
    focus <- getCurrentFocus
    handleSystemEvent renderer app event focus newWidgets) widgets systemEvents

handleSystemEvent :: Renderer AppM -> App -> SystemEvent -> Path -> WidgetTree -> AppM WidgetTree
handleSystemEvent renderer app systemEvent currentFocus widgets = do
  let (ChildEventResult stopProcessing eventRequests appEvents newWidgets newStates) = handleEvent renderer app systemEvent currentFocus widgets
  let newRoot = fromMaybe widgets newWidgets

  appContext %= compose newStates
  launchWidgetTasks renderer eventRequests

  handleAppEvents appEvents
    >>  handleFocusChange renderer currentFocus systemEvent stopProcessing newRoot
    >>= handleClipboardGet renderer eventRequests
    >>= handleClipboardSet renderer eventRequests
    >>= handleResizeChildren renderer eventRequests

handleFocusChange :: Renderer AppM -> Path -> SystemEvent -> Bool -> WidgetTree -> AppM WidgetTree
handleFocusChange renderer currentFocus systemEvent stopProcessing widgetRoot
  | focusChangeRequested = do
      ring <- use focusRing
      app <- use appContext
      oldFocus <- getCurrentFocus
      newRoot1 <- handleSystemEvent renderer app Blur oldFocus widgetRoot
      focusRing .= rotate ring
      newApp <- use appContext
      newFocus <- getCurrentFocus
      newRoot2 <- handleSystemEvent renderer newApp Focus newFocus newRoot1
      return $ setFocusedStatus newFocus True (setFocusedStatus currentFocus False newRoot2)
  | otherwise = return widgetRoot
  where
    focusChangeRequested = not stopProcessing && isKeyPressed systemEvent keyTab
    rotate = if isShiftPressed systemEvent then inverseRotateList else rotateList

handleResizeChildren :: Renderer AppM -> [(Path, EventRequest)] -> WidgetTree -> AppM WidgetTree
handleResizeChildren renderer eventRequests widgetRoot =
  case L.find (\(path, evt) -> isResizeChildren evt) eventRequests of
    Just (path, event) -> updateUI renderer widgetRoot
    Nothing -> return widgetRoot

handleClipboardGet :: Renderer AppM -> [(Path, EventRequest)] -> WidgetTree -> AppM WidgetTree
handleClipboardGet renderer eventRequests widgetRoot =
  case L.find (\(path, evt) -> isGetClipboard evt) eventRequests of
    Just (path, event) -> do
      app <- use appContext
      hasText <- SDL.hasClipboardText
      contents <- if hasText then fmap ClipboardText SDL.getClipboardText else return ClipboardEmpty

      handleSystemEvent renderer app (Clipboard contents) path widgetRoot
    Nothing -> return widgetRoot

handleClipboardSet :: Renderer AppM -> [(Path, EventRequest)] -> WidgetTree -> AppM WidgetTree
handleClipboardSet renderer eventRequests widgetRoot =
  case L.find (\(path, evt) -> isSetClipboard evt) eventRequests of
    Just (path, SetClipboard (ClipboardText text)) -> do
      SDL.setClipboardText text

      return widgetRoot
    Just _ -> return widgetRoot
    Nothing -> return widgetRoot

handleWindowResize :: SDL.Window -> Renderer AppM -> WidgetTree -> AppM WidgetTree
handleWindowResize window renderer widgets = do
  app <- use appContext
  dpr <- use devicePixelRate
  Rect rx ry rw rh <- getWindowSize window

  let newWindowSize = Rect rx ry (rw / dpr) (rh / dpr)

  windowSize .= newWindowSize

  liftIO $ GL.viewport GL.$= (GL.Position 0 0, GL.Size (round rw) (round rh))

  resizeUI renderer app newWindowSize widgets

launchWidgetTasks :: (MonomerM s e m) => Renderer m -> [(Path, EventRequest)] -> m ()
launchWidgetTasks renderer eventRequests = do
  let customHandlers = L.filter isCustomHandler eventRequests

  tasks <- forM customHandlers $ \(path, RunCustom handler) -> do
    asyncTask <- liftIO $ async (liftIO handler)

    return $ WidgetTask path asyncTask

  previousTasks <- use widgetTasks
  widgetTasks .= previousTasks ++ tasks

isCustomHandler :: (Path, EventRequest) -> Bool
isCustomHandler (_, RunCustom _) = True
isCustomHandler _ = False

isUpdateUserState :: (Path, EventRequest) -> Bool
isUpdateUserState (_, UpdateUserState) = True
isUpdateUserState _ = False

processWidgetTasks :: Renderer AppM -> WidgetTree -> AppM WidgetTree
processWidgetTasks renderer widgets = do
  tasks <- use widgetTasks
  (active, finished) <- partitionM (\(WidgetTask _ task) -> fmap isNothing (liftIO $ poll task)) tasks
  widgetTasks .= active

  processCustomHandlers renderer widgets finished

processCustomHandlers :: Renderer AppM -> WidgetTree -> [WidgetTask] -> AppM WidgetTree
processCustomHandlers renderer widgets tasks = do
  newWidgets <- foldM (stepWidgetTask renderer) widgets tasks
  return newWidgets

stepWidgetTask :: Renderer AppM -> WidgetTree -> WidgetTask -> AppM WidgetTree
stepWidgetTask renderer widgets (WidgetTask path task) = do
  app <- use appContext
  taskStatus <- liftIO $ poll task

  if (isJust taskStatus)
    then processCustomHandler renderer app widgets path (fromJust taskStatus)
    else return widgets

processCustomHandler :: (Typeable a) => Renderer AppM -> App -> WidgetTree -> Path -> Either SomeException a -> AppM WidgetTree
processCustomHandler renderer app widgets _ (Left _) = return widgets
processCustomHandler renderer app widgets path (Right val) = do
  let (ChildEventResult stopProcessing eventRequests appEvents newWidgets newStates) = handleCustomCommand app path widgets val
  let newRoot = fromMaybe widgets newWidgets

  appContext %= compose newStates
  launchWidgetTasks renderer eventRequests

  handleAppEvents appEvents
    >> handleResizeChildren renderer eventRequests newRoot

handleAppEvents :: (MonomerM s e m) => [e] -> m ()
handleAppEvents events = do
  appEventHandler <- use appEventHandler
  app <- use appContext
  let (newApp, tasks) = reduceAppEvents appEventHandler app events

  appContext .= newApp
  launchUserTasks tasks

reduceAppEvents :: AppEventHandler s e -> s -> [e] -> (s, [IO (Maybe e)])
reduceAppEvents appEventHandler app events = foldl reducer (app, []) events where
  reducer (app, tasks) event = case appEventHandler app event of
    State newApp -> (newApp, tasks)
    StateEvent newApp newEvent -> reducer (newApp, tasks) newEvent
    Task newApp task -> (newApp, task : tasks)

renderWidgets :: (MonomerM s e m) => SDL.Window -> Context -> Renderer m -> s -> WidgetNode s e m -> Int -> m ()
renderWidgets !window !c !renderer app widgets ticks =
  doInDrawingContext window c $ do
    handleRender renderer app [0] widgets ticks

doInDrawingContext :: (MonadIO m) => SDL.Window -> Context -> m s -> m s
doInDrawingContext window c action = do
  SDL.V2 fbWidth fbHeight <- SDL.glGetDrawableSize window
  let !pxRatio = fromIntegral fbWidth / fromIntegral fbHeight

  liftIO $ GL.clear [GL.ColorBuffer]
  liftIO $ beginFrame c fbWidth fbHeight pxRatio

  ret <- action

  liftIO $ endFrame c
  SDL.glSwapWindow window
  return ret

collectPaths :: (Monad m) => Tree (WidgetInstance s e m) -> Path -> [(WidgetInstance s e m, Path)]
collectPaths treeNode path = fmap (\(node, path) -> (node, reverse path)) (collectReversedPaths treeNode path)

collectReversedPaths :: (Monad m) => Tree (WidgetInstance s e m) -> Path -> [(WidgetInstance s e m, Path)]
collectReversedPaths (Node widgetNode children) path = (widgetNode, path) : remainingItems where
  pairs = zip (seqToNodeList children) (map (: path) [0..])
  remainingItems = concatMap (\(wn, path) -> collectReversedPaths wn path) pairs
