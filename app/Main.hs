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
import GUI.Common.Core
import GUI.Common.Event
import GUI.Common.Keyboard
import GUI.Common.Style
import GUI.Common.Types
import GUI.Common.Util
import GUI.Data.Tree
import GUI.Widgets

import qualified GUI.Platform.NanoVGRenderer as NV

foreign import ccall unsafe "initGlew" glewInit :: IO CInt

type GWidgetMonad s e m = (MonadState (GUIContext s e) m, MonadIO m)

launchUserTasks :: GWidgetMonad a e m => [AsyncHandler e] -> m ()
launchUserTasks handlers = do
  tasks <- forM handlers $ \(AsyncHandler handler) -> do
    asyncTask <- liftIO $ async (liftIO handler)

    return $ UserTask asyncTask

  previousTasks <- use userTasks
  userTasks .= previousTasks ++ tasks

checkUserTasks :: GWidgetMonad a e m => m [e]
checkUserTasks = do
  tasks <- use userTasks
  (active, finished) <- partitionM (\(UserTask task) -> fmap isNothing (liftIO $ poll task)) tasks
  userTasks .= active

  processUserTaskHandlers finished

processUserTaskHandlers :: GWidgetMonad a e m => [UserTask e] -> m [e]
processUserTaskHandlers tasks = do
  results <- forM tasks stepUserTask
  return $ catMaybes results

stepUserTask :: GWidgetMonad a e m => UserTask e -> m (Maybe e)
stepUserTask (UserTask task) = do
  taskStatus <- liftIO $ poll task

  return $ maybe Nothing processUserTaskHandler taskStatus

processUserTaskHandler :: Either SomeException e -> Maybe e
processUserTaskHandler (Left _) = Nothing
processUserTaskHandler (Right evt) = Just evt

data AppEvent = Action1 Int | Action2 | UpdateText3 T.Text deriving (Show, Eq)

type WidgetM = StateT App IO
type LocalWidget = Widget App AppEvent WidgetM
type WidgetTree = Tree (WidgetInstance App AppEvent WidgetM)

type AppContext = GUIContext App AppEvent
type AppM = StateT AppContext IO

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

  runStateT (runWidgets window c) (initGUIContext def winSize useHiDPI devicePixelRate)

  putStrLn "About to destroyWindow"
  SDL.destroyWindow window
  SDL.quit


handleAppEvent :: App -> AppEvent -> WidgetM [AsyncHandler AppEvent]
handleAppEvent app evt = do
  case evt of
    Action1 2 -> return [AsyncHandler $ do
      threadDelay $ 1 * 1000 * 1000

      return $ UpdateText3 "HOLA"
      ]
    otherwise -> handleSyncAppEvent app evt

handleSyncAppEvent :: App -> AppEvent -> WidgetM [AsyncHandler AppEvent]
handleSyncAppEvent app evt = do
  liftIO . putStrLn $ "Calledddd"
  case evt of
    Action1 0 -> do
      txt1 <- use textField1
      txt2 <- use textField2
      txt3 <- use textField3
      liftIO . putStrLn $ "Current text 1 is: " ++ (show txt1)
      liftIO . putStrLn $ "Current text 2 is: " ++ (show txt2)
      liftIO . putStrLn $ "Current text 3 is: " ++ (show txt3)
    Action1 v -> do
      clickCount += 1
      count <- use clickCount
      liftIO . putStrLn $ "Clicked button: " ++ (show v) ++ " - Count is: " ++ (show count)
    Action2   -> liftIO . putStrLn $ "I don't know what's this"
    UpdateText3 txt -> do
      textField3 .= txt

  return []

buildUI :: App -> WidgetTree
buildUI model = styledTree where
  border1 = border 5 (rgb 0 255 0) 20
  border2 = borderLeft 20 (rgb 200 200 0) <> borderRight 20 (rgb 200 0 200)
  buttonStyle = bgColor (rgb 0 0 255) <> textSize 64 <> border1 <> border2 <> bgRadius 20
  labelStyle = bgColor (rgb 100 100 100) <> textSize 48
  textStyle = textColor (rgb 0 255 0) <> textAlignH ACenter
  extraWidgets = map (\i -> sandbox (Action1 (10 + i))) [1..(_clickCount model)]
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
          sandbox (Action1 0) `style` buttonStyle,
          sandbox (Action1 1) `style` buttonStyle,
          sandbox (Action1 2) `style` buttonStyle
        ],
        button "Add items" (Action1 0) `style` buttonStyle,
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

  mainLoop window c renderer (fromIntegral ticks) newUI

getWindowSize :: (MonadIO m) => SDL.Window -> m Rect
getWindowSize window = do
  SDL.V2 fbWidth fbHeight <- SDL.glGetDrawableSize window
  return (Rect 0 0 (fromIntegral fbWidth) (fromIntegral fbHeight))

updateUI :: Renderer WidgetM -> WidgetTree -> AppM WidgetTree
updateUI renderer oldWidgets = do
  windowSize <- use windowSize
  oldFocus <- getCurrentFocus

  resizedUI <- zoom appContext $ do
    app <- get
    resizeUI renderer windowSize (mergeTrees app (buildUI app) oldWidgets)

  let paths = map snd $ filter (isFocusable . fst) $ collectPaths resizedUI []
  focusRing .= rotateUntil oldFocus paths
  currentFocus <- getCurrentFocus

  return (setFocusedStatus currentFocus True resizedUI)

mainLoop :: SDL.Window -> Context -> Renderer WidgetM -> Int -> WidgetTree -> AppM ()
mainLoop window c renderer prevTicks widgets = do
  useHiDPI <- use useHiDPI
  devicePixelRate <- use devicePixelRate
  ticks <- fmap fromIntegral SDL.ticks
  events <- SDL.pollEvents
  mousePos <- getCurrentMousePos

  let frameLength = 1000 `div` 30
  let nextFrame = \t -> if t >= frameLength then 0 else frameLength - t
  let !ts = (ticks - prevTicks)
  let eventsPayload = fmap SDL.eventPayload events
  let quit = elem SDL.QuitEvent eventsPayload
  let resized = not $ null [ e | e@SDL.WindowResizedEvent {} <- eventsPayload ]
  let mousePixelRate = if not useHiDPI then devicePixelRate else 1
  let baseSystemEvents = convertEvents mousePixelRate mousePos eventsPayload

  -- Pre process events (change focus, add Enter/Leave events when Move is received, etc)
  pendingEvents <- checkUserTasks
  systemEvents <- preProcessEvents widgets baseSystemEvents
  oldApp <- use appContext

  newWidgets <- handleAppEvents renderer (SQ.fromList pendingEvents)
    >>  handleSystemEvents renderer systemEvents widgets
    >>= rebuildIfNecessary renderer oldApp
    >>= processWidgetTasks renderer
    >>= bindIf resized (handleWindowResize window renderer)

  renderWidgets window c renderer newWidgets ticks

  liftIO $ threadDelay $ (nextFrame ts) * 1000
  unless quit (mainLoop window c renderer ticks newWidgets)

rebuildIfNecessary :: Renderer WidgetM -> App -> WidgetTree -> AppM WidgetTree
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

getCurrentFocus :: AppM Path
getCurrentFocus = do
  ring <- use focusRing
  return (if length ring > 0 then ring!!0 else [])

handleEvent :: Renderer WidgetM -> SystemEvent -> Path -> WidgetTree -> ChildEventResult App AppEvent WidgetM
handleEvent renderer systemEvent targetPath widgets = case systemEvent of
  -- Keyboard
  KeyAction _ _ _       -> handleEventFromPath targetPath widgets systemEvent
  TextInput _           -> handleEventFromPath targetPath widgets systemEvent
  -- Clipboard
  Clipboard _           -> handleEventFromPath targetPath widgets systemEvent
  -- Mouse/touch
  Click point _ _       -> handleEventFromPoint point widgets systemEvent
  WheelScroll point _ _ -> handleEventFromPoint point widgets systemEvent
  Focus                 -> handleEventFromPath targetPath widgets systemEvent
  Blur                  -> handleEventFromPath targetPath widgets systemEvent
  Enter point           -> handleEventFromPoint point widgets systemEvent
  Move point            -> handleEventFromPoint point widgets systemEvent
  Leave oldPath _       -> handleEventFromPath oldPath widgets systemEvent

handleSystemEvents :: Renderer WidgetM -> [SystemEvent] -> WidgetTree -> AppM WidgetTree
handleSystemEvents renderer systemEvents widgets = do
  foldM (\newWidgets event -> do
    focus <- getCurrentFocus
    handleSystemEvent renderer event focus newWidgets) widgets systemEvents

handleSystemEvent :: Renderer WidgetM -> SystemEvent -> Path -> WidgetTree -> AppM WidgetTree
handleSystemEvent renderer systemEvent currentFocus widgets = do
  let (ChildEventResult stopProcessing eventRequests appEvents newWidgets) = handleEvent renderer systemEvent currentFocus widgets
  let newRoot = fromMaybe widgets newWidgets

  handleWidgetUserStateUpdate newRoot eventRequests
  launchWidgetTasks renderer eventRequests

  handleAppEvents renderer appEvents
    >>  handleFocusChange renderer currentFocus systemEvent stopProcessing newRoot
    >>= handleClipboardGet renderer eventRequests
    >>= handleClipboardSet renderer eventRequests
    >>= handleResizeChildren renderer eventRequests

handleFocusChange :: Renderer WidgetM -> Path -> SystemEvent -> Bool -> WidgetTree -> AppM WidgetTree
handleFocusChange renderer currentFocus systemEvent stopProcessing widgetRoot
  | focusChangeRequested = do
      ring <- use focusRing
      oldFocus <- getCurrentFocus
      newRoot1 <- handleSystemEvent renderer Blur oldFocus widgetRoot
      focusRing .= rotate ring
      newFocus <- getCurrentFocus
      newRoot2 <- handleSystemEvent renderer Focus newFocus newRoot1
      return $ setFocusedStatus newFocus True (setFocusedStatus currentFocus False newRoot2)
  | otherwise = return widgetRoot
  where
    focusChangeRequested = not stopProcessing && isKeyPressed systemEvent keyTab
    rotate = if isShiftPressed systemEvent then inverseRotateList else rotateList

handleResizeChildren :: Renderer WidgetM -> [(Path, EventRequest)] -> WidgetTree -> AppM WidgetTree
handleResizeChildren renderer eventRequests widgetRoot =
  case L.find (\(path, evt) -> isResizeChildren evt) eventRequests of
    Just (path, event) -> updateUI renderer widgetRoot
    Nothing -> return widgetRoot

handleClipboardGet :: Renderer WidgetM -> [(Path, EventRequest)] -> WidgetTree -> AppM WidgetTree
handleClipboardGet renderer eventRequests widgetRoot =
  case L.find (\(path, evt) -> isGetClipboard evt) eventRequests of
    Just (path, event) -> do
      hasText <- SDL.hasClipboardText
      contents <- if hasText then fmap ClipboardText SDL.getClipboardText else return ClipboardEmpty

      handleSystemEvent renderer (Clipboard contents) path widgetRoot
    Nothing -> return widgetRoot

handleClipboardSet :: Renderer WidgetM -> [(Path, EventRequest)] -> WidgetTree -> AppM WidgetTree
handleClipboardSet renderer eventRequests widgetRoot =
  case L.find (\(path, evt) -> isSetClipboard evt) eventRequests of
    Just (path, SetClipboard (ClipboardText text)) -> do
      SDL.setClipboardText text

      return widgetRoot
    Just _ -> return widgetRoot
    Nothing -> return widgetRoot

handleWindowResize :: SDL.Window -> Renderer WidgetM -> WidgetTree -> AppM WidgetTree
handleWindowResize window renderer widgets = do
  ctx <- get
  dpr <- use devicePixelRate
  Rect rx ry rw rh <- getWindowSize window

  let newWindowSize = Rect rx ry (rw / dpr) (rh / dpr)

  windowSize .= newWindowSize

  liftIO $ GL.viewport GL.$= (GL.Position 0 0, GL.Size (round rw) (round rh))

  zoom appContext $ do
    resizeUI renderer newWindowSize widgets

handleWidgetUserStateUpdate :: WidgetTree -> [(Path, EventRequest)] -> AppM ()
handleWidgetUserStateUpdate widgets eventRequests = do
  let runStateHandlers = L.filter isUpdateUserState eventRequests

  zoom appContext $ do
    forM_ runStateHandlers $ \(path, _) -> do
      handleUserUpdateState (reverse path) widgets

launchWidgetTasks :: Renderer WidgetM -> [(Path, EventRequest)] -> AppM ()
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

processWidgetTasks :: Renderer WidgetM -> WidgetTree -> AppM WidgetTree
processWidgetTasks renderer widgets = do
  tasks <- use widgetTasks
  (active, finished) <- partitionM (\(WidgetTask _ task) -> fmap isNothing (liftIO $ poll task)) tasks
  widgetTasks .= active

  processCustomHandlers renderer widgets finished

processCustomHandlers :: Renderer WidgetM -> WidgetTree -> [WidgetTask] -> AppM WidgetTree
processCustomHandlers renderer widgets tasks = do
  newWidgets <- foldM (stepWidgetTask renderer) widgets tasks
  return newWidgets

stepWidgetTask :: Renderer WidgetM -> WidgetTree -> WidgetTask -> AppM WidgetTree
stepWidgetTask renderer widgets (WidgetTask path task) = do
  taskStatus <- liftIO $ poll task

  if (isJust taskStatus)
    then processCustomHandler renderer widgets path (fromJust taskStatus)
    else return widgets

processCustomHandler :: (Typeable a) => Renderer WidgetM -> WidgetTree -> Path -> Either SomeException a -> AppM WidgetTree
processCustomHandler renderer widgets _ (Left _) = return widgets
processCustomHandler renderer widgets path (Right val) = do
  let (ChildEventResult stopProcessing eventRequests appEvents newWidgets) = handleCustomCommand path widgets val
  let newRoot = fromMaybe widgets newWidgets

  launchWidgetTasks renderer eventRequests

  handleAppEvents renderer appEvents
    >> handleResizeChildren renderer eventRequests newRoot

handleAppEvents :: Renderer WidgetM -> SQ.Seq AppEvent -> AppM ()
handleAppEvents renderer appEvents = do
  tasks <- zoom appContext $ do
    tasks <- forM appEvents $ \event -> do
      currApp <- get
      handleAppEvent currApp event
    return $ msum tasks

  launchUserTasks $ tasks

renderWidgets :: SDL.Window -> Context -> Renderer WidgetM -> WidgetTree -> Int -> AppM ()
renderWidgets !window !c !renderer widgets ticks =
  doInDrawingContext window c $ do
    guiContext <- get
    zoom appContext $ do
      handleRender renderer widgets ticks

doInDrawingContext :: SDL.Window -> Context -> AppM a -> AppM a
doInDrawingContext window c action = do
  SDL.V2 fbWidth fbHeight <- SDL.glGetDrawableSize window
  let !pxRatio = fromIntegral fbWidth / fromIntegral fbHeight

  liftIO $ GL.clear [GL.ColorBuffer]
  liftIO $ beginFrame c fbWidth fbHeight pxRatio

  ret <- action

  liftIO $ endFrame c
  SDL.glSwapWindow window
  return ret

collectPaths :: (MonadState s m) => Tree (WidgetInstance s e m) -> Path -> [(WidgetInstance s e m, Path)]
collectPaths treeNode path = fmap (\(node, path) -> (node, reverse path)) (collectReversedPaths treeNode path)

collectReversedPaths :: (MonadState s m) => Tree (WidgetInstance s e m) -> Path -> [(WidgetInstance s e m, Path)]
collectReversedPaths (Node widgetNode children) path = (widgetNode, path) : remainingItems where
  pairs = zip (seqToNodeList children) (map (: path) [0..])
  remainingItems = concatMap (\(wn, path) -> collectReversedPaths wn path) pairs
