{-# LANGUAGE BangPatterns #-}
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
import NanoVG
import SDL (($=))
import Unsafe.Coerce

import Debug.Trace

import System.Remote.Monitoring

import qualified Data.List as L
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

import Types
import GUI.Common.Core
import GUI.Common.Event
import GUI.Common.Style
import GUI.Widgets

import qualified GUI.Data.Tree as TR
import qualified GUI.Platform.NanoVGRenderer as NV
import qualified GUI.Widget.Core as W

foreign import ccall unsafe "initGlew" glewInit :: IO CInt

data AppEvent = Action1 Int | Action2 deriving (Show, Eq)

type WidgetM = StateT App IO
type LocalWidget = W.Widget App AppEvent WidgetM
type WidgetTree = TR.Tree (W.WidgetInstance App AppEvent WidgetM)

type AppContext = W.GUIContext App
type AppM = StateT AppContext IO

(screenWidth, screenHeight) = (640, 480)
windowSize = (Rect 0 0 (fromIntegral screenWidth) (fromIntegral screenHeight))

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

  window <-
    SDL.createWindow
      "SDL / OpenGL Example"
      SDL.defaultWindow {SDL.windowInitialSize = SDL.V2 screenWidth screenHeight,
                         SDL.windowOpenGL = Just customOpenGL }

  err <- SRE.getError
  err <- STR.peekCString err
  putStrLn err

  _ <- SDL.glCreateContext window

  _ <- glewInit

  c@(Context c') <- createGL3 (S.fromList [Antialias, StencilStrokes, Debug])

  fontRes <- createFont c "sans" (FileName "./assets/fonts/Roboto-Regular.ttf")

  SREv.startTextInput

  runStateT (runWidgets window c) (W.initGUIContext def)

  putStrLn "About to destroyWindow"
  SDL.destroyWindow window
  SDL.quit

handleAppEvent :: AppEvent -> WidgetM ()
handleAppEvent evt = do
  case evt of
    Action1 v -> do
      when (v == 0) $ clickCount += 1
      count <- use clickCount
      liftIO . putStrLn $ "Clicked button: " ++ (show v) ++ " - Count is: " ++ (show count)
    Action2   -> liftIO . putStrLn $ "I don't know what's this"

buildUI :: App -> WidgetTree
buildUI model = styledTree where
  border1 = border 5 (RGB 0 255 0) 20
  border2 = borderLeft 20 (RGB 200 200 0) <> borderRight 20 (RGB 200 0 200)
  buttonStyle = bgColor (RGB 0 0 255) <> textSize 64 <> border1 <> border2 <> bgRadius 20
  labelStyle = bgColor (RGB 100 100 100) <> textSize 48
  textStyle = textColor (RGB 0 255 0)
  extraWidgets = map (\i -> sandbox (Action1 (10 + i))) [1..(_clickCount model)]
  widgetTree = vgrid ([
      hgrid [
        scroll $ vstack [
          textField `style` textStyle,
          spacer,
          label "Label 1",
          spacer,
          label "Label 2",
          spacer,
          label "Label 3",
          spacer,
          label "Label 4",
          spacer,
          label "Label 5",
          spacer,
          label "Label 6",
          spacer,
          label "Label 7"
        ],
        vgrid [
          textField `style` textStyle,
          scroll $ label "This is a really really really long label, you know?" `style` labelStyle
        ]
      ],
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
      hgrid [
        sandbox (Action1 1) `style` buttonStyle,
        sandbox (Action1 2) `style` buttonStyle,
        sandbox (Action1 3) `style` buttonStyle
      ],
      button "Add items" (Action1 0) `style` buttonStyle,
      textField `style` textStyle
    ] ++ extraWidgets)
  styledTree = W.cascadeStyle mempty widgetTree

runWidgets :: SDL.Window -> Context -> AppM ()
runWidgets window c = do
  let renderer = NV.makeRenderer c

  ticks <- SDL.ticks
  newUI <- doInDrawingContext window c $ updateUI renderer empty

  mainLoop window c renderer (fromIntegral ticks) newUI

updateUI :: Renderer WidgetM -> WidgetTree -> AppM WidgetTree
updateUI renderer oldWidgets = do
  resizedUI <- zoom appContext $ do
    app <- get
    W.resizeUI renderer windowSize (W.mergeTrees (buildUI app) oldWidgets)

  let paths = map snd $ filter (W.isFocusable . fst) $ collectPaths resizedUI []
  focusRing .= paths
  currentFocus <- getCurrentFocus

  return (W.setFocusedStatus currentFocus True resizedUI)

mainLoop :: SDL.Window -> Context -> Renderer WidgetM -> Int -> WidgetTree -> AppM ()
mainLoop window c renderer prevTicks widgets = do
  ticks <- fmap fromIntegral SDL.ticks
  events <- SDL.pollEvents
  mousePos <- getCurrentMousePos

  let frameLength = 1000 `div` 30
  let nextFrame = \t -> if t >= frameLength then 0 else frameLength - t
  let !ts = (ticks - prevTicks)
  let eventsPayload = fmap SDL.eventPayload events
  let quit = elem SDL.QuitEvent eventsPayload

  handledWidgets <- processWidgetTasks renderer widgets

  focus <- getCurrentFocus
  newWidgets <- handleSystemEvents renderer (convertEvents mousePos eventsPayload) focus handledWidgets

  renderWidgets window c renderer newWidgets ticks

  liftIO $ threadDelay $ (nextFrame ts) * 1000
  unless quit (mainLoop window c renderer ticks newWidgets)

getCurrentMousePos :: AppM Point
getCurrentMousePos = do
  SDL.P (SDL.V2 x y) <- Mouse.getAbsoluteMouseLocation
  return $ Point (fromIntegral x) (fromIntegral y)

getCurrentFocus :: AppM TR.Path
getCurrentFocus = do
  ring <- use focusRing
  return (if length ring > 0 then ring!!0 else [])

handleSystemEvents :: Renderer WidgetM -> [SystemEvent] -> TR.Path -> WidgetTree -> AppM WidgetTree
handleSystemEvents renderer systemEvents currentFocus widgets =
  foldM (\newWidgets event -> handleSystemEvent renderer event currentFocus newWidgets) widgets systemEvents

handleEvent :: Renderer WidgetM -> SystemEvent -> TR.Path -> WidgetTree -> W.ChildEventResult App AppEvent WidgetM
handleEvent renderer systemEvent currentFocus widgets = case systemEvent of
  Click point _ _       -> W.handleEventFromPoint point widgets systemEvent
  WheelScroll point _ _ -> W.handleEventFromPoint point widgets systemEvent
  KeyAction _ _         -> W.handleEventFromPath currentFocus widgets systemEvent
  TextInput _           -> W.handleEventFromPath currentFocus widgets systemEvent

handleSystemEvent :: Renderer WidgetM -> SystemEvent -> TR.Path -> WidgetTree -> AppM WidgetTree
handleSystemEvent renderer systemEvent currentFocus widgets = do
  let (W.ChildEventResult stopProcessing eventRequests appEvents newWidgets) = handleEvent renderer systemEvent currentFocus widgets
  let newRoot = fromMaybe widgets newWidgets

  launchWidgetTasks renderer eventRequests

  handleFocusChange currentFocus systemEvent stopProcessing newRoot
    >>= handleAppEvents renderer appEvents
    >>= handleResizeChildren renderer eventRequests

handleFocusChange :: TR.Path -> SystemEvent -> Bool -> WidgetTree -> AppM WidgetTree
handleFocusChange currentFocus systemEvent stopProcessing widgetRoot
  | focusChangeRequested = do
      ring <- use focusRing
      focusRing .= rotateList ring
      newFocus <- getCurrentFocus
      return $ W.setFocusedStatus newFocus True (W.setFocusedStatus currentFocus False widgetRoot)
  | otherwise = return widgetRoot
  where
    focusChangeRequested = not stopProcessing && isKeyPressed systemEvent keycodeTab

handleResizeChildren :: Renderer WidgetM -> [(TR.Path, W.EventRequest)] -> WidgetTree -> AppM WidgetTree
handleResizeChildren renderer eventRequests widgetRoot =
  case L.find (\(path, evt) -> evt == W.ResizeChildren) eventRequests of
    Just (path, event) -> updateUI renderer widgetRoot
    Nothing -> return widgetRoot

launchWidgetTasks :: Renderer WidgetM -> [(TR.Path, W.EventRequest)] -> AppM ()
launchWidgetTasks renderer eventRequests = do
  let customHandlers = L.filter isCustomHandler eventRequests

  tasks <- forM customHandlers $ \(path, W.RunCustom handler) -> do
    asyncTask <- liftIO $ async (liftIO handler)

    return $ W.WidgetTask path asyncTask

  previousTasks <- use widgetTasks
  widgetTasks .= previousTasks ++ tasks

isCustomHandler :: (TR.Path, W.EventRequest) -> Bool
isCustomHandler (_, W.RunCustom _) = True
isCustomHandler _ = False

processWidgetTasks :: Renderer WidgetM -> WidgetTree -> AppM WidgetTree
processWidgetTasks renderer widgets = do
  tasks <- use widgetTasks
  (active, finished) <- partitionM (\(W.WidgetTask _ task) -> fmap isNothing (liftIO $ poll task)) tasks
  widgetTasks .= active

  processCustomHandlers renderer widgets finished

processCustomHandlers :: Renderer WidgetM -> WidgetTree -> [W.WidgetTask] -> AppM WidgetTree
processCustomHandlers renderer widgets tasks = do
  newWidgets <- foldM (stepWidgetTask renderer) widgets tasks
  return newWidgets

stepWidgetTask :: Renderer WidgetM -> WidgetTree -> W.WidgetTask -> AppM WidgetTree
stepWidgetTask renderer widgets (W.WidgetTask path task) = do
  taskStatus <- liftIO $ poll task

  if (isJust taskStatus)
    then processCustomHandler renderer widgets path (fromJust taskStatus)
    else return widgets

processCustomHandler :: (Typeable a) => Renderer WidgetM -> WidgetTree -> TR.Path -> Either SomeException a -> AppM WidgetTree
processCustomHandler renderer widgets _ (Left _) = return widgets
processCustomHandler renderer widgets path (Right val) = do
  let (W.ChildEventResult stopProcessing eventRequests appEvents newWidgets) = W.handleCustomCommand path widgets val
  let newRoot = fromMaybe widgets newWidgets

  launchWidgetTasks renderer eventRequests

  handleAppEvents renderer appEvents newRoot
    >>= handleResizeChildren renderer eventRequests

keycodeTab :: (Integral a) => a
keycodeTab = fromIntegral $ Keyboard.unwrapKeycode SDL.KeycodeTab

isKeyboardEvent :: SystemEvent -> Bool
isKeyboardEvent (KeyAction _ _) = True
isKeyboardEvent _ = False

isKeyPressed :: SystemEvent -> KeyCode -> Bool
isKeyPressed (KeyAction keyCode KeyPressed) keyCodeChecked = keyCode == keyCodeChecked
isKeyPressed _ _ = False

isKeyTab :: KeyCode -> Bool
isKeyTab key = matchesSDLKeyCode key SDL.KeycodeTab

matchesSDLKeyCode :: KeyCode -> SDL.Keycode -> Bool
matchesSDLKeyCode keyCode sdlKeyCode = keyCode == (fromIntegral $ Keyboard.unwrapKeycode sdlKeyCode)

handleAppEvents :: Renderer WidgetM -> SQ.Seq AppEvent -> WidgetTree -> AppM WidgetTree
handleAppEvents renderer appEvents oldWidgets
  | SQ.null appEvents = return oldWidgets
  | otherwise = do
    (app, newApp) <- zoom appContext $ do
      app <- get
      forM_ appEvents handleAppEvent
      newApp <- get
      return (app, newApp)

    if app /= newApp
      then updateUI renderer oldWidgets
      else return oldWidgets

renderWidgets :: SDL.Window -> Context -> Renderer WidgetM -> WidgetTree -> Int -> AppM ()
renderWidgets !window !c !renderer widgets ticks =
  doInDrawingContext window c $ do
    guiContext <- get
    zoom appContext $ do
      W.handleRender renderer widgets ticks

doInDrawingContext :: SDL.Window -> Context -> AppM a -> AppM a
doInDrawingContext window c action = do
  SDL.V2 fbWidth fbHeight <- SDL.glGetDrawableSize window
  let !pxRatio = fromIntegral fbWidth / fromIntegral fbHeight

  liftIO $ GL.clear [GL.ColorBuffer]
  liftIO $ beginFrame c screenWidth screenHeight pxRatio

  ret <- action

  liftIO $ endFrame c
  SDL.glSwapWindow window
  return ret

collectPaths :: (MonadState s m) => TR.Tree (W.WidgetInstance s e m) -> TR.Path -> [(W.WidgetInstance s e m, TR.Path)]
collectPaths (TR.Node widgetNode children) path = (widgetNode, reverse path) : remainingItems where
  pairs = zip (TR.seqToNodeList children) (map (: path) [0..])
  remainingItems = concatMap (\(wn, path) -> collectPaths wn path) pairs
