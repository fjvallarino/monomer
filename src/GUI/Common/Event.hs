{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

module GUI.Common.Event where

import Control.Monad.State
  
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Data.Typeable (cast, Typeable)
import Unsafe.Coerce

import GUI.Common.Keyboard
import GUI.Common.Types

import GUI.Data.Tree

import qualified SDL

data Button = LeftBtn | MiddleBtn | RightBtn deriving (Show, Eq, Ord)
data ButtonState = PressedBtn | ReleasedBtn deriving (Show, Eq)
data WheelDirection = WheelNormal | WheelFlipped deriving (Show, Eq)

data KeyStatus = KeyPressed | KeyReleased deriving (Show, Eq)

data EventRequest s m = (MonadState s m) =>
                    IgnoreParentEvents
                  | IgnoreChildrenEvents
                  | ResizeChildren
                  | ResizeAll
                  | GetClipboard
                  | SetClipboard ClipboardData
                  | RunState (m ())
                  | forall a . Typeable a => RunCustom (IO a)

data SystemEvent = Click Point Button ButtonState
                 | WheelScroll Point Point WheelDirection
                 | KeyAction !KeyMod !KeyCode !KeyStatus
                 | TextInput T.Text
                 | Clipboard ClipboardData
                 | Focus
                 | Blur
                 | Enter Point
                 | Move Point
                 | Leave Path Point
                 deriving (Show, Eq)

data InputStatus = InputStatus {
  statusKeyMod :: KeyMod,
  statusKeys :: M.Map KeyCode KeyStatus,
  statusButtons :: M.Map Button ButtonState
} deriving (Eq, Show)

defInputStatus = InputStatus {
  statusKeyMod = defKeyMod,
  statusKeys = M.empty,
  statusButtons = M.empty
}

isIgnoreParentEvents :: (MonadState s m) => EventRequest s m -> Bool
isIgnoreParentEvents IgnoreParentEvents = True
isIgnoreParentEvents _ = False

isIgnoreChildrenEvents :: (MonadState s m) => EventRequest s m -> Bool
isIgnoreChildrenEvents IgnoreChildrenEvents = True
isIgnoreChildrenEvents _ = False

isResizeChildren :: (MonadState s m) => EventRequest s m -> Bool
isResizeChildren ResizeChildren = True
isResizeChildren _ = False

isResizeAll :: (MonadState s m) => EventRequest s m -> Bool
isResizeAll ResizeAll = True
isResizeAll _ = False

isGetClipboard :: (MonadState s m) => EventRequest s m -> Bool
isGetClipboard GetClipboard = True
isGetClipboard _ = False

isSetClipboard :: (MonadState s m) => EventRequest s m -> Bool
isSetClipboard (SetClipboard _) = True
isSetClipboard _ = False

convertEvents :: Double -> Point -> [SDL.EventPayload] -> [SystemEvent]
convertEvents devicePixelRate mousePos events = newEvents
  where
    newEvents = mouseEvents ++ mouseMoveEvents ++ mouseWheelEvents ++ keyboardEvents ++ textEvents
    mouseEvents = mouseClick devicePixelRate events
    mouseMoveEvents = mouseMoveEvent devicePixelRate mousePos events
    mouseWheelEvents = mouseWheelEvent devicePixelRate mousePos events
    keyboardEvents = keyboardEvent events
    textEvents = textEvent events

mouseClick :: Double -> [SDL.EventPayload] -> [SystemEvent]
mouseClick devicePixelRate events =
  case clickEvent of
    Just (SDL.MouseButtonEvent SDL.MouseButtonEventData
          { SDL.mouseButtonEventMotion = motion,
            SDL.mouseButtonEventButton = button,
            SDL.mouseButtonEventPos = SDL.P (SDL.V2 x y) }) -> leftClicked ++ leftReleased ++ rightClicked ++ rightReleased
      where isLeft = button == SDL.ButtonLeft
            isRight = button == SDL.ButtonRight
            isClicked = motion == SDL.Pressed
            isReleased = motion == SDL.Released
            mousePos = Point (fromIntegral x * devicePixelRate) (fromIntegral y * devicePixelRate)
            leftClicked = if isLeft && isClicked then [Click mousePos LeftBtn PressedBtn] else []
            leftReleased = if isLeft && isReleased then [Click mousePos LeftBtn ReleasedBtn] else []
            rightClicked = if isRight && isClicked then [Click mousePos RightBtn PressedBtn] else []
            rightReleased = if isRight && isReleased then [Click mousePos RightBtn ReleasedBtn] else []

    otherwise -> []
  where clickEvent = L.find (\evt -> case evt of
                                     SDL.MouseButtonEvent _ -> True
                                     otherwise -> False
                          ) events

mouseMoveEvent :: Double -> Point -> [SDL.EventPayload] -> [SystemEvent]
mouseMoveEvent devicePixelRate mousePos events =
  case moveEvent of
    Just (SDL.MouseMotionEvent _) -> [Move mousePos]
    otherwise -> []
  where moveEvent = L.find (\evt -> case evt of
                                     SDL.MouseMotionEvent _ -> True
                                     otherwise -> False
                          ) events

mouseWheelEvent :: Double -> Point -> [SDL.EventPayload] -> [SystemEvent]
mouseWheelEvent devicePixelRate mousePos events =
  case touchEvent of
    Just (SDL.MouseWheelEvent SDL.MouseWheelEventData
          { SDL.mouseWheelEventPos = (SDL.V2 x y),
            SDL.mouseWheelEventDirection = direction,
            SDL.mouseWheelEventWhich = which }) -> if which == SDL.Touch then [] else [WheelScroll mousePos wheelDelta wheelDirection]
      where wheelDirection = if direction == SDL.ScrollNormal then WheelNormal else WheelFlipped
            wheelDelta = Point (fromIntegral x * devicePixelRate) (fromIntegral y * devicePixelRate)
    otherwise -> []
  where touchEvent = L.find (\evt -> case evt of
                                     SDL.MouseWheelEvent _ -> True
                                     otherwise -> False
                          ) events

keyboardEvent :: [SDL.EventPayload] -> [SystemEvent]
keyboardEvent events = activeKeys
  where
    activeKeys = map (\(SDL.KeyboardEvent k) -> KeyAction (keyMod k) (keyCode k) (keyStatus k)) (unsafeCoerce keyboardEvents)
    keyMod event = convertKeyModifier $ SDL.keysymModifier $ SDL.keyboardEventKeysym event
    keyCode event = fromIntegral $ SDL.unwrapKeycode $ SDL.keysymKeycode $ SDL.keyboardEventKeysym event
    keyStatus event = if SDL.keyboardEventKeyMotion event == SDL.Pressed then KeyPressed else KeyReleased
    keyboardEvents = filter (\e -> case e of
                                      SDL.KeyboardEvent k -> True
                                      _ -> False) events

textEvent :: [SDL.EventPayload] -> [SystemEvent]
textEvent events = inputText
  where
    inputText = map (\(SDL.TextInputEvent t) -> TextInput (SDL.textInputEventText t)) (unsafeCoerce inputTextEvents)
    inputTextEvents = filter (\e -> case e of
                                      SDL.TextInputEvent _ -> True
                                      _ -> False) events

checkKeyboard :: SystemEvent -> (KeyMod -> KeyCode -> KeyStatus -> Bool) -> Bool
checkKeyboard (KeyAction mod code motion) testFn = testFn mod code motion
checkKeyboard _ _ = False

isClipboardCopy :: SystemEvent -> Bool
isClipboardCopy event = checkKeyboard event (\mod code motion -> (keyModLeftGUI mod || keyModLeftCtrl mod) && isKeyC code)

isClipboardPaste :: SystemEvent -> Bool
isClipboardPaste event = checkKeyboard event (\mod code motion -> (keyModLeftGUI mod || keyModLeftCtrl mod) && isKeyV code)
