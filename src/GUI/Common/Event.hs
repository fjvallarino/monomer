{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

module GUI.Common.Event where

import qualified Data.List as L
import qualified Data.Text as T

import Data.Typeable (cast, Typeable)
import Unsafe.Coerce

import GUI.Common.Keyboard
import GUI.Common.Types

import qualified SDL

data Button = LeftBtn | RightBtn deriving (Show, Eq)
data ButtonState = PressedBtn | ReleasedBtn deriving (Show, Eq)
data WheelDirection = WheelNormal | WheelFlipped deriving (Show, Eq)

data KeyMotion = KeyPressed | KeyReleased deriving (Show, Eq)

data EventRequest = IgnoreParentEvents
                  | IgnoreChildrenEvents
                  | ResizeChildren
                  | ResizeAll
                  | GetClipboard
                  | SetClipboard ClipboardData
                  | forall a . Typeable a => RunCustom (IO a)

data SystemEvent = Click Point Button ButtonState
                 | WheelScroll Point Point WheelDirection
                 | KeyAction !KeyMod !KeyCode !KeyMotion
                 | TextInput T.Text
                 | Clipboard ClipboardData
                 deriving (Show, Eq)

isIgnoreParentEvents :: EventRequest -> Bool
isIgnoreParentEvents IgnoreParentEvents = True
isIgnoreParentEvents _ = False

isIgnoreChildrenEvents :: EventRequest -> Bool
isIgnoreChildrenEvents IgnoreChildrenEvents = True
isIgnoreChildrenEvents _ = False

isResizeChildren :: EventRequest -> Bool
isResizeChildren ResizeChildren = True
isResizeChildren _ = False

isResizeAll :: EventRequest -> Bool
isResizeAll ResizeAll = True
isResizeAll _ = False

isGetClipboard :: EventRequest -> Bool
isGetClipboard GetClipboard = True
isGetClipboard _ = False

isSetClipboard :: EventRequest -> Bool
isSetClipboard (SetClipboard _) = True
isSetClipboard _ = False

convertEvents :: Double -> Point -> [SDL.EventPayload] -> [SystemEvent]
convertEvents devicePixelRate mousePos events = newEvents
  where
    newEvents = mouseEvents ++ mouseWheelEvents ++ keyboardEvents ++ textEvents
    mouseEvents = mouseClick devicePixelRate events
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
    activeKeys = map (\(SDL.KeyboardEvent k) -> KeyAction (keyMod k) (keyCode k) (keyMotion k)) (unsafeCoerce keyboardEvents)
    keyMod event = convertKeyModifier $ SDL.keysymModifier $ SDL.keyboardEventKeysym event
    keyCode event = fromIntegral $ SDL.unwrapKeycode $ SDL.keysymKeycode $ SDL.keyboardEventKeysym event
    keyMotion event = if SDL.keyboardEventKeyMotion event == SDL.Pressed then KeyPressed else KeyReleased
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

checkKeyboard :: SystemEvent -> (KeyMod -> KeyCode -> KeyMotion -> Bool) -> Bool
checkKeyboard (KeyAction mod code motion) testFn = testFn mod code motion
checkKeyboard _ _ = False

isClipboardCopy :: SystemEvent -> Bool
isClipboardCopy event = checkKeyboard event (\mod code motion -> (keyModLeftGUI mod || keyModLeftCtrl mod) && isKeyC code)

isClipboardPaste :: SystemEvent -> Bool
isClipboardPaste event = checkKeyboard event (\mod code motion -> (keyModLeftGUI mod || keyModLeftCtrl mod) && isKeyV code)
