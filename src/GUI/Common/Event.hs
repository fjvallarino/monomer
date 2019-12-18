{-# LANGUAGE RecordWildCards #-}

module GUI.Common.Event where

import qualified Data.List as L

import qualified Data.Text as T
import Unsafe.Coerce

import Control.Monad (when)
import Data.Maybe

import GUI.Common.Core
import GUI.Common.Style

import qualified SDL

type KeyCode = Int

data Button = LeftBtn | RightBtn deriving (Show, Eq)
data ButtonState = PressedBtn | ReleasedBtn deriving (Show, Eq)

data KeyMotion = KeyPressed | KeyReleased deriving (Show, Eq)
data Direction = Horizontal | Vertical deriving (Show, Eq)

data WheelDirection = WheelNormal | WheelFlipped deriving (Show, Eq)

data SystemEvent = Click Point Button ButtonState
                 | WheelScroll Point Point WheelDirection
                 | KeyAction KeyCode KeyMotion
                 | TextInput T.Text
                 deriving (Show, Eq)

getKeycode :: SDL.Keycode -> Int
getKeycode keyCode = fromIntegral $ SDL.unwrapKeycode keyCode

keyBackspace = getKeycode SDL.KeycodeBackspace
keyLeft = getKeycode SDL.KeycodeLeft
keyRight = getKeycode SDL.KeycodeRight

convertEvents :: Point -> [SDL.EventPayload] -> [SystemEvent]
convertEvents mousePos events = newEvents
  where
    newEvents = mouseEvents ++ mouseWheelEvents ++ keyboardEvents ++ textEvents
    mouseEvents = mouseClick events
    mouseWheelEvents = mouseWheelEvent mousePos events
    keyboardEvents = keyboardEvent events
    textEvents = textEvent events

mouseClick :: [SDL.EventPayload] -> [SystemEvent]
mouseClick events =
  case clickEvent of
    Just (SDL.MouseButtonEvent SDL.MouseButtonEventData
          { SDL.mouseButtonEventMotion = motion,
            SDL.mouseButtonEventButton = button,
            SDL.mouseButtonEventPos = SDL.P (SDL.V2 x y) }) -> leftClicked ++ leftReleased ++ rightClicked ++ rightReleased
      where isLeft = button == SDL.ButtonLeft
            isRight = button == SDL.ButtonRight
            isClicked = motion == SDL.Pressed
            isReleased = motion == SDL.Released
            mousePos = Point (fromIntegral x) (fromIntegral y)
            leftClicked = if isLeft && isClicked then [Click mousePos LeftBtn PressedBtn] else []
            leftReleased = if isLeft && isReleased then [Click mousePos LeftBtn ReleasedBtn] else []
            rightClicked = if isRight && isClicked then [Click mousePos RightBtn PressedBtn] else []
            rightReleased = if isRight && isReleased then [Click mousePos RightBtn ReleasedBtn] else []

    otherwise -> []
  where clickEvent = L.find (\evt -> case evt of
                                     SDL.MouseButtonEvent _ -> True
                                     otherwise -> False
                          ) events

mouseWheelEvent :: Point -> [SDL.EventPayload] -> [SystemEvent]
mouseWheelEvent mousePos events =
  case touchEvent of
    Just (SDL.MouseWheelEvent SDL.MouseWheelEventData
          { SDL.mouseWheelEventPos = (SDL.V2 x y),
            SDL.mouseWheelEventDirection = direction,
            SDL.mouseWheelEventWhich = which }) -> if which == SDL.Touch then [] else [WheelScroll mousePos wheelDelta wheelDirection]
      where wheelDirection = if direction == SDL.ScrollNormal then WheelNormal else WheelFlipped
            wheelDelta = Point (fromIntegral x) (fromIntegral y)
    otherwise -> []
  where touchEvent = L.find (\evt -> case evt of
                                     SDL.MouseWheelEvent _ -> True
                                     otherwise -> False
                          ) events

keyboardEvent :: [SDL.EventPayload] -> [SystemEvent]
keyboardEvent events = activeKeys
  where
    activeKeys = map (\(SDL.KeyboardEvent k) -> KeyAction (keyCode k) (keyMotion k)) (unsafeCoerce keyboardEvents)
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
