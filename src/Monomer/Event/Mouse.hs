module Monomer.Event.Mouse where

import qualified Data.List as L

import qualified SDL

import Monomer.Common.Types
import Monomer.Event.Types

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
